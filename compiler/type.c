#include "includes/ir.h"

Type* get_type_of_last_element(IrGenContext* ir_context, ParserContext* parser_context, void* ast, const wchar_t* search_point_class_name) {
  ASTType type = *((ASTType*)ast);

  Type* result = infer_type(ir_context, parser_context, ast, search_point_class_name);

  if (type == AST_Identifier) {
    if (((IdentifierAST*)ast)->attribute != NULL) {
      result = get_type_of_last_element(ir_context, parser_context, ((IdentifierAST*)ast)->attribute, result->type_str);
    }
  }

  if (type == AST_FunctionCall) {
    if (((FunctionCallAST*)ast)->attribute != NULL) {
      result = get_type_of_last_element(ir_context, parser_context, ((FunctionCallAST*)ast)->attribute, result->type_str);
    }
  }

  if (type == AST_ArrayAccess) {
    if (((ArrayAccessAST*)ast)->attribute != NULL) {
      result = get_type_of_last_element(ir_context, parser_context, ((ArrayAccessAST*)ast)->attribute, result->type_str);
    }
  }

  return result;
}

bool is_same_type(Type* t1, Type* t2) {
  if (!wcscmp(t1->type_str, t2->type_str)) {
    if (!wcscmp(t1->type_str, L"array")) {
      return is_same_type(t1->array_element_type, t2->array_element_type);
    }

    return true;
  }
  else {
    return false;
  }
}

void check_type_of_variable_declaration(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationAST* variable_declaration_ast) {
  return;
  const wchar_t* type_str = variable_declaration_ast->variable_type;

  int result = 0;

  if (find_symbol_from_set(parser_context->primitive_types, type_str) == NULL) result = 1;
  if (find_symbol_from_set(parser_context->class_symbol_table, type_str) == NULL) result = 1;

  if (result) {
    handle_error(ER_TypeNotExist, get_token_of_ast(variable_declaration_ast), parser_context->current_file_name, parser_context->file_str);
  }
}

bool check_castability(ParserContext* parser_context, Type* from, Type* to) {
  int primitive_count = 0;

  if (is_primitive_type(parser_context, from)) primitive_count++;
  if (is_primitive_type(parser_context, to)) primitive_count++;

  if (primitive_count == 2) { // both are primitive type.
    return true;
  }
  else if (primitive_count == 1) { // only one type is primitive type.
    return false;
  }

  // check for the castability between non primitives.
  Symbol* from_symbol = find_symbol(parser_context->class_symbol_table, from->type_str);
  Symbol* to_symbol = find_symbol(parser_context->class_symbol_table, to->type_str);

  if (from_symbol) { // are they classes?
    goto class_type_check;
  }
  else {
    goto default_type_check;
  }

 class_type_check:{
  
    ClassData* from_type = ((ClassData*)from_symbol->data);
    ClassData* to_type = ((ClassData*)to_symbol->data);
    while (1) {
      // to -> next search find from.
      // up casting.
      if (wcscmp(from_type->name, to_type->name) == 0) {
	return true;
      }

      if (wcscmp(from_type->parent_class_name, L"") == 0) {
	return false;
      }

      Symbol* parent_symbol = find_symbol(parser_context->class_symbol_table, from_type->parent_class_name);

      from_type = ((ClassData*)parent_symbol->data);
    }
    return true;
  }

 default_type_check:{
    if (!wcscmp(from->type_str, L"null")) { // null to class convert, it is okay.
      if (to_symbol) {
	return true;
      }
    }
  }

  return is_same_type(from, to);
}

Type* get_type(Token* tok, wchar_t* str) {
  wchar_t* type = pull_token(str)->str;

  Type* result = (Type*)safe_malloc(sizeof(Type));
  result->type_str = _wcsdup(type);
  result->array_element_type = NULL;
  result->is_array = 0;

  if (!wcscmp(type, L"array")) { // for array
    consume(str, TokLesser);
    Type* array_element_type = get_type(tok, str);
    result->array_element_type = array_element_type;
    result->is_array = 1;
    consume(str, TokGreater);
  }

  return result;
}

Type* infer_type(IrGenContext* ir_context, ParserContext* parser_context, void* ast, wchar_t* search_point_class_name) {
  ASTType type = *((ASTType*)(ast));

  switch (type) {
  case AST_Negative: {
    return infer_type(ir_context, parser_context, ((NegAST*)ast)->ast, search_point_class_name);
  }

  case AST_NumberLiteral: {
    NumberLiteralAST* number_ast = (NumberLiteralAST*)ast;
    Type* result = (Type*)safe_malloc(sizeof(Type));
    result->type_str = number_ast->numeric_type;
    result->array_element_type = NULL;
    result->is_array = 0;
    return result;
  }
  case AST_StringLiteral: {
    Type* result = (Type*)safe_malloc(sizeof(Type));
    result->type_str = L"string";
    result->array_element_type = NULL;
    result->is_array = 0;
    return result;
  }
  case AST_BoolLiteral: {
    Type* result = (Type*)safe_malloc(sizeof(Type));
    result->type_str = L"bool";
    result->array_element_type = NULL;
    result->is_array = 0;
    return result;
  }

  case AST_Identifier: {
    IdentifierAST* ident_ast = (IdentifierAST*)ast;

    if (!wcscmp(ident_ast->identifier->str, L"this")) {
      Type* result = (Type*)safe_malloc(sizeof(Type));
      result->type_str = ir_context->current_class;
      result->is_array = 0;
      result->array_element_type = NULL;

      return result;
    }

    VariableData* data = find_variable_data(parser_context, ident_ast->identifier, search_point_class_name, ident_ast->identifier->str);

    return clone_type(data->type);
  }
  case AST_BinExpr: {
    BinExprAST* bin_expr_ast = (BinExprAST*)ast;

    Type* left_type = get_type_of_last_element(ir_context, parser_context, bin_expr_ast->left, search_point_class_name);
    Type* right_type = get_type_of_last_element(ir_context, parser_context, bin_expr_ast->right, search_point_class_name);

    if (check_castability(parser_context, left_type, right_type)) {
      return right_type;
    }
    else if (check_castability(parser_context, right_type, left_type)) {
      return left_type;
    }
    else {
      // Both types are can not be computed.
      printf(L"[Temporary error] in parser.c Type %s and %s are can not be computed.", left_type, right_type);
      abort();
    }
    break;
  }

  case AST_VariableDeclaration: {
    VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)ast;

    return clone_type(variable_declaration_ast->variable_type);

    break;
  }

  case AST_FunctionCall: {
    FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;

    FunctionData* function_data =
      find_function_data(parser_context, function_call_ast->function_name_token, search_point_class_name, function_call_ast->function_name_token->str, ast);

    return clone_type(function_data->return_type);
  }

  case AST_IdentIncrease:
  case AST_IdentDecrease: {
    if (type == AST_IdentIncrease) {
      IdentIncreaseAST* ident_increase = (IdentIncreaseAST*)ast;

      VariableData* data = find_variable_data(parser_context, ident_increase->identifier, search_point_class_name, ident_increase->identifier->str);
      return clone_type(data->type);
    }

    if (type == AST_IdentDecrease) {
      IdentDecreaseAST* ident_decrease = (IdentDecreaseAST*)ast;

      VariableData* data = find_variable_data(parser_context, ident_decrease->identifier, search_point_class_name, ident_decrease->identifier->str);

      return clone_type(data->type);
    }
  }

  case AST_New: {
    NewAST* new_ast = (NewAST*)ast;

    Type* result = (Type*)safe_malloc(sizeof(Type));
    result->type_str = _wcsdup(new_ast->class_name_token->str);
    result->is_array = 0;
    result->array_element_type = NULL;

    return result;
  }

  case AST_ArrayAccess: {
    ArrayAccessAST* array_access_ast = (ArrayAccessAST*)ast;

    Type* result = infer_type(ir_context, parser_context, array_access_ast->target_array, search_point_class_name);

    int i;
    for (i = 0; i < array_access_ast->access_count; i++) {
      result = result->array_element_type;
    }

    return result;
  }

  case AST_ArrayDeclaration: {
    ArrayDeclarationAST* array_declaration_ast = (ArrayDeclarationAST*)ast;

    Type* element_type = NULL;

    int i;
    for (i = 0; i < array_declaration_ast->element_count; i++) {
      if (element_type == NULL) {
	element_type = infer_type(ir_context, parser_context, array_declaration_ast->elements[i], ir_context->current_class);
      }
      else {
	int elements_type_check = is_same_type(element_type, infer_type(ir_context, parser_context, array_declaration_ast->elements[i], ir_context->current_class));
	if (!elements_type_check) {
	  // Handle error for un-mathced types of elements
	}
      }
    }

    if (element_type == NULL) {
      element_type = (Type*)safe_malloc(sizeof(Type));
      element_type->type_str = L"null";
      element_type->is_array = 0;
      element_type->array_element_type = NULL;
    }

    Type* result = (Type*)safe_malloc(sizeof(Type));
    result->array_element_type = element_type;
    result->is_array = 1;
    result->type_str = L"array";

    return result;
  }
  }
}
