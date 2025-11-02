#include "includes/ir.h"

IrGenContext* create_ir_context() {
  IrGenContext* ir_context = (IrGenContext*)safe_malloc(sizeof(IrGenContext));
  ir_context->current_class = L"";
  ir_context->is_class_initializer = 0;
  ir_context->label_id = 0;
  ir_context->string_builder = create_string_builder();

  return ir_context;
}

StringBuilder* create_string_builder() {
  StringBuilder* string_builder = (StringBuilder*)safe_malloc(sizeof(StringBuilder));
  string_builder->str = NULL;

  return string_builder;
}

void append_integer(StringBuilder* string_builder, int integer) {
    wchar_t* buf = (wchar_t*)safe_malloc(64 * sizeof(wchar_t));

    swprintf(buf, L"%d", integer);

    append_string(string_builder, buf);
}

void append_string(StringBuilder* string_builder, const wchar_t* ctx) {
  if (!ctx) ctx = L"";

  size_t original_length = string_builder->str ? wcslen(string_builder->str) : 0;
  size_t append_length = wcslen(ctx);

  size_t new_length = original_length + append_length + 2;

  wchar_t* result = (wchar_t*)safe_malloc(new_length * sizeof(wchar_t));

  if (string_builder->str) {
    wcscpy_s(result, new_length, string_builder->str);
    free(string_builder->str);
  }
  else {
    result[0] = L'\0';
  }

  wcscat_s(result, new_length, ctx);

  result[original_length + append_length] = L' ';

  result[original_length + append_length + 1] = L'\0';

  string_builder->str = result;

}

void initialize_byte_table() {
  ir_byte_table = create_symbol_table();

  insert_ir_symbol(ir_byte_table, create_ir_data(L"{", (int)ir_byte_table->size));
  insert_ir_symbol(ir_byte_table, create_ir_data(L"}", (int)ir_byte_table->size));
  insert_ir_symbol(ir_byte_table, create_ir_data(L".", (int)ir_byte_table->size));
}

int get_prev_variable_index_size(SymbolTable* variable_symbol_table) {
  SymbolTable* searcher_table = variable_symbol_table;

  int _size = 0;

  while (searcher_table->prev != NULL) {
    searcher_table = searcher_table->prev;
    _size += searcher_table->size;
  }

  return _size;
}

void open_scope(ParserContext* parser_context) {
  SymbolTable* current_symbol_table = parser_context->variable_symbol_table;

  SymbolTable* new_symbol_table = create_symbol_table();
  new_symbol_table->prev = current_symbol_table;
  new_symbol_table->size = 0;

  parser_context->variable_symbol_table = new_symbol_table;
}

void close_scope(ParserContext* parser_context) {
  SymbolTable* current_symbol_table = parser_context->variable_symbol_table;
  parser_context->variable_symbol_table = parser_context->variable_symbol_table->prev;
  free(current_symbol_table);
}

void create_parameter_buffer(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationBundleAST* parameters_ast) {

  append_integer(ir_context->string_builder, parameters_ast->variable_count);

  int i;
  for (i = 0; i < parameters_ast->variable_count; i++) {
    VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

    VariableData* variable_data = create_variable_data(parser_context->variable_symbol_table, parameter->variable_type, parameter->variable_name_token->str, parameter->access_modifier);

    insert_symbol(parser_context->variable_symbol_table, parameter->variable_name_token->str, variable_data);
  }
}

FunctionData* get_member_function_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name) {
  Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

  if (class_symbol == NULL) return NULL;

  ClassData* class_data = class_symbol->data;

  Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
  if (member_symbol != NULL) {
    FunctionData* function_data = member_symbol->data;
    return function_data;
  }
  else {
    if (wcscmp(class_data->parent_class_name, L"") != 0) {
      return get_member_function_data(parser_context, class_data->parent_class_name, function_name);
    }

    return NULL;
  }
  return NULL;
}

int get_member_function_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name) {
  Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

  if (class_symbol == NULL) return IDENTIFIER_NOT_FOUND;

  ClassData* class_data = class_symbol->data;

  Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
  if (member_symbol != NULL) {
    FunctionData* function_data = member_symbol->data;
    return function_data->index + get_parent_member_function_count(parser_context, class_name);
  }
  else {
    if (wcscmp(class_data->parent_class_name, L"") != 0) {
      return get_member_function_index(parser_context, class_data->parent_class_name, function_name);
    }

    return IDENTIFIER_NOT_FOUND;
  }
  return IDENTIFIER_NOT_FOUND;
}

VariableData* get_member_variable_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* variable_name) {
  Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

  if (class_symbol == NULL) return NULL;

  ClassData* class_data = class_symbol->data;

  Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
  if (member_symbol != NULL) {
    VariableData* variable_data = member_symbol->data;
    return variable_data;
  }
  else {
    if (wcscmp(class_data->parent_class_name, L"") != 0) {
      return get_member_variable_data(parser_context, class_data->parent_class_name, variable_name);
    }

    return NULL;
  }
  return NULL;
}

int get_member_variable_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* variable_name) {
  Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

  if (class_symbol == NULL) return IDENTIFIER_NOT_FOUND;

  ClassData* class_data = class_symbol->data;

  Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
  if (member_symbol != NULL) {
    VariableData* variable_data = member_symbol->data;
    return variable_data->index + get_parent_member_variable_count(parser_context, class_name);
  }
  else {
    if (wcscmp(class_data->parent_class_name, L"") != 0) {
      return get_member_variable_index(parser_context, class_data->parent_class_name, variable_name);
    }

    return IDENTIFIER_NOT_FOUND;
  }
  return IDENTIFIER_NOT_FOUND;
}

int get_parent_member_variable_count(ParserContext* parser_context, const wchar_t* class_name) {
  int result = 0;

  if (wcscmp(class_name, L"") != 0) {
    ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, class_name)->data;
    if (wcscmp(current_class_data->parent_class_name, L"") != 0) {
      ClassData* parent_class_data = find_symbol(parser_context->class_symbol_table, current_class_data->parent_class_name)->data;
      result += parent_class_data->member_variables->size;
      result += get_parent_member_variable_count(parser_context, current_class_data->parent_class_name);
    }
  }

  return result;
}

int get_parent_member_function_count(ParserContext* parser_context, const wchar_t* class_name) {
  int result = 0;

  if (wcscmp(class_name, L"") != 0) {
    ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, class_name)->data;

    if (wcscmp(current_class_data->parent_class_name, L"") != 0) {
      ClassData* parent_class_data = find_symbol(parser_context->class_symbol_table, current_class_data->parent_class_name)->data;
      result += parent_class_data->member_functions->size;
      result += get_parent_member_function_count(parser_context, current_class_data->parent_class_name);
    }
  }

  return result;
}

void new_line(StringBuilder* string_builder) {
  append_string(string_builder, L"\n");
}

void create_class_initializer(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* class_ast) {
  append_string(ir_context->string_builder, L"$initializer");
  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);

  ir_context->is_class_initializer = true;

  int i;
  for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
    create_ir(ir_context, parser_context, class_ast->member_variables[i]);
  }

  ir_context->is_class_initializer = false;
  append_string(ir_context->string_builder, L"}");
  new_line(ir_context->string_builder);
}

void create_class_constructor_ir(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* class_ast) {

  append_string(ir_context->string_builder, L"$constructor");
  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);

  open_scope(parser_context);
  ir_context->is_class_initializer = 1;

  int i;
  for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
    create_ir(ir_context, parser_context, class_ast->member_variables[i]);
  }

  ir_context->is_class_initializer = 0;
  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);
  close_scope(parser_context);
}

VariableData* find_variable_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* identifier) {
  Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier);
  VariableData* result = NULL;
  if (local_symbol == NULL) {
    result = get_member_variable_data(parser_context, class_name, identifier);
  }
  else {
    result = local_symbol->data;
  }

  if (result == NULL) {
    handle_error(ER_FailedToFindVariable, tok, parser_context->current_file_name, parser_context->file_str);
  }

  return result;
}

FunctionData* find_function_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* function_name, FunctionCallAST* function_call_ast) {
  Symbol* local_symbol = find_symbol(parser_context->function_symbol_table, function_name);
  FunctionData* result = NULL;

  if (local_symbol == NULL) {
    result = get_member_function_data(parser_context, class_name, function_name);
  }
  else {
    FunctionData* function_data = local_symbol->data;
    result = function_data;
  }

  if (result == NULL) {
    handle_error(ER_FailedToFindFunction, tok, parser_context->current_file_name, parser_context->file_str);
  }

  return result;
}

bool check_accessibility(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* target_class_name, int access_modifier) {
  if (access_modifier == AM_PUBLIC)return true;

  int find_variable_in_class = wcscmp(target_class_name, ir_context->current_class) == 0;

  if (find_variable_in_class) {
    return true;
  }
  else {
    if (wcscmp(ir_context->current_class, L"") == 0) {
      return false;
    }
    else {
      int is_parent = check_castability(parser_context, target_class_name, ir_context->current_class);

      return access_modifier == AM_PROTECTED && is_parent;
    }
  }
}

Token* get_token_of_ast(void* attribute) {
  Token* wrong_token = NULL;
  switch (*((ASTType*)attribute)) {
  case AST_VariableDeclaration: {
    wrong_token = ((VariableDeclarationAST*)attribute)->variable_name_token;
    break;
  }
  case AST_Identifier: {
    wrong_token = ((IdentifierAST*)attribute)->identifier;
    break;
  }
  case AST_IdentIncrease: {
    wrong_token = ((IdentIncreaseAST*)attribute)->identifier;
    break;
  }

  case AST_IdentDecrease: {
    wrong_token = ((IdentDecreaseAST*)attribute)->identifier;
    break;
  }

  case AST_FunctionCall: {
    wrong_token = ((FunctionCallAST*)attribute)->function_name_token;
    break;
  }

  case AST_ArrayAccess: {
    break;
  }
  }

  return wrong_token;
}

void create_attribute_ir(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* target_class_name, void* attribute) {
  Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);

  if (target_class_symbol == NULL) {
    handle_error(ER_FailedToFindAttribute, get_token_of_ast(attribute), parser_context->current_file_name, parser_context->file_str);
  }

  ClassData* target_class = target_class_symbol->data;
  Symbol* member_symbol = NULL;

  switch (*((ASTType*)attribute)) {
  case AST_Identifier: {
    int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentifierAST*)attribute)->identifier->str);
    VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentifierAST*)attribute)->identifier->str);

    if (member_variable_data == NULL) {
      handle_error(ER_FailedToFindMemberVariable, get_token_of_ast(attribute), parser_context->current_file_name, parser_context->file_str);
    }

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_variable_data->access_modifier)) {
      // handle error
      printf("Unable to access variable : %S. access modifier of %S is %d.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
      exit(1);
    }

    append_string(ir_context->string_builder, L"@attr");
    append_integer(ir_context->string_builder, member_variable_index);
    new_line(ir_context->string_builder);

    if (((IdentifierAST*)attribute)->attribute != NULL) {
      create_attribute_ir(ir_context, parser_context, member_variable_data->type->type_str, ((IdentifierAST*)attribute)->attribute);
    }

    break;
  }

  case AST_IdentIncrease: {
    IdentIncreaseAST* ident_increase_ast = (IdentIncreaseAST*)attribute;

    int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentIncreaseAST*)attribute)->identifier->str);
    VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentIncreaseAST*)attribute)->identifier->str);

    if (member_variable_data == NULL) {
      handle_error(ER_FailedToFindMemberVariable, get_token_of_ast(attribute), parser_context->current_file_name, parser_context->file_str);
    }

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_variable_data->access_modifier)) {
      // handle error
      printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
      exit(1);
    }

    append_string(ir_context->string_builder, L"@attr_inc");
    append_integer(ir_context->string_builder, member_variable_data->index);
    new_line(ir_context->string_builder);

    break;
  }

  case AST_IdentDecrease: {
    IdentDecreaseAST* ident_decrease_ast = (IdentDecreaseAST*)attribute;

    int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentDecreaseAST*)attribute)->identifier->str);
    VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentDecreaseAST*)attribute)->identifier->str);

    if (member_variable_data == NULL) {
      handle_error(ER_FailedToFindMemberVariable, get_token_of_ast(attribute), parser_context->current_file_name, parser_context->file_str);
    }

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_variable_data->access_modifier)) {
      // handle error
      printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
      exit(1);
    }

    append_string(ir_context->string_builder, L"@attr_dec");
    append_integer(ir_context->string_builder, member_variable_data->index);
    new_line(ir_context->string_builder);

    break;
  }

  case AST_FunctionCall: {
    FunctionCallAST* function_call_ast = ((FunctionCallAST*)attribute);
    int member_function_index = get_member_function_index(parser_context, target_class_name, function_call_ast->function_name_token->str);
    FunctionData* member_function_data = get_member_function_data(parser_context, target_class_name, function_call_ast->function_name_token->str);

    if (member_function_data == NULL) {
      handle_error(ER_FailedToFindMemberFunction, get_token_of_ast(attribute), parser_context->current_file_name, parser_context->file_str);
    }

    append_string(ir_context->string_builder, L"@attr_call");
    append_integer(ir_context->string_builder, member_function_index);
    append_integer(ir_context->string_builder, function_call_ast->parameter_count);

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_function_data->access_modifier)) {
      // handle error
      printf("Unable to access function : %S. access modifier of %S is %S.", member_function_data->name, member_function_data->name, member_function_data->access_modifier);
      exit(1);
    }

    check_function_call_condition(ir_context, parser_context, member_function_data, function_call_ast->parameters, function_call_ast->parameter_count);

    int i;
    for (i = 0; i < member_function_data->parameter_count; i++) {
      create_ir(ir_context, parser_context, function_call_ast->parameters[i]);
    }

    new_line(ir_context->string_builder);

    if (function_call_ast->attribute != NULL) {
      create_attribute_ir(ir_context, parser_context, member_function_data->return_type->type_str, function_call_ast->attribute);
    }
    break;
  }

  case AST_ArrayAccess: {
    ArrayAccessAST* array_access_ast = (ArrayAccessAST*)attribute;

    // In this case, doesn't need to check accessibility.
    // because it creates its target_array AST.
    // and it will automatically check the accessibility
    create_attribute_ir(ir_context, parser_context, target_class_name, ((ArrayAccessAST*)attribute)->target_array);

    int i;
    for (i = 0; i < array_access_ast->access_count; i++) {
      create_ir(ir_context, parser_context, array_access_ast->indexes[i]);

      append_string(ir_context->string_builder, L"@array_load");
      new_line(ir_context->string_builder);
    }
    break;
  }
  }
}

void create_assign_ir(IrGenContext* ir_context, ParserContext* parser_context, void* left_ast, void* right_ast) {
  Type* to_type = get_type_of_last_element(ir_context, parser_context, left_ast, ir_context->current_class);
  Type* from_type = get_type_of_last_element(ir_context, parser_context, right_ast, ir_context->current_class);

  if (!check_castability(parser_context, from_type, to_type)) {
    handle_error(ER_TypeUnmatch, get_token_of_ast(right_ast), parser_context->current_file_name, parser_context->file_str);
    return;
  }

  create_ir(ir_context, parser_context, right_ast);

  // for non-attribute, direct assign
  // a = 5; a[3] = 5
  switch (*((ASTType*)left_ast)) {
  case AST_Identifier: {
    if (((IdentifierAST*)left_ast)->attribute == NULL) {
      IdentifierAST* identifier_ast = (IdentifierAST*)left_ast;
      wchar_t store_str_buffer[128];

      Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier_ast->identifier->str);

      if (local_symbol == NULL) {
	int id = get_member_variable_index(parser_context, ir_context->current_class, identifier_ast->identifier->str);
	append_string(ir_context->string_builder, L"@mstore");
	append_integer(ir_context->string_builder, id);
      }
      else {
	append_string(ir_context->string_builder, L"@store");
	append_integer(ir_context->string_builder, ((VariableData*)local_symbol->data)->index);
      }
      new_line(ir_context->string_builder);

      return;
    }
    break;
  }

  case AST_ArrayAccess: {
    if (((ArrayAccessAST*)left_ast)->attribute == NULL) {
      IdentifierAST* identifier_ast = ((ArrayAccessAST*)left_ast)->target_array;

      create_ir(ir_context, parser_context, identifier_ast);

      wchar_t store_str_buffer[128];

      int i;
      int access_count = ((ArrayAccessAST*)left_ast)->access_count;
      for (i = 0; i < access_count; i++) {
	create_ir(ir_context, parser_context, ((ArrayAccessAST*)left_ast)->indexes[i]);

	if (i == access_count - 1) {
	  append_string(ir_context->string_builder, L"@array_store");
	}
	else {
	  append_string(ir_context->string_builder, L"@array_load");
	}
	new_line(ir_context->string_builder);
      }
      return;
    }
    break;
  }

  default:
    // handle error for unable to store variable
    printf("Error at ir.c, unable to store variable\n");
    break;
  }

  void* temp_left_ast = left_ast;
  void* attribute_backup = left_ast;
  void* last_ast = NULL;

  int loop = 1;
  // extract last ast for store command.
  while (loop) {
    switch (*((ASTType*)left_ast)) {
    case AST_Identifier: {
      if (((IdentifierAST*)left_ast)->attribute == NULL) {
	if (*((ASTType*)attribute_backup) == AST_Identifier) {
	  ((IdentifierAST*)attribute_backup)->attribute = NULL;
	}
	else if (*((ASTType*)attribute_backup) == AST_FunctionCall) {
	  ((FunctionCallAST*)attribute_backup)->attribute = NULL;
	}
	else if (*((ASTType*)attribute_backup) == AST_ArrayAccess) {
	  ((ArrayAccessAST*)attribute_backup)->attribute = NULL;
	}


	last_ast = left_ast;
	loop = 0;
	break;
      }

      attribute_backup = left_ast;
      left_ast = ((IdentifierAST*)left_ast)->attribute;

      break;
    }

    case AST_FunctionCall: {
      attribute_backup = left_ast;
      left_ast = ((FunctionCallAST*)left_ast)->attribute;

      break;
    }
    case AST_ArrayAccess: {
      if (((ArrayAccessAST*)left_ast)->attribute == NULL) {
	if (*((ASTType*)attribute_backup) == AST_Identifier) {
	  ((IdentifierAST*)attribute_backup)->attribute = NULL;
	}
	else if (*((ASTType*)attribute_backup) == AST_FunctionCall) {
	  ((FunctionCallAST*)attribute_backup)->attribute = NULL;
	}
	else if (*((ASTType*)attribute_backup) == AST_ArrayAccess) {
	  ((ArrayAccessAST*)attribute_backup)->attribute = NULL;
	}

	last_ast = left_ast;
	loop = 0;
	break;
      }

      attribute_backup = left_ast;
      left_ast = ((ArrayAccessAST*)left_ast)->attribute;

      break;
    }

    default:
      loop = 0;
      break;
    }
  }

  if (last_ast == NULL) {
    // handle error for unable to store variable
    printf("Error at ir.c, unable to store variable\n");
    exit(1);
  }

  create_ir(ir_context, parser_context, temp_left_ast);
  // process the last ast for store.
  switch (*((ASTType*)last_ast)) {
  case AST_Identifier: {
    Type* target_class_type = infer_type(ir_context, parser_context, temp_left_ast, L"");
    wchar_t* target_class_name = target_class_type->type_str;
    free(target_class_type);

    Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);
    ClassData* target_class = target_class_symbol->data;

    int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentifierAST*)last_ast)->identifier->str);
    VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentifierAST*)last_ast)->identifier->str);

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_variable_data->access_modifier)) {
      // handle error
      printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
      exit(1);
    }

    append_string(ir_context->string_builder, L"@attr_store");
    append_integer(ir_context->string_builder, member_variable_index);
    new_line(ir_context->string_builder);

    break;
  }
  case AST_ArrayAccess: {
    wchar_t* target_class_name = infer_type(ir_context, parser_context, temp_left_ast, L"")->type_str;
    Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);
    ClassData* target_class = target_class_symbol->data;

    int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((ArrayAccessAST*)last_ast)->target_array->identifier->str);
    VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((ArrayAccessAST*)last_ast)->target_array->identifier->str);

    if (!check_accessibility(ir_context, parser_context, target_class_name, member_variable_data->access_modifier)) {
      // handle error
      printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
      exit(1);
    }

    ArrayAccessAST* array_access_last_ast = (ArrayAccessAST*)last_ast;

    append_string(ir_context->string_builder, L"@attr");
    append_integer(ir_context->string_builder, member_variable_data);
    new_line(ir_context->string_builder);

    wchar_t store_str_buffer[128];

    int i;
    int access_count = array_access_last_ast->access_count;
    for (i = 0; i < access_count; i++) {
      create_ir(ir_context, parser_context, array_access_last_ast->indexes[i]);

      if (i == access_count - 1) {
	append_string(ir_context->string_builder, L"@array_store");
      }
      else {
	append_string(ir_context->string_builder, L"@array_load");
      }
      new_line(ir_context->string_builder);
    }
    break;
  }
  }
}

void check_function_call_condition(IrGenContext* ir_context, ParserContext* parser_context, FunctionData* function_data, const void** parameters, int parameter_count) {
  if (function_data->parameter_types == VARIABLE_ARGUMENTS) {// pass the case for the variable arguments
    return;
  }

  if (function_data->parameter_count != parameter_count) {
    printf("[Temporary error] at ir.c Type not matched\n");
    abort();
  }

  int i;
  for (i = 0; i < parameter_count; i++) {

    Type* infered_type = get_type_of_last_element(ir_context, parser_context, parameters[i], ir_context->current_class);

    if (!check_castability(parser_context, infered_type, function_data->parameter_types[i])) {
      handle_error(ER_TypeUnmatch, get_token_of_ast(parameters[i]), parser_context->current_file_name, parser_context->file_str);
    }
  }

}

void create_if_statement_block(IrGenContext* ir_context, ParserContext* parser_context, IfStatementAST* if_statement_ast, int end_label_id) {
  wchar_t if_statement_buffer[128];

  ir_context->label_id++;
  int block_id = ir_context->label_id;

  if (if_statement_ast->if_type != StmtElse) {
    append_string(ir_context->string_builder, L"@jne");
    append_integer(ir_context->string_builder, block_id);
    new_line(ir_context->string_builder);
  }

  open_scope(parser_context);

  int i;
  for (i = 0; i < if_statement_ast->body_count; i++) {
    create_ir(ir_context, parser_context, if_statement_ast->body[i]);
  }

  close_scope(parser_context);

  // as the statement ended. jump for the terminate the statement
  append_string(ir_context->string_builder, L"@goto");
  append_integer(ir_context->string_builder, end_label_id);
  new_line(ir_context->string_builder);

  if (if_statement_ast->if_type != StmtElse) {
    append_string(ir_context->string_builder, L"@label");
    append_integer(ir_context->string_builder, block_id);
    new_line(ir_context->string_builder);
  }
}

void create_array_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, ArrayDeclarationAST* array_declaration_ast) {
  wchar_t* result = L"";
  int i;
  for (i = 0; i < array_declaration_ast->element_count; i++) {
    create_ir(ir_context, parser_context, array_declaration_ast->elements[i]);
  }

  append_string(ir_context->string_builder, L"@array");
  append_integer(ir_context->string_builder, array_declaration_ast->element_count);
  new_line(ir_context->string_builder);
}

void create_return_ir(IrGenContext* ir_context, ParserContext* parser_context, ReturnAST* return_ast) {

  create_ir(ir_context, parser_context, return_ast->expression);

  append_string(ir_context->string_builder, L"@ret");
  new_line(ir_context->string_builder);
}

void create_bin_expr_ir(IrGenContext* ir_context, ParserContext* parser_context, BinExprAST* bin_expr_ast) {

  if (bin_expr_ast->opType == OpASSIGN) {
    create_assign_ir(ir_context, parser_context, bin_expr_ast->left, bin_expr_ast->right);
    return;
  }

  create_ir(ir_context, parser_context, bin_expr_ast->left);
  create_ir(ir_context, parser_context, bin_expr_ast->right);

  switch (bin_expr_ast->opType) {
  case OpADD:
    append_string(ir_context->string_builder, L"@add");
    break;
  case OpSUB:
    append_string(ir_context->string_builder, L"@sub");
    break;
  case OpMUL:
    append_string(ir_context->string_builder, L"@mul");
    break;
  case OpDIV:
    append_string(ir_context->string_builder, L"@div");
    break;
  case OpEQUAL:
    append_string(ir_context->string_builder, L"@equal");
    break;
  case OpNOTEQUAL:
    append_string(ir_context->string_builder, L"@notequal");
    break;
  case OpGREATER:
    append_string(ir_context->string_builder, L"@greater");
    break;
  case OpLESS:
    append_string(ir_context->string_builder, L"@less");
    break;
  case OpEQUALGREATER:
    append_string(ir_context->string_builder, L"@eqgreater");
    break;
  case OpEQUALLESS:
    append_string(ir_context->string_builder, L"@eqless");
    break;
  case OpOR:
    append_string(ir_context->string_builder, L"@or");
    break;
  case OpAND:
    append_string(ir_context->string_builder, L"@and");
    break;
  }

  new_line(ir_context->string_builder);
}

void create_identifier_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentifierAST* identifier_ast) {
  int is_special_identifier = !wcscmp(identifier_ast->identifier->str, L"this");

  if (is_special_identifier) {
    append_string(ir_context->string_builder, L"@this");
  }
  else {
    Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier_ast->identifier->str);
    VariableData* variable_data = find_variable_data(parser_context, identifier_ast->identifier, ir_context->current_class, identifier_ast->identifier->str);
    int variable_exist_in_local_area = local_symbol != NULL;

    if (variable_exist_in_local_area) {
      append_string(ir_context->string_builder, L"@load");
      append_integer(ir_context->string_builder, ((VariableData*)local_symbol->data)->index);
    }
    else {
      int member_id = get_member_variable_index(parser_context, ir_context->current_class, identifier_ast->identifier->str);
      append_string(ir_context->string_builder, L"@mload");
      append_integer(ir_context->string_builder, member_id);
    }
  }
  new_line(ir_context->string_builder);

  if (identifier_ast->attribute != NULL) {
    Type* target_class_type = infer_type(ir_context, parser_context, identifier_ast, ir_context->current_class);
    wchar_t* target_class_name = target_class_type->type_str;
    free(target_class_type);
    create_attribute_ir(ir_context, parser_context, target_class_name, identifier_ast->attribute);
  }
}

void create_string_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, StringLiteralAST* string_literal_ast) {
  wchar_t* string_literal = string_literal_ast->string_literal_token->str;
  wchar_t push_str_buffer[128];


  // TOOD !!
  //swprintf(push_str_buffer, 128, L"@push string %s", string_literal);
  new_line(ir_context->string_builder);
}

void create_new_ir(IrGenContext* ir_context, ParserContext* parser_context, NewAST* new_ast) {

  Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, new_ast->class_name_token->str);
  if (class_symbol == NULL) {
    handle_error(ER_FailedToFindClass, new_ast->class_name_token, parser_context->current_file_name, parser_context->file_str);
  }

  ClassData* class_data = class_symbol->data;

  FunctionData* constructor_data = class_data->constructor_data;

  check_function_call_condition(ir_context, parser_context, constructor_data, new_ast->parameters, new_ast->parameter_count);

  int i;
  for (i = 0; i < new_ast->parameter_count; i++) {
    create_ir(ir_context, parser_context, new_ast->parameters[i]);
  }

  append_string(ir_context->string_builder, L"@new");
  append_integer(ir_context->string_builder, class_data->index);
  append_integer(ir_context->string_builder, constructor_data->parameter_count);
  new_line(ir_context->string_builder);
}

void create_array_access_ir(IrGenContext* ir_context, ParserContext* parser_context, ArrayAccessAST* array_access_ast) {
  create_ir(ir_context, parser_context, array_access_ast->target_array);

  int i;
  for (i = 0; i < array_access_ast->access_count; i++) {
    create_ir(ir_context, parser_context, array_access_ast->indexes[i]);

    append_string(ir_context->string_builder, L"@array_load");
    new_line(ir_context->string_builder);
  }
}

void create_function_call_ir(IrGenContext* ir_context, ParserContext* parser_context, FunctionCallAST* function_call_ast) {
  wchar_t* function_name = wcsdup(function_call_ast->function_name_token->str);

  FunctionData* function_data =
    find_function_data(parser_context, function_call_ast->function_name_token, ir_context->current_class,
		       function_name, function_call_ast);

  Symbol* local_symbol = find_symbol(parser_context->function_symbol_table, function_call_ast->function_name_token->str);

  int member_call = local_symbol == NULL;
  int local_call = (local_symbol != NULL) && !((FunctionData*)local_symbol->data)->is_builtin_function;
  int builtin_call = (local_symbol != NULL) && ((FunctionData*)local_symbol->data)->is_builtin_function;


  if (member_call) {
    int index = get_member_function_index(parser_context, ir_context->current_class, function_call_ast->function_name_token->str);
    append_string(ir_context->string_builder, L"@mcall");
    append_integer(ir_context->string_builder, index);
    append_integer(ir_context->string_builder, function_call_ast->parameter_count);
  }

  if (local_call) {
    append_string(ir_context->string_builder, L"@call");
    append_integer(ir_context->string_builder, ((FunctionData*)local_symbol->data)->index);
    append_integer(ir_context->string_builder, function_call_ast->parameter_count);
  }

  if (builtin_call) {
    append_string(ir_context->string_builder, L"@bcall");
    append_integer(ir_context->string_builder, ((FunctionData*)local_symbol->data)->index);
    append_integer(ir_context->string_builder, function_call_ast->parameter_count);
  }
  new_line(ir_context->string_builder);

  check_function_call_condition(ir_context, parser_context, function_data, function_call_ast->parameters, function_call_ast->parameter_count);

  int i;
  for (i = 0; i < function_call_ast->parameter_count; i++) {
    create_ir(ir_context, parser_context, function_call_ast->parameters[i]);
  }

  free(function_name);
}

void create_number_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, NumberLiteralAST* number_literal_ast) {
  wchar_t* number_literal = number_literal_ast->number_literal_token->str;

  append_string(ir_context->string_builder, L"@push");
  append_string(ir_context->string_builder, number_literal_ast->numeric_type);
  append_string(ir_context->string_builder, number_literal);
  new_line(ir_context->string_builder);
}

void create_constructor_ir(IrGenContext* ir_context, ParserContext* parser_context, ConstructorAST* constructor_ast) {
  wchar_t* parameter_buffer = L"";

  append_string(ir_context->string_builder, L"$constructor");

  if (constructor_ast->parameters) {
    create_parameter_buffer(ir_context, parser_context, constructor_ast->parameters);
  }

  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);

  int i;
  for (i = 0; i < constructor_ast->body_count; i++) {
    create_ir(ir_context, parser_context, constructor_ast->body[i]);
  }

  append_string(ir_context->string_builder, L"}");
  new_line(ir_context->string_builder);
}

void create_function_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, FunctionDeclarationAST* function_declaration_ast) {
  Symbol* function_symbol = find_symbol(parser_context->function_symbol_table, function_declaration_ast->function_name_token->str);

  int should_find_member_function = function_symbol == NULL && wcscmp(ir_context->current_class, L"");
  if (should_find_member_function) {
    ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, ir_context->current_class)->data;
    function_symbol = find_symbol(current_class_data->member_functions, function_declaration_ast->function_name_token->str);
  }

  open_scope(parser_context);


  FunctionData* function_data = function_symbol->data;

  function_data->access_modifier = function_declaration_ast->access_modifier;

  append_string(ir_context->string_builder, L"func");
  append_integer(ir_context->string_builder, function_data->index);
  
  create_parameter_buffer(ir_context, parser_context, function_declaration_ast->parameters);

  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);

  int i;
  for (i = 0; i < function_declaration_ast->body_count; i++) {
    create_ir(ir_context, parser_context, function_declaration_ast->body[i]);
  }

  append_string(ir_context->string_builder, L"}");
  new_line(ir_context->string_builder);
  new_line(ir_context->string_builder);

  close_scope(parser_context);
}

void create_class_ir(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* ast) {
  ClassAST* class_ast = (ClassAST*)ast;
  wchar_t* parent_name = L"";

  if (class_ast->parent_class_name_token != NULL) {
    parent_name = class_ast->parent_class_name_token->str;
  }

  wchar_t* class_name = class_ast->class_name_token->str;
  ClassData* class_data = find_symbol(parser_context->class_symbol_table, class_name)->data;

  Symbol* parent_symbol = find_symbol(parser_context->class_symbol_table, parent_name);
  ClassData* parent_data = NULL;

  open_scope(parser_context);

  ir_context->current_class = wcsdup(class_ast->class_name_token->str);

  if (parent_symbol != NULL) {
    parent_data = parent_symbol->data;
  }

  append_string(ir_context->string_builder, L"class");
  append_integer(ir_context->string_builder, class_data->index);
  append_integer(ir_context->string_builder, parent_data ? parent_data->index : 0);
  append_string(ir_context->string_builder, L"{");
  new_line(ir_context->string_builder);

  create_class_initializer(ir_context, parser_context, class_ast);
  create_ir(ir_context, parser_context, class_ast->constructor);

  int i;
  for (i = 0; i < class_ast->member_function_count; i++) {
    create_ir(ir_context, parser_context, class_ast->member_functions[i]);
  }

  append_string(ir_context->string_builder, L"}");
  new_line(ir_context->string_builder);
  new_line(ir_context->string_builder);

  free(ir_context->current_class);
  ir_context->current_class = L"";
  close_scope(parser_context);
}

void create_variable_initializer(IrGenContext* ir_context, ParserContext* parser_context, Type* variable_type) {

  if (is_primitive_type(parser_context, variable_type)) {
    append_string(ir_context->string_builder, L"@push");
    append_string(ir_context->string_builder, L"int");
    append_integer(ir_context->string_builder, 0);
  }
  else {
    if (wcscmp(variable_type->type_str, L"array") == 0) {
      append_string(ir_context->string_builder, L"@array");
      append_integer(ir_context->string_builder, 0);
    }
    else {
      append_string(ir_context->string_builder, L"@null");
    }
  }

  new_line(ir_context->string_builder);
}

void initialize_class_data(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* class_ast) {
  int i = 0;
  for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
    int j = 0;
    VariableDeclarationBundleAST* bundle = class_ast->member_variables[i];

    for (j = 0; j < bundle->variable_count; j++) {
      VariableDeclarationAST* var_decl = bundle->variable_declarations[j];

      VariableData* data = NULL;

      // store variables into member variable area.
      SymbolTable* member_variable_symbol_table = ((ClassData*)find_symbol(parser_context->class_symbol_table, class_ast->class_name_token->str)->data)->member_variables;
      data = create_variable_data(member_variable_symbol_table, var_decl->variable_type, var_decl->variable_name_token->str, var_decl->access_modifier);
      insert_symbol(member_variable_symbol_table, var_decl->variable_name_token->str, data);
    }
  }
}

void create_variable_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationAST* variable_declaration_ast) {

  check_type_of_variable_declaration(ir_context, parser_context, variable_declaration_ast);

  VariableData* data = NULL;
  int is_member_variable = ir_context->is_class_initializer;

  if (is_member_variable) {
    data = get_member_variable_data(parser_context, ir_context->current_class, variable_declaration_ast->variable_name_token->str);
  }
  else {
    data = create_variable_data(parser_context->variable_symbol_table, variable_declaration_ast->variable_type, variable_declaration_ast->variable_name_token->str, variable_declaration_ast->access_modifier);
    insert_symbol(parser_context->variable_symbol_table, variable_declaration_ast->variable_name_token->str, data);
  }

  if (variable_declaration_ast->declaration) {
    create_ir(ir_context, parser_context, variable_declaration_ast->declaration);

    Type* to_type = get_type_of_last_element(ir_context, parser_context, variable_declaration_ast, ir_context->current_class);
    Type* from_type = get_type_of_last_element(ir_context, parser_context, variable_declaration_ast->declaration, ir_context->current_class);
    if (!check_castability(parser_context, from_type, to_type)) {
      handle_error(ER_TypeUnmatch, get_token_of_ast(variable_declaration_ast->declaration), parser_context->current_file_name, parser_context->file_str);

      return;
    }
  }
  else {
    create_variable_initializer(ir_context, parser_context, variable_declaration_ast->variable_type);
  }

  // indexing for additional parent class member variables.
  int parent_variable_count = get_parent_member_variable_count(parser_context, ir_context->current_class);

  if (ir_context->is_class_initializer) {
    append_string(ir_context->string_builder, L"@mstore");
    append_integer(ir_context->string_builder, data->index + parent_variable_count);
  }
  else {
    append_string(ir_context->string_builder, L"@store");
    append_integer(ir_context->string_builder, data->index);
  }

  new_line(ir_context->string_builder);
}

void create_ident_increase_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentIncreaseAST* ident_increase_ast) {
  VariableData* variable_data = find_variable_data(parser_context, ident_increase_ast->identifier, ir_context->current_class, ident_increase_ast->identifier->str);

  append_string(ir_context->string_builder, L"@inc");
  append_integer(ir_context->string_builder, variable_data->index);
  new_line(ir_context->string_builder);
}

void create_ident_decrease_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentDecreaseAST* ident_decrease_ast) {
  VariableData* variable_data = find_variable_data(parser_context, ident_decrease_ast->identifier, ir_context->current_class, ident_decrease_ast->identifier->str);

  append_string(ir_context->string_builder, L"@dec");
  append_integer(ir_context->string_builder, variable_data->index);
  new_line(ir_context->string_builder);
}

void create_if_statement_ir(IrGenContext* ir_context, ParserContext* parser_context, IfStatementAST* if_statement_ast) {
  if (if_statement_ast->if_type != StmtElse) {
    create_ir(ir_context, parser_context, if_statement_ast->condition);
  }

  ir_context->label_id++;
  int end_label_id = ir_context->label_id;

  create_if_statement_block(ir_context, parser_context, if_statement_ast, end_label_id);

  if (if_statement_ast->next_statement != NULL) {
    create_ir(ir_context, parser_context, if_statement_ast->next_statement);
  }

  append_string(ir_context->string_builder, L"@label");
  append_integer(ir_context->string_builder, end_label_id);
  new_line(ir_context->string_builder);
}

void create_for_statement_ir(IrGenContext* ir_context, ParserContext* parser_context, ForStatementAST* for_statement_ast) {
  wchar_t* result = L"";
  open_scope(parser_context);

  create_ir(ir_context, parser_context, for_statement_ast->init);

  ir_context->label_id++;
  int end_label_id = ir_context->label_id;
  ir_context->label_id++;
  int begin_label_id = ir_context->label_id;

  append_string(ir_context->string_builder, L"@goto");
  append_integer(ir_context->string_builder, end_label_id);
  new_line(ir_context->string_builder);

  append_string(ir_context->string_builder, L"@label");
  append_integer(ir_context->string_builder, begin_label_id);
  new_line(ir_context->string_builder);

  int i;
  for (i = 0; i < for_statement_ast->body_count; i++) {
    create_ir(ir_context, parser_context, for_statement_ast->body[i]);
  }

  create_ir(ir_context, parser_context, for_statement_ast->step);

  append_string(ir_context->string_builder, L"@label");
  append_integer(ir_context->string_builder, end_label_id);
  new_line(ir_context->string_builder);

  create_ir(ir_context, parser_context, for_statement_ast->condition);

  append_string(ir_context->string_builder, L"@je");
  append_integer(ir_context->string_builder, begin_label_id);
  new_line(ir_context->string_builder);

  close_scope(parser_context);
}

void create_bool_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, BoolLiteralAST* bool_literal_ast) {
  append_string(ir_context->string_builder, L"@push");
  append_string(ir_context->string_builder, L"bool");
  append_string(ir_context->string_builder, bool_literal_ast->bool_type ? L"true" : L"false");
  new_line(ir_context->string_builder);
}

void create_neg_ir(IrGenContext* ir_context, ParserContext* parser_context, NegAST* negative_ast) {
  create_ir(ir_context, parser_context, negative_ast->ast);
  append_string(ir_context->string_builder, L"@neg");
  new_line(ir_context->string_builder);
}

void create_ir(IrGenContext* ir_context, ParserContext* parser_context, void* ast) {

  switch (*((ASTType*)ast)) {

  case AST_Negative: {
    create_neg_ir(ir_context, parser_context, (NegAST*)ast);
    break;
  }

  case AST_BoolLiteral: {
    create_bool_literal_ir(ir_context, parser_context, (BoolLiteralAST*)ast);
    break;
  }

  case AST_ArrayDeclaration: {
    create_array_declaration_ir(ir_context, parser_context, (ArrayDeclarationAST*)ast);
    break;
  }

  case AST_Return: {
    create_return_ir(ir_context, parser_context, (ReturnAST*)ast);
    break;
  }

  case AST_BinExpr: {
    create_bin_expr_ir(ir_context, parser_context, (BinExprAST*)ast);
    break;
  }

  case AST_Identifier: {
    create_identifier_ir(ir_context, parser_context, (IdentifierAST*)ast);
    break;
  }

  case AST_StringLiteral: {
    create_string_literal_ir(ir_context, parser_context, (StringLiteralAST*)ast);
    break;
  }

  case AST_New: {
    create_new_ir(ir_context, parser_context, (NewAST*)ast);
    break;
  }

  case AST_ArrayAccess: {
    create_array_access_ir(ir_context, parser_context, (ArrayAccessAST*)ast);
    break;
  }

  case AST_FunctionCall: {
    create_function_call_ir(ir_context, parser_context, (FunctionCallAST*)ast);
    break;
  }

  case AST_NumberLiteral: {
    create_number_literal_ir(ir_context, parser_context, (NumberLiteralAST*)ast);
    break;
  }

  case AST_Constructor: {
    create_constructor_ir(ir_context, parser_context, (ConstructorAST*)ast);
    break;
  }

  case AST_FunctionDeclaration: {
    create_function_declaration_ir(ir_context, parser_context, (FunctionDeclarationAST*)ast);
    break;
  }

  case AST_Class: {
    create_class_ir(ir_context, parser_context, (ClassAST*)ast);
    break;
  }

  case AST_VariableDeclarationBundle: {
    VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)ast;

    int i;
    for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
      create_variable_declaration_ir(ir_context, parser_context, variable_declaration_bundle_ast->variable_declarations[i]);
    }

    break;
  }

  case AST_VariableDeclaration: {
    create_variable_declaration_ir(ir_context, parser_context, (VariableDeclarationAST*)ast);
    break;
  }

  case AST_IdentIncrease: {
    create_ident_increase_ir(ir_context, parser_context, (IdentIncreaseAST*)ast);
    break;
  }

  case AST_IdentDecrease: {
    create_ident_decrease_ir(ir_context, parser_context, (IdentDecreaseAST*)ast);
    break;
  }

  case AST_IfStatement: {
    create_if_statement_ir(ir_context, parser_context, (IfStatementAST*)ast);
    break;
  }

  case AST_ForStatement: {
    create_for_statement_ir(ir_context, parser_context, (ForStatementAST*)ast);
    break;
  }
  }

}
