#include "includes/parser.h"

void set_file_string(ParserContext* parser_context, const wchar_t* str) {
  parser_context->file_str = _wcsdup(str);
}

ParserContext* create_parser_context() {
  ParserContext* parser_context = (ParserContext*)safe_malloc(sizeof(ParserContext));
  initialize_parser_context(parser_context);
  return parser_context;
}

bool is_primitive_type(ParserContext* parser_context, Type* type) {
  return find_symbol_from_set(parser_context->primitive_types, type->type_str) != NULL;
}

void initialize_parser_context(ParserContext* parser_context) {
  parser_context->class_count = 0;
  parser_context->current_class = L"";
  parser_context->current_function_name = L"";
  parser_context->current_file_name = L"";
  parser_context->file_str = L"";

  parser_context->variable_symbol_table = create_symbol_table();
  parser_context->function_symbol_table = create_symbol_table();
  parser_context->class_symbol_table = create_symbol_table();

  parser_context->primitive_types = create_set();
}

void* parse_term(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  void* node = parse(tokenizer_context, parser_context, str);

  while (peek_token(tokenizer_context, str) && (peek_token(tokenizer_context, str)->type == TokMul || peek_token(tokenizer_context, str)->type == TokDiv)) {
    TokenType op = pull_token(tokenizer_context, str)->type;
    void* right = parse(tokenizer_context, parser_context, str);

    OperatorType op_type = (op == TokMul) ? OpMUL : OpDIV;

    BinExprAST* bin_expr = (BinExprAST*)safe_malloc(sizeof(BinExprAST));

    bin_expr->TYPE = AST_BinExpr;
    bin_expr->left = node;
    bin_expr->right = right;
    bin_expr->opType = op_type;

    node = bin_expr;
  }
  return node;
}

void* parse_simple_expression(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  void* node = parse_term(tokenizer_context, parser_context, str);

  while (peek_token(tokenizer_context, str) && (peek_token(tokenizer_context, str)->type == TokAdd || peek_token(tokenizer_context, str)->type == TokSub)) {
    TokenType op = pull_token(tokenizer_context, str)->type;
    void* right = parse_term(tokenizer_context, parser_context, str);

    OperatorType op_type = (op == TokAdd) ? OpADD : OpSUB;

    BinExprAST* bin_expr = (BinExprAST*)safe_malloc(sizeof(BinExprAST));

    bin_expr->TYPE = AST_BinExpr;
    bin_expr->left = node;
    bin_expr->right = right;
    bin_expr->opType = op_type;

    node = bin_expr;
  }
  return node;
}

void* parse_unary_expression(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  if (peek_token(tokenizer_context, str) && peek_token(tokenizer_context, str)->type == TokNot) {
    pull_token(tokenizer_context, str); // Consume '!'
    UnaryExprAST* unary_expr = (UnaryExprAST*)safe_malloc(sizeof(UnaryExprAST));

    unary_expr->TYPE = AST_UnaryExpr;
    unary_expr->expr = parse_unary_expression(tokenizer_context, parser_context, str);
    return unary_expr;
  }
  return parse_simple_expression(tokenizer_context, parser_context, str);
}

void* parse_compare_expression(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  void* node = parse_unary_expression(tokenizer_context, parser_context, str);

  while (peek_token(tokenizer_context, str) && (peek_token(tokenizer_context, str)->type == TokEqual || peek_token(tokenizer_context, str)->type == TokNotEqual ||
						peek_token(tokenizer_context, str)->type == TokGreater || peek_token(tokenizer_context, str)->type == TokLesser ||
						peek_token(tokenizer_context, str)->type == TokEqualGreater || peek_token(tokenizer_context, str)->type == TokEqualLesser)) {
    Token* operator_token = pull_token(tokenizer_context, str);
    TokenType op = operator_token->type;
    void* right = parse_unary_expression(tokenizer_context, parser_context, str);

    OperatorType op_type = OpNone;
    switch (op) {
    case TokEqual: op_type = OpEQUAL; break;
    case TokNotEqual: op_type = OpNOTEQUAL; break;
    case TokGreater: op_type = OpGREATER; break;
    case TokLesser: op_type = OpLESS; break;
    case TokEqualGreater: op_type = OpEQUALGREATER; break;
    case TokEqualLesser: op_type = OpEQUALLESS; break;
    }

    BinExprAST* bin_expr = (BinExprAST*)safe_malloc(sizeof(BinExprAST));

    bin_expr->TYPE = AST_BinExpr;
    bin_expr->left = node;
    bin_expr->right = right;

    if (op_type == OpNone) {
      handle_error(ER_UndefinedOperator, operator_token, parser_context->current_file_name, parser_context->file_str);
    }

    bin_expr->opType = op_type;

    node = bin_expr;
  }
  return node;
}

void* parse_expression(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  void* node = parse_compare_expression(tokenizer_context, parser_context, str);

  while (peek_token(tokenizer_context, str) && (peek_token(tokenizer_context, str)->type == TokOr || peek_token(tokenizer_context, str)->type == TokAnd)) {
    TokenType op = pull_token(tokenizer_context, str)->type;
    void* right = parse_compare_expression(tokenizer_context, parser_context, str);

    OperatorType op_type;
    switch (op) {
    case TokOr: op_type = OpOR; break;
    case TokAnd: op_type = OpAND; break;
    }

    BinExprAST* bin_expr = (BinExprAST*)safe_malloc(sizeof(BinExprAST));

    bin_expr->TYPE = AST_BinExpr;
    bin_expr->left = node;
    bin_expr->right = right;
    bin_expr->opType = op_type;

    node = bin_expr;
  }

  return node;
}

void consume(TokenizerContext* tokenizer_context, wchar_t* str, TokenType expected_type) {
  Token* tok = pull_token(tokenizer_context, str);

  if (tok->type != expected_type) {
    handle_error(ER_UnexpectedToken, tok, L"main.cnut", str);
  }
}

void* create_if_statement_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  IfStatementAST* if_statement = (IfStatementAST*)safe_malloc(sizeof(IfStatementAST));

  IfStatementType if_type = StmtIf;

  if (peek_token(tokenizer_context, str)->type == TokElse) {
    consume(tokenizer_context, str, TokElse);

    if (peek_token(tokenizer_context, str)->type == TokIf) {
      if_type = StmtElseIf;

      consume(tokenizer_context, str, TokIf);
    }
    else {
      if_type = StmtElse;
    }
  }

  if_statement->next_statement = NULL;
  if_statement->if_type = if_type;
  if_statement->TYPE = AST_IfStatement;
  if_statement->body_count = 0;
  if_statement->condition = NULL;

  if (if_type != StmtElse) {
    consume(tokenizer_context, str, TokLParen); // consume (

    void* condition = parse_expression(tokenizer_context, parser_context, str);

    consume(tokenizer_context, str, TokRParen); // consume )

    if_statement->condition = condition;
  }

  consume(tokenizer_context, str, TokLBracket); // consume {

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    Token* body_token = peek_token(tokenizer_context, str);
    void* body_element = parse(tokenizer_context, parser_context, str);

    if(body_token->type == TokIdent){
      consume(tokenizer_context, str, TokSemiColon);
    }
    
    if (if_statement->body_count == 0) {
      if_statement->body = (void**)safe_malloc(sizeof(void*));
    }
    else {
      if_statement->body = (void**)safe_realloc(if_statement->body, sizeof(void*) * (if_statement->body_count + 1));
    }

    if_statement->body[if_statement->body_count] = body_element;

    if_statement->body_count++;
  }

  consume(tokenizer_context, str, TokRBracket); // consume }

  if (peek_token(tokenizer_context,str)->type == TokElse) {
    if_statement->next_statement = create_if_statement_ast(tokenizer_context,parser_context, tok, str);
  }

  return if_statement;
}

void* create_paren_group_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  void* node = parse_expression(tokenizer_context, parser_context, str);

  consume(tokenizer_context, str, TokRParen); // consume )

  return node;
}

void* create_string_literal_ast(ParserContext* parser_context, Token* tok, wchar_t* str) {
  StringLiteralAST* literal = (StringLiteralAST*)safe_malloc(sizeof(StringLiteralAST));
  literal->TYPE = AST_StringLiteral;
  literal->string_literal_token = tok;

  return literal;
}

void* create_number_literal_ast(ParserContext* parser_context, Token* tok, wchar_t* str) {
  NumberLiteralAST* literal = (NumberLiteralAST*)safe_malloc(sizeof(NumberLiteralAST));
  literal->TYPE = AST_NumberLiteral;
  literal->number_literal_token = tok;
  wchar_t* numeric_type;

  if (is_decimal(tok->str)) {
    if (tok->str[wcslen(tok->str) - 1] == L'f') {
      numeric_type = L"float";
    }
    else {
      numeric_type = L"double";
    }
  }
  else {
    numeric_type = L"int";
  }

  literal->numeric_type = numeric_type;

  return literal;
}

void* create_return_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  void* result = NULL;
  void* expression = parse_expression(tokenizer_context, parser_context, str);

  result = (ReturnAST*)safe_malloc(sizeof(ReturnAST));
  ((ReturnAST*)result)->expression = expression;
  ((ReturnAST*)result)->TYPE = AST_Return;

  consume(tokenizer_context, str, TokSemiColon);
  
  return result;
}

FunctionCallParameterContext* parse_function_call_parameter(TokenizerContext* tokenizer_context, ParserContext* parser_context, wchar_t* str) {
  FunctionCallParameterContext* result = (FunctionCallParameterContext*)safe_malloc(sizeof(FunctionCallParameterContext));

  result->parameters = NULL;
  result->parameter_count = 0;

  consume(tokenizer_context, str, TokLParen);

  while (peek_token(tokenizer_context, str)->type != TokRParen) {
    void* parameter = parse_expression(tokenizer_context, parser_context, str);

    if (result->parameter_count == 0) {
      result->parameters = (void**)safe_malloc(sizeof(void*));
    }
    else {
      result->parameters = (void**)safe_realloc(result->parameters, sizeof(void*) * (result->parameter_count + 1));
    }

    int index = result->parameter_count;
    result->parameters[index] = parameter;
    result->parameter_count++;

    if (peek_token(tokenizer_context, str)->type == TokComma) {
      consume(tokenizer_context, str, TokComma);
    }
    else if (peek_token(tokenizer_context, str)->type == TokRParen) {
      break;
    }
    else {
      handle_error(ER_UnexpectedToken, peek_token(tokenizer_context, str), parser_context->current_file_name, parser_context->file_str);
    }
  }

  consume(tokenizer_context, str, TokRParen);
  
  return result;
}

void free_function_call_parameter(FunctionCallParameterContext* function_call_parameter) {
  int i;
  for(i=0; i<function_call_parameter->parameter_count; i++){
    free(function_call_parameter->parameters[i]);
  }
      
  free(function_call_parameter);
}

void* create_function_call_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  FunctionCallAST* result = (FunctionCallAST*)safe_malloc(sizeof(FunctionCallAST));

  FunctionCallParameterContext* function_call_parameter = parse_function_call_parameter(tokenizer_context, parser_context, str);

  wchar_t* function_name = _wcsdup(tok->str);
  result->function_name_token = tok;
  result->TYPE = AST_FunctionCall;
  result->parameter_count = function_call_parameter->parameter_count;
  result->parameters = function_call_parameter->parameters;

  free_function_call_parameter(function_call_parameter);

  return result;
}

void* create_array_access_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, void* target_array, Token* tok, wchar_t* str) {
  ArrayAccessAST* array_access_ast = (ArrayAccessAST*)safe_malloc(sizeof(ArrayAccessAST));
  array_access_ast->TYPE = AST_ArrayAccess;
  array_access_ast->access_count = 0;
  array_access_ast->indexes = NULL;

  int access_count = 0;
  void** indexes = NULL;

  while (peek_token(tokenizer_context, str)->type == TokLSquareBracket) {
    consume(tokenizer_context, str, TokLSquareBracket);
    void* index = parse_expression(tokenizer_context, parser_context, str);
    consume(tokenizer_context,str, TokRSquareBracket);

    indexes = (void**)safe_realloc(indexes, (access_count + 1) * sizeof(void*));
    indexes[access_count] = index;

    access_count++;
  }

  array_access_ast->indexes = indexes;

  array_access_ast->target_array = target_array;
  array_access_ast->access_count = access_count;

  return array_access_ast;
}

void* create_identifier_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str, bool is_attribute_identifier) {
  void* result = NULL;

  Token* next_token = peek_token(tokenizer_context,str);
  if (next_token->type == TokLParen) { // identifier ( 
    result = create_function_call_ast(tokenizer_context, parser_context, tok, str);

    ((FunctionCallAST*)result)->attribute = NULL;

    if (peek_token(tokenizer_context,str)->type == TokDot) {
      consume(tokenizer_context,str, TokDot);
      ((FunctionCallAST*)result)->attribute = create_identifier_ast(tokenizer_context,parser_context, pull_token(tokenizer_context, str), str, true);
    }
  }
  else {
    if (next_token->type == TokIncrease || next_token->type == TokDecrease) {
      if (next_token->type == TokIncrease) {
	consume(tokenizer_context,str, TokIncrease);
	result = (IdentIncreaseAST*)safe_malloc(sizeof(IdentIncreaseAST));
	((IdentIncreaseAST*)result)->identifier = tok;
	((IdentIncreaseAST*)result)->TYPE = AST_IdentIncrease;
      }

      if (next_token->type == TokDecrease) {
	consume(tokenizer_context,str, TokDecrease);
	result = (IdentDecreaseAST*)safe_malloc(sizeof(IdentDecreaseAST));
	((IdentDecreaseAST*)result)->identifier = tok;
	((IdentDecreaseAST*)result)->TYPE = AST_IdentDecrease;
      }
    }
    else {
      result = (IdentifierAST*)safe_malloc(sizeof(IdentifierAST));
      ((IdentifierAST*)result)->TYPE = AST_Identifier;
      ((IdentifierAST*)result)->identifier = tok;

      ((IdentifierAST*)result)->attribute = NULL;

      if (peek_token(tokenizer_context,str)->type == TokDot) {
	consume(tokenizer_context,str, TokDot);
	((IdentifierAST*)result)->attribute = create_identifier_ast(tokenizer_context,parser_context, pull_token(tokenizer_context, str), str, true);
      }
    }
  }

  if (peek_token(tokenizer_context,str)->type == TokLSquareBracket) {
    result = create_array_access_ast(tokenizer_context,parser_context, result, tok, str);
    ((ArrayAccessAST*)result)->attribute = NULL;

    if (peek_token(tokenizer_context,str)->type == TokDot) {
      consume(tokenizer_context,str, TokDot);
      ((ArrayAccessAST*)result)->attribute = create_identifier_ast(tokenizer_context,parser_context, pull_token(tokenizer_context, str), str, true);
    }
  }

  int is_assigned = !is_attribute_identifier && peek_token(tokenizer_context, str)->type == TokAssign;
  if (is_assigned) {
    consume(tokenizer_context,str, TokAssign);

    void* right_term = parse_expression(tokenizer_context, parser_context, str);

    BinExprAST* bin_expr_ast = (BinExprAST*)safe_malloc(sizeof(BinExprAST));
    bin_expr_ast->TYPE = AST_BinExpr;
    bin_expr_ast->left = result;
    bin_expr_ast->right = right_term;
    bin_expr_ast->opType = OpASSIGN;

    result = bin_expr_ast;
  }

  return result;
}

void* create_function_declaration_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  // func add(a: int, b: int): int {}
  FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)safe_malloc(sizeof(FunctionDeclarationAST));
  function_declaration_ast->TYPE = AST_FunctionDeclaration;
  function_declaration_ast->body_count = 0;
  function_declaration_ast->access_modifier = AM_DEFAULT;
  
  Token* function_name_token = pull_token(tokenizer_context, str);
  
  wchar_t* function_name = function_name_token->str;
  function_declaration_ast->function_name_token = function_name_token;

  parser_context->current_function_name = _wcsdup(function_name);

  VariableDeclarationBundleAST* parameters = create_function_parameters(tokenizer_context, tok, str);
  function_declaration_ast->parameters = parameters;
  
  consume(tokenizer_context, str, TokColon); // consume :

  Type* return_type_element = get_type(tokenizer_context, tok, str);

  function_declaration_ast->return_type = return_type_element;

  consume(tokenizer_context, str, TokLBracket); // consume {

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    Token* body_token = peek_token(tokenizer_context, str);
    void* body_element = parse(tokenizer_context, parser_context, str);

    if(body_token->type == TokIdent){
      consume(tokenizer_context, str, TokSemiColon);
    }
 
    if (function_declaration_ast->body_count == 0) {
      function_declaration_ast->body = (void**)safe_malloc(sizeof(void*));
    }
    else {
      function_declaration_ast->body = (void**)safe_realloc(function_declaration_ast->body, sizeof(void*) * (function_declaration_ast->body_count + 1));
    }

    function_declaration_ast->body[function_declaration_ast->body_count] = body_element;

    function_declaration_ast->body_count++;
  }

  consume(tokenizer_context, str, TokRBracket); // consume }

  if (wcscmp(parser_context->current_class, L"") == 0) {
    FunctionData* data = create_function_data(parser_context->function_symbol_table, function_name, function_declaration_ast->return_type, parameters);
    insert_symbol(parser_context->function_symbol_table, data->name, data);
  }
  else {
    SymbolTable* member_function_symbol_table = ((ClassData*)find_symbol(parser_context->class_symbol_table, parser_context->current_class)->data)->member_functions;
    FunctionData* data = create_function_data(parser_context->function_symbol_table, function_name, function_declaration_ast->return_type, parameters);
    insert_symbol(member_function_symbol_table, data->name, data);
  }
  parser_context->current_function_name = L"";

  return function_declaration_ast;
}

void* create_for_statement_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  ForStatementAST* for_statement = (ForStatementAST*)safe_malloc(sizeof(ForStatementAST));

  for_statement->TYPE = AST_ForStatement;
  for_statement->body_count = 0;

  consume(tokenizer_context, str, TokLParen); // consume (

  void* init = parse_expression(tokenizer_context, parser_context, str);

  if (peek_token(tokenizer_context, str)->type == TokSemiColon) {
    consume(tokenizer_context, str, TokSemiColon);
  }

  void* condition = parse_expression(tokenizer_context, parser_context, str);

  if (peek_token(tokenizer_context, str)->type == TokSemiColon) {
    consume(tokenizer_context, str, TokSemiColon);
  }

  void* step = parse_expression(tokenizer_context, parser_context, str);

  consume(tokenizer_context, str, TokRParen); // consume )

  for_statement->init = init;
  for_statement->condition = condition;
  for_statement->step = step;

  consume(tokenizer_context, str, TokLBracket); // consume {

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    Token* body_token = peek_token(tokenizer_context, str);
    void* body_element = parse(tokenizer_context, parser_context, str);

    if(body_token->type == TokIdent){
      consume(tokenizer_context, str, TokSemiColon);
    }

    if (for_statement->body_count == 0) {
      for_statement->body = (void**)safe_malloc(sizeof(void*));
    }
    else {
      for_statement->body = (void**)safe_realloc(for_statement->body, sizeof(void*) * (for_statement->body_count + 1));
    }

    for_statement->body[for_statement->body_count] = body_element;

    for_statement->body_count++;
  }

  consume(tokenizer_context, str, TokRBracket); // consume }

  return for_statement;
}

void* create_variable_declaration_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  // var i: int;
  VariableDeclarationBundleAST* bundles = (VariableDeclarationBundleAST*)safe_malloc(sizeof(VariableDeclarationBundleAST));
  bundles->TYPE = AST_VariableDeclarationBundle;
  bundles->variable_count = 0;
  bundles->variable_declarations = NULL;

  while (1) {
    Token* name_token = pull_token(tokenizer_context, str);
    wchar_t* name = name_token->str;

    consume(tokenizer_context, str, TokColon);

    wchar_t* type_element = get_type(tokenizer_context, tok, str);

    tok = pull_token(tokenizer_context, str);

    void* declaration = NULL;

    if (tok->type == TokSemiColon) {
    }
    else if (tok->type == TokComma) {
    }
    else if (tok->type == TokAssign) {
      declaration = parse_expression(tokenizer_context, parser_context, str);
    }

    VariableDeclarationAST* variable = (VariableDeclarationAST*)safe_malloc(sizeof(VariableDeclarationAST));
    variable->TYPE = AST_VariableDeclaration;
    variable->variable_name_token = name_token;
    variable->variable_type = type_element;

    variable->declaration = declaration;
    variable->access_modifier = AM_DEFAULT;

    if (bundles->variable_count == 0) {
      bundles->variable_declarations = (VariableDeclarationAST*)safe_malloc(sizeof(VariableDeclarationAST) * 1);
    }
    else {
      bundles->variable_declarations
	= (VariableDeclarationAST*)safe_realloc(bundles->variable_declarations, sizeof(VariableDeclarationAST) * (bundles->variable_count + 1));
    }

    bundles->variable_declarations[bundles->variable_count] = variable;
    bundles->variable_count++;

    if (tok->type == TokSemiColon) {
      break;
    }

    if (peek_token(tokenizer_context, str)->type == TokSemiColon) {
      consume(tokenizer_context, str, TokSemiColon);
      break;
    }
  }

  return bundles;
}

void create_class_constructor_data(ParserContext* parser_context, ClassAST* class_ast) {
  Type* constructor_return_type_element = (Type*)safe_malloc(sizeof(Type));
  constructor_return_type_element->type_str = L"void";
  constructor_return_type_element->is_array = 0;
  constructor_return_type_element->array_element_type = NULL;

  ClassData* class_data = ((ClassData*)find_symbol(parser_context->class_symbol_table, parser_context->current_class)->data);
  class_data->constructor_data = create_function_data(class_data->member_functions, L"constructor", constructor_return_type_element, class_ast->constructor->parameters);
}

void initialize_constructor_of_class(ClassAST* class_ast) {
  class_ast->constructor = (ConstructorAST*)safe_malloc(sizeof(ConstructorAST));
  class_ast->constructor->TYPE = AST_Constructor;
  class_ast->constructor->body_count = 0;
  VariableDeclarationBundleAST* empty_parameters = (VariableDeclarationBundleAST*)safe_malloc(sizeof(VariableDeclarationBundleAST));
  empty_parameters->variable_count = 0;
  empty_parameters->variable_declarations = NULL;
  class_ast->constructor->parameters = empty_parameters;
}

Token* get_parent_name(TokenizerContext* tokenizer_context, wchar_t* str) {
  Token* parent_name = NULL;
  if (peek_token(tokenizer_context, str)->type == TokExtends) {
    consume(tokenizer_context, str, TokExtends);
    parent_name = pull_token(tokenizer_context, str);
  }
  return parent_name;
}

void* create_class_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  Token* class_name_token = pull_token(tokenizer_context, str);
  wchar_t* class_name = class_name_token->str;

  Token* parent_name_token = get_parent_name(tokenizer_context, str);
  wchar_t* parent_name = parent_name_token == NULL ? L"" : _wcsdup(parent_name_token->str);

  ClassAST* class_ast = (ClassAST*)safe_malloc(sizeof(ClassAST));

  class_ast->TYPE = AST_Class;
  class_ast->class_name_token = class_name_token;
  class_ast->parent_class_name_token = parent_name_token;
  class_ast->constructor = NULL;

  class_ast->member_function_count = 0;
  class_ast->member_variable_bundle_count = 0;

  ClassData* class_data = create_class_data(parser_context, class_ast);

  insert_symbol(parser_context->class_symbol_table, class_name, class_data);

  parser_context->current_class = class_name_token->str;
  initialize_constructor_of_class(class_ast);

  consume(tokenizer_context, str, TokLBracket);

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    void* body_element = parse(tokenizer_context, parser_context, str);

    ASTType body_type = *((ASTType*)body_element);

    switch (body_type) {
    case AST_FunctionDeclaration:
      class_ast->member_function_count++;

      if (class_ast->member_function_count == 1) {
	class_ast->member_functions = (void**)safe_malloc(sizeof(void*));
      }
      else {
	class_ast->member_functions = (void**)safe_realloc(class_ast->member_functions, class_ast->member_function_count * sizeof(void*));
      }

      class_ast->member_functions[class_ast->member_function_count - 1] = body_element;

      break;
    case AST_VariableDeclarationBundle:
      class_ast->member_variable_bundle_count++;

      if (class_ast->member_variable_bundle_count == 1) {
	class_ast->member_variables = (void**)safe_malloc(sizeof(void*));
      }
      else {
	class_ast->member_variables = (void**)safe_realloc(class_ast->member_variables, class_ast->member_variable_bundle_count * sizeof(void*));
      }

      class_ast->member_variables[class_ast->member_variable_bundle_count - 1] = body_element;

      break;
    case AST_Constructor:
      class_ast->constructor = body_element;

      break;
    }
  }

  create_class_constructor_data(parser_context, class_ast);

  parser_context->current_class = L"";

  consume(tokenizer_context, str, TokRBracket);

  return class_ast;
}

VariableDeclarationBundleAST* create_function_parameters(TokenizerContext* tokenizer_context, Token* tok, wchar_t* str) {
  consume(tokenizer_context, str, TokLParen); // consume (

  VariableDeclarationBundleAST* parameters = (VariableDeclarationBundleAST*)safe_malloc(sizeof(VariableDeclarationBundleAST));
  parameters->variable_count = 0;
  parameters->variable_declarations = NULL;
  parameters->TYPE = AST_VariableDeclarationBundle;
  
  while (peek_token(tokenizer_context, str)->type != TokRParen) {
    Token* parameter_name_token = pull_token(tokenizer_context, str);
    wchar_t* parameter_name = parameter_name_token->str;
    // assert parameter_name_token is type identifier

    consume(tokenizer_context, str, TokColon); // consume :

    Type* parameter_type_element = get_type(tokenizer_context, peek_token(tokenizer_context, str), str);
    // assert parameter_name_token is type string for type.

    if (peek_token(tokenizer_context, str)->type == TokComma) {
      consume(tokenizer_context, str, TokComma); // consume ,
    }

    VariableDeclarationAST* variable = (VariableDeclarationAST*)safe_malloc(sizeof(VariableDeclarationAST));
    variable->TYPE = AST_VariableDeclaration;
    variable->variable_name_token = parameter_name_token;
    variable->variable_type = parameter_type_element;
    variable->declaration = NULL;
    variable->access_modifier = AM_DEFAULT;

    if (parameters->variable_count == 0) {
      parameters->variable_declarations = (VariableDeclarationAST*)safe_malloc(sizeof(VariableDeclarationAST) * 1);
    }
    else {
      parameters->variable_declarations
	= (VariableDeclarationAST*)safe_realloc(parameters->variable_declarations, sizeof(VariableDeclarationAST) * (parameters->variable_count + 1));
    }

    parameters->variable_declarations[parameters->variable_count] = variable;
    parameters->variable_count++;
  }

  consume(tokenizer_context, str, TokRParen); // consume )

  return parameters;
}

void* create_constructor_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  ConstructorAST* constructor_ast = (ConstructorAST*)safe_malloc(sizeof(ConstructorAST));

  constructor_ast->TYPE = AST_Constructor;
  parser_context->current_function_name = L"constructor";
  constructor_ast->body_count = 0;
  constructor_ast->parameters = create_function_parameters(tokenizer_context, tok, str);

  consume(tokenizer_context, str, TokLBracket); // consume {

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    Token* body_token = peek_token(tokenizer_context, str);
    void* body_element = parse(tokenizer_context, parser_context, str);

    if(body_token->type == TokIdent){
      consume(tokenizer_context, str, TokSemiColon);
    }

    if (constructor_ast->body_count == 0) {
      constructor_ast->body = (void**)safe_malloc(sizeof(void*));
    }
    else {
      constructor_ast->body = (void**)safe_realloc(constructor_ast->body, sizeof(void*) * (constructor_ast->body_count + 1));
    }

    constructor_ast->body[constructor_ast->body_count] = body_element;

    constructor_ast->body_count++;
  }

  consume(tokenizer_context, str, TokRBracket); // consume }

  parser_context->current_function_name = L"";

  return constructor_ast;
}

void* create_new_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  NewAST* result = (NewAST*)safe_malloc(sizeof(NewAST));

  Token* class_name_token = pull_token(tokenizer_context, str);
  wchar_t* class_name = _wcsdup(class_name_token->str);

  FunctionCallParameterContext* function_call_parameter = parse_function_call_parameter(tokenizer_context, parser_context, str);

  result->TYPE = AST_New;
  result->parameter_count = function_call_parameter->parameter_count;
  result->parameters = function_call_parameter->parameters;
  result->class_name_token = class_name_token;

  return result;
}

void* create_null_ast(Token* tok, wchar_t* str) {
  NullAST* result = (NullAST*)safe_malloc(sizeof(NullAST));

  result->TYPE = AST_Null;

  return result;
}

void* create_array_declaration_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, wchar_t* str) {
  ArrayDeclarationAST* result = (ArrayDeclarationAST*)safe_malloc(sizeof(ArrayDeclarationAST));
  result->TYPE = AST_ArrayDeclaration;

  int element_count = 0;
  void** elements = NULL;

  while (peek_token(tokenizer_context, str)->type != TokRBracket) {
    elements = (void**)safe_realloc(elements, sizeof(void*) * (element_count + 1));
    void* element = parse_expression(tokenizer_context, parser_context, str);

    elements[element_count] = element;
    element_count++;

    Token* next_token = peek_token(tokenizer_context, str);

    if (next_token->type == TokLBracket) {
      break;
    }
    else if (next_token->type == TokComma) {
      consume(tokenizer_context, str, TokComma);
    }
  }

  result->element_count = element_count;
  result->elements = elements;
  consume(tokenizer_context, str, TokRBracket);

  return result;
}

void* create_bool_literal_ast(ParserContext* parser_context, Token* tok, const wchar_t* str) {
  BoolLiteralAST* literal = (BoolLiteralAST*)safe_malloc(sizeof(BoolLiteralAST));
  literal->TYPE = AST_BoolLiteral;
  literal->bool_type = wcscmp(tok->str, L"true") == 0;

  return literal;
}

void* create_neg_ast(TokenizerContext* tokenizer_context, ParserContext* parser_context, Token* tok, const wchar_t* str) {
  NegAST* neg = (NegAST*)safe_malloc(sizeof(NegAST));
  neg->TYPE = AST_Negative;
  neg->ast = parse_term(tokenizer_context, parser_context, str);
  return neg;
}

void* parse(TokenizerContext* tokenizer_context, ParserContext* parser_context, const wchar_t* str) {
  Token* tok = pull_token(tokenizer_context, str);

  switch ((TokenType)tok->type) {
    
  case TokSub:
    return create_neg_ast(tokenizer_context, parser_context, tok, str);

  case TokLBracket:
    return create_array_declaration_ast(tokenizer_context, parser_context, tok, str);

  case TokNew:
    return create_new_ast(tokenizer_context, parser_context, tok, str);

  case TokLParen:
    return create_paren_group_ast(tokenizer_context, parser_context, tok, str);

  case TokTrue:
  case TokFalse:
    return create_bool_literal_ast(parser_context, tok, str);

  case TokStringLiteral:
    return create_string_literal_ast(parser_context, tok, str);

  case TokNumberLiteral:
    return create_number_literal_ast(parser_context, tok, str);

  case TokReturn:
    return create_return_ast(tokenizer_context, parser_context, tok, str);

  case TokIdent:
    return create_identifier_ast(tokenizer_context, parser_context, tok, str, false);

  case TokFunc:
    return create_function_declaration_ast(tokenizer_context, parser_context, tok, str);

  case TokFor:
    return create_for_statement_ast(tokenizer_context, parser_context, tok, str);

  case TokIf:
    return create_if_statement_ast(tokenizer_context, parser_context, tok, str);

  case TokVar:
    return create_variable_declaration_ast(tokenizer_context, parser_context, tok, str);

  case TokClass:
    return create_class_ast(tokenizer_context, parser_context, tok, str);

  case TokPrivate:
  case TokPublic:
  case TokProtected: {
    void* element = parse(tokenizer_context, parser_context, str);
    wchar_t* access_modifier = AM_DEFAULT;

    switch (tok->type)
      {
      case TokPrivate:
	access_modifier = AM_PRIVATE;
	break;
      case TokProtected:
	access_modifier = AM_PROTECTED;
	break;
      case TokPublic:
	access_modifier = AM_PUBLIC;
	break;
      }

    switch (*((ASTType*)element)) {
    case AST_FunctionDeclaration:
      ((FunctionDeclarationAST*)element)->access_modifier = access_modifier;
      break;
    case AST_VariableDeclarationBundle: {
      VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)element;

      int i;
      for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
	variable_declaration_bundle_ast->variable_declarations[i]->access_modifier = access_modifier;
      }
      break;
    };
    case AST_VariableDeclaration: {
      VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)element;
      variable_declaration_ast->access_modifier = access_modifier;
      break;
    }
    }

    return element;
  }

  case TokConstructor: {
    return create_constructor_ast(tokenizer_context, parser_context, tok, str);
  }

  default: {
    handle_error(ER_UnexpectedToken, tok, parser_context->current_file_name, parser_context->file_str);
  }
  }
}
