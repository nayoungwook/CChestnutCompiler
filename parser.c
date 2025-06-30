#include "parser.h"

SymbolTable* function_symbol_table;
SymbolTable* type_symbol_table;
SymbolTable* variable_symbol_table;

Set* primitive_types;

int get_prev_variable_index_size() {
	SymbolTable* searcher_table = variable_symbol_table;

	int _size = 0;

	while (searcher_table->prev != NULL) {
		searcher_table = searcher_table->prev;
		_size += searcher_table->size;
	}

	return _size;
}

void insert_variable_symbol(const wchar_t* name, VariableData* data) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size++;

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = name;
	symbol->hash = _hash;
	symbol->next = variable_symbol_table->table[_hash];

	variable_symbol_table->table[_hash] = symbol;
}

void insert_set_symbol(Set* target_set, const wchar_t* str) {
	unsigned int _hash = hash(str);
	target_set->size++;

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = str;
	symbol->symbol = str;
	symbol->hash = _hash;
	symbol->next = target_set->table[_hash];

	target_set->table[_hash] = symbol;
}

void remove_variable_symbol(const wchar_t* name) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size--;
	Symbol* target_symbol = variable_symbol_table->table[_hash];
	variable_symbol_table->table[_hash] = variable_symbol_table->table[_hash]->next;
	free(target_symbol);
}

VariableData* create_variable_data(const wchar_t* type, const wchar_t* name) {
	VariableData* result = (VariableData*)malloc(sizeof(VariableData));
	result->type = (wchar_t*)malloc(sizeof(wchar_t) * 128);
	result->name = (wchar_t*)malloc(sizeof(wchar_t) * 256);

	wcscpy_s(result->type, 128, type);
	wcscpy_s(result->name, 256, name);
	result->index = variable_symbol_table->size + get_prev_variable_index_size() + 1;

	return result;
}

FunctionData* create_function_data(const wchar_t* name, const wchar_t* return_type, VariableDeclarationBundleAST* parameters) {
	FunctionData* result = (FunctionData*)malloc(sizeof(FunctionData));

	result->name = (wchar_t*)malloc(sizeof(wchar_t) * 256);
	result->return_type = (wchar_t*)malloc(sizeof(wchar_t) * 128);
	result->mangled_name = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
	result->generalized_mangled_name = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
	result->index = variable_symbol_table->size + get_prev_function_index_size() + 1;

	wcscpy_s(result->name, 128, name);
	wcscpy_s(result->return_type, 128, return_type);
	wcscpy_s(result->mangled_name, 1024, create_mangled_name(name, parameters));
	wcscpy_s(result->generalized_mangled_name, 1024, create_generalized_mangled_name(name, parameters));

	return result;
}

void open_scope() {
	SymbolTable* current_symbol_table = variable_symbol_table;

	SymbolTable* new_symbol_table = create_symbol_table();
	new_symbol_table->prev = current_symbol_table;
	new_symbol_table->size = 0;

	variable_symbol_table = new_symbol_table;
}

void close_scope() {
	SymbolTable* current_symbol_table = variable_symbol_table;
	variable_symbol_table = variable_symbol_table->prev;
	free(current_symbol_table);
}

void* parse_term(wchar_t* str) {
	void* node = parse(str);

	while (peek_token(str) && (peek_token(str)->type == TokMul || peek_token(str)->type == TokDiv)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse(str);

		enum OperatorType op_type = (op == TokMul) ? OpMUL : OpDIV;

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* parse_simple_expression(wchar_t* str) {
	void* node = parse_term(str);

	while (peek_token(str) && (peek_token(str)->type == TokAdd || peek_token(str)->type == TokSub)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_term(str);

		enum OperatorType op_type = (op == TokAdd) ? OpADD : OpSUB;

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* parse_unary_expression(wchar_t* str) {
	if (peek_token(str) && peek_token(str)->type == TokNot) {
		pull_token(str); // Consume '!'
		UnaryExprAST* unary_expr = (UnaryExprAST*)malloc(sizeof(UnaryExprAST));
		if (!unary_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		unary_expr->TYPE = AST_UnaryExpr;
		unary_expr->expr = parse_unary_expression(str);
		return unary_expr;
	}
	return parse_simple_expression(str);
}

void* parse_compare_expression(wchar_t* str) {
	void* node = parse_unary_expression(str);

	while (peek_token(str) && (peek_token(str)->type == TokEqual || peek_token(str)->type == TokNotEqual ||
		peek_token(str)->type == TokGreater || peek_token(str)->type == TokLesser ||
		peek_token(str)->type == TokEqualGreater || peek_token(str)->type == TokEqualLesser)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_unary_expression(str);

		enum OperatorType op_type;
		switch (op) {
		case TokEqual: op_type = OpEQUAL; break;
		case TokNotEqual: op_type = OpNOTEQUAL; break;
		case TokGreater: op_type = OpGREATER; break;
		case TokLesser: op_type = OpLESSER; break;
		case TokEqualGreater: op_type = OpEQUALGREATER; break;
		case TokEqualLesser: op_type = OpEQUALLESSER; break;
		}

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* parse_expression(wchar_t* str) {
	void* node = parse_compare_expression(str);

	while (peek_token(str) && (peek_token(str)->type == TokOr || peek_token(str)->type == TokAnd)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_compare_expression(str);

		enum OperatorType op_type;
		switch (op) {
		case TokOr: op_type = OpOR; break;
		case TokAnd: op_type = OpAND; break;
		}

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* consume(wchar_t* str, TokenType expected_type) {
	Token* tok = pull_token(str);

	if (tok->type != expected_type) {
		// Throw error.
		printf("Unexpected token : %d, expected : %d\n", tok->type, expected_type);
	}
}

void* create_if_statement_ast(Token* tok, wchar_t* str) {
	IfStatementAST* if_statement = (IfStatementAST*)malloc(sizeof(IfStatementAST));

	IfType if_type = StmtIf;

	if (peek_token(str)->type == TokElse) {
		consume(str, TokElse);

		if (peek_token(str)->type == TokIf) {
			if_type = StmtElseIf;

			consume(str, TokIf);
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
		consume(str, TokLParen); // consume (

		void* condition = parse_expression(str);

		consume(str, TokRParen); // consume )

		if_statement->condition = condition;
	}

	consume(str, TokLBracket); // consume {

	open_scope();

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse_expression(str);

		if (if_statement->body_count == 0) {
			if_statement->body = (void**)malloc(sizeof(void*));
		}
		else {
			if_statement->body = (void**)realloc(if_statement->body, sizeof(void*) * (if_statement->body_count + 1));
		}

		if_statement->body[if_statement->body_count] = body_element;

		if_statement->body_count++;
	}

	consume(str, TokRBracket); // consume }

	close_scope();

	if (peek_token(str)->type == TokElse) {
		if_statement->next_statement = create_if_statement_ast(tok, str);
	}

	return if_statement;
}

void* create_paren_group_ast(Token* tok, wchar_t* str) {
	void* node = parse_expression(str);

	consume(str, TokRParen); // consume )

	return node;
}

void* create_string_literal_ast(Token* tok, wchar_t* str) {
	StringLiteralAST* literal = (StringLiteralAST*)malloc(sizeof(StringLiteralAST));
	literal->TYPE = AST_StringLiteral;
	literal->string_literal = tok->str;

	return literal;
}

void* create_number_literal_ast(Token* tok, wchar_t* str) {
	NumberLiteralAST* literal = (NumberLiteralAST*)malloc(sizeof(NumberLiteralAST));
	literal->TYPE = AST_NumberLiteral;
	literal->number_literal = tok->str;
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

void* create_return_ast(Token* tok, wchar_t* str) {
	void* result = NULL;
	void* expression = parse_expression(str);

	result = (ReturnAST*)malloc(sizeof(ReturnAST));
	((ReturnAST*)result)->expression = expression;
	((ReturnAST*)result)->TYPE = AST_Return;

	consume(str, TokSemiColon);

	return result;
}

wchar_t* infer_type(void* ast) {
	ASTType type = *((ASTType*)(ast));
	switch (type) {
	case AST_NumberLiteral:
		NumberLiteralAST* number_ast = (NumberLiteralAST*)ast;
		return number_ast->numeric_type;
	case AST_StringLiteral:
		return L"string";
	case AST_Identifier:
		IdentifierAST* ident_ast = (IdentifierAST*)ast;
		VariableData* data = (VariableData*)find_symbol(variable_symbol_table, ident_ast->identifier)->data;
		return data->type;
	}
}

void* create_function_call_ast(Token* tok, wchar_t* str) {
	void* result = (FunctionCallAST*)malloc(sizeof(FunctionCallAST));
	((FunctionCallAST*)result)->TYPE = AST_FunctionCall;
	((FunctionCallAST*)result)->parameter_count = 0;

	consume(str, TokLParen);

	while (peek_token(str)->type != TokRParen) {
		void* parameter = parse_expression(str);

		if (((FunctionCallAST*)result)->parameter_count == 0) {
			((FunctionCallAST*)result)->parameters = (void**)malloc(sizeof(void*));
		}
		else {
			((FunctionCallAST*)result)->parameters = (void**)realloc(((FunctionCallAST*)result)->parameters, sizeof(void*) * (((FunctionCallAST*)result)->parameter_count + 1));
		}

		((FunctionCallAST*)result)->parameters[((FunctionCallAST*)result)->parameter_count] = parameter;
		((FunctionCallAST*)result)->parameter_count++;

		printf("inferenced type : %S\n", infer_type(parameter));

		if (peek_token(str)->type == TokComma) {
			consume(str, TokComma);
		}
	}

	consume(str, TokRParen);

	if (peek_token(str)->type == TokSemiColon) {
		consume(str, TokSemiColon);
	}

	((FunctionCallAST*)result)->function_name = tok->str;
	return result;
}

void* create_identifier_ast(Token* tok, wchar_t* str) {
	void* result = NULL;

	Token* next_token = peek_token(str);
	if (next_token->type == TokLParen) { // identifier ( 
		result = create_function_call_ast(tok, str);
	}
	else {
		if (next_token->type == TokIncrease || next_token->type == TokDecrease) {
			if (next_token->type == TokIncrease) {
				consume(str, TokIncrease);
				result = (IdentIncreaseAST*)malloc(sizeof(IdentIncreaseAST));
				((IdentIncreaseAST*)result)->identifier = tok->str;
				((IdentIncreaseAST*)result)->TYPE = AST_IdentIncrease;
			}
			if (next_token->type == TokDecrease) {
				consume(str, TokDecrease);
				result = (IdentDecreaseAST*)malloc(sizeof(IdentDecreaseAST));
				((IdentDecreaseAST*)result)->identifier = tok->str;
				((IdentDecreaseAST*)result)->TYPE = AST_IdentDecrease;
			}

			if (peek_token(str)->type == TokSemiColon) {
				consume(str, TokSemiColon);
			}
		}
		else {
			result = (IdentifierAST*)malloc(sizeof(IdentifierAST));
			((IdentifierAST*)result)->TYPE = AST_Identifier;
			((IdentifierAST*)result)->identifier = tok->str;

			// check for assign
			if (peek_token(str)->type == TokAssign) {
				consume(str, TokAssign);
				void* right = parse_unary_expression(str);

				BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
				if (!bin_expr) {
					fprintf(stderr, "Memory allocation failed\n");
					exit(1);
				}
				bin_expr->TYPE = AST_BinExpr;
				bin_expr->left = result;
				bin_expr->right = right;
				bin_expr->opType = OpASSIGN;

				result = bin_expr;
			}
		}

		VariableData* data = find_symbol(variable_symbol_table, tok->str)->data;
		((IdentifierAST*)result)->index = data->index;
	}

	return result;
}

int check_castability(const wchar_t* from, const wchar_t* to) {
	int primitive_count = 0;

	if (is_primitive_type(from)) primitive_count++;
	if (is_primitive_type(to)) primitive_count++;

	if (primitive_count == 2) { // both are primitive type.
		return 1;
	}
	else if (primitive_count == 1) { // only one type is primitive type.
		return 0;
	}

	// check for the castability between classes.
	Symbol* from_symbol = find_symbol(type_symbol_table, from);
	Symbol* to_symbol = find_symbol(type_symbol_table, to);

	if (from_symbol) { // are both classes?
		Type* from_type = ((Type*)from_symbol->data);
		Type* to_type = ((Type*)to_symbol->data);
		while (1) {
			// to -> next search find from.
			// up casting.

			if (!strcmp(from_type->type_str, to_type->type_str)) return 1;

			if (!from_type->parent_type) {
				return 0;
			}

			from_type = from_type->parent_type;
		}
		return 1;
	}
	else {
		printf("[Temporary error] Error at util.c %s is not a class.\n");
		return 0;
	}

	return 0;
}

Symbol* create_type_symbol(const wchar_t* type_str, Type* type) {
	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));

	unsigned int _hash = hash(type_str);

	symbol->data = type;
	symbol->symbol = type_str;
	symbol->hash = _hash;
}

void insert_type_symbol(Type* target_type, const wchar_t* type_str) {
	Type* child = (Type*)malloc(sizeof(Type));
	child->type_str = type_str;
	child->parent_type = target_type;
	child->child_types = NULL;

	Symbol* child_symbol = create_type_symbol(type_str, child);
	child_symbol->next = type_symbol_table->table[child_symbol->hash];
	type_symbol_table->table[child_symbol->hash] = child_symbol;

	if (target_type) {
		if (!target_type->child_types) {
			target_type->child_types = create_symbol_table();
		}

		Symbol* child_symbol = create_type_symbol(type_str, child);
		child_symbol->next = target_type->child_types->table[child_symbol->hash];
		target_type->child_types->table[child_symbol->hash] = child_symbol;
		target_type->child_types->size++;
	}
}

void remove_type_symbol(const wchar_t* type_str) {
	unsigned int _hash = hash(type_str);
	type_symbol_table->size--;

	Symbol* target_symbol = type_symbol_table->table[_hash];
	type_symbol_table->table[_hash] = type_symbol_table->table[_hash]->next;
}

int get_prev_function_index_size() { // search for inheritation
	int _size = 0;

	return _size;
}

void insert_function_symbol(FunctionDeclarationAST* ast) {
	FunctionData* data = create_function_data(ast->function_name, ast->return_type, ast->parameters);
	unsigned int _hash = hash(data->mangled_name);

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = data->mangled_name;
	symbol->hash = _hash;
	function_symbol_table->size++;
	function_symbol_table->table[_hash] = symbol;
}

void remove_function_symbol(const wchar_t* mangled_name) {
	unsigned int _hash = hash(mangled_name);
	function_symbol_table->size--;

	Symbol* target_symbol = function_symbol_table->table[_hash];
	function_symbol_table->table[_hash] = function_symbol_table->table[_hash]->next;
	free(target_symbol);
}

void* create_function_declaration_ast(Token* tok, wchar_t* str) {
	// func add(a: int, b: int): int {}
	FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)malloc(sizeof(FunctionDeclarationAST));
	function_declaration_ast->TYPE = AST_FunctionDeclaration;
	function_declaration_ast->body_count = 0;

	wchar_t* function_name = pull_token(str)->str;
	function_declaration_ast->function_name = function_name;

	VariableDeclarationBundleAST* parameters = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
	parameters->variable_count = 0;
	parameters->variable_declarations = NULL;

	consume(str, TokLParen); // consume (

	open_scope();

	while (peek_token(str)->type != TokRParen) {
		Token* parameter_name_token = pull_token(str);
		wchar_t* parameter_name = parameter_name_token->str;
		// assert parameter_name_token is type identifier

		consume(str, TokColon); // consume :

		Token* parameter_type_token = pull_token(str);
		wchar_t* parameter_type = parameter_type_token->str;
		// assert parameter_name_token is type string for type.

		if (peek_token(str)->type == TokComma) {
			consume(str, TokComma); // consume ,
		}

		VariableDeclarationAST* variable = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST));
		variable->TYPE = AST_VariableDeclaration;
		variable->variable_name = parameter_name;
		variable->variable_type = parameter_type;
		variable->declaration = NULL;

		VariableData* variable_data = create_variable_data(parameter_type, parameter_name);
		insert_variable_symbol(parameter_name, variable_data);
		variable->index = variable_data->index;

		if (parameters->variable_count == 0) {
			parameters->variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
		}
		else {
			parameters->variable_declarations
				= (VariableDeclarationAST*)realloc(parameters->variable_declarations, sizeof(VariableDeclarationAST) * (parameters->variable_count + 1));
		}

		parameters->variable_declarations[parameters->variable_count] = variable;
		parameters->variable_count++;

	}

	function_declaration_ast->parameters = parameters;

	consume(str, TokRParen); // consume )
	consume(str, TokColon); // consume :

	wchar_t* return_type = pull_token(str)->str;
	function_declaration_ast->return_type = return_type;

	consume(str, TokLBracket); // consume {

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse(str);

		if (function_declaration_ast->body_count == 0) {
			function_declaration_ast->body = (void**)malloc(sizeof(void*));
		}
		else {
			function_declaration_ast->body = (void**)realloc(function_declaration_ast->body, sizeof(void*) * (function_declaration_ast->body_count + 1));
		}

		function_declaration_ast->body[function_declaration_ast->body_count] = body_element;

		function_declaration_ast->body_count++;
	}

	close_scope();

	consume(str, TokRBracket); // consume }

	insert_function_symbol(function_declaration_ast);

	return function_declaration_ast;
}

void* create_for_statement_ast(Token* tok, wchar_t* str) {
	ForStatementAST* for_statement = (ForStatementAST*)malloc(sizeof(ForStatementAST));

	for_statement->TYPE = AST_ForStatement;
	for_statement->body_count = 0;

	consume(str, TokLParen); // consume (

	open_scope();

	void* init = parse_expression(str);
	void* condition = parse_expression(str);

	if (peek_token(str)->type == TokSemiColon)
		pull_token(str);

	void* step = parse_expression(str);

	consume(str, TokRParen); // consume )

	for_statement->init = init;
	for_statement->condition = condition;
	for_statement->step = step;

	consume(str, TokLBracket); // consume {

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse_expression(str);

		if (for_statement->body_count == 0) {
			for_statement->body = (void**)malloc(sizeof(void*));
		}
		else {
			for_statement->body = (void**)realloc(for_statement->body, sizeof(void*) * (for_statement->body_count + 1));
		}

		for_statement->body[for_statement->body_count] = body_element;

		for_statement->body_count++;
	}

	consume(str, TokRBracket); // consume }

	close_scope();

	return for_statement;
}

void* create_variable_declaration_ast(Token* tok, wchar_t* str) {
	// var i: int;
	VariableDeclarationBundleAST* bundles = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
	bundles->TYPE = AST_VariableDeclarationBundle;
	bundles->variable_count = 0;
	bundles->variable_declarations = NULL;

	while (1) {
		Token* name_token = pull_token(str);
		wchar_t* name = name_token->str;

		consume(str, TokColon);

		wchar_t* type = pull_token(str)->str;

		tok = pull_token(str);

		void* declaration = NULL;

		if (tok->type == TokSemiColon) {
			break;
		}
		else if (tok->type == TokComma) {
		}
		else if (tok->type == TokAssign) {
			declaration = parse_expression(str);
		}

		VariableDeclarationAST* variable = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST));
		variable->TYPE = AST_VariableDeclaration;
		variable->variable_name = name;
		variable->variable_type = type;
		variable->declaration = declaration;

		if (bundles->variable_count == 0) {
			bundles->variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
		}
		else {
			bundles->variable_declarations
				= (VariableDeclarationAST*)realloc(bundles->variable_declarations, sizeof(VariableDeclarationAST) * (bundles->variable_count + 1));
		}

		VariableData* data = create_variable_data(type, name);
		insert_variable_symbol(name, data);

		variable->index = data->index;

		bundles->variable_declarations[bundles->variable_count] = variable;
		bundles->variable_count++;

		Token* tok = pull_token(str);
		if (tok->type == TokSemiColon) {
			break;
		}
	}
	return bundles;
}

void* parse(wchar_t* str) {
	Token* tok = pull_token(str);

	switch (tok->type) {

	case TokLParen:
		return create_paren_group_ast(tok, str);

	case TokStringLiteral:
		return create_string_literal_ast(tok, str);

	case TokNumberLiteral:
		return create_number_literal_ast(tok, str);

	case TokReturn:
		return create_return_ast(tok, str);

	case TokIdent:
		return create_identifier_ast(tok, str);

	case TokFunc:
		return create_function_declaration_ast(tok, str);

	case TokFor:
		return create_for_statement_ast(tok, str);

	case TokIf:
		return create_if_statement_ast(tok, str);

	case TokVar:
		return create_variable_declaration_ast(tok, str);
	}
}