#include "includes/parser.h"

SymbolTable* function_symbol_table;
SymbolTable* class_symbol_table;
SymbolTable* class_hierarchy;
SymbolTable* variable_symbol_table;

Set* primitive_types;

static wchar_t* current_class = L"";
static wchar_t* current_function_name = L"";

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

FunctionData* create_function_data(SymbolTable* function_symbol_table, const wchar_t* name, Type* return_type, VariableDeclarationBundleAST* parameters) {
	FunctionData* result = (FunctionData*)malloc(sizeof(FunctionData));

	result->name = _wcsdup(name);
	result->return_type = return_type;
	result->index = function_symbol_table->size + 1;
	result->access_modifier = L"default";
	result->parameter_types = NULL;
	result->parameter_count = parameters->variable_count;

	int i;
	for (i = 0; i < parameters->variable_count; i++) {
		result->parameter_types = (Type**)realloc(result->parameter_types, sizeof(Type*) * (i + 1));

		VariableDeclarationAST* variable_declaration = parameters->variable_declarations[i];
		result->parameter_types[i] = variable_declaration->variable_type;
	}

	return result;
}

static int class_count = 0;

ClassData* create_class_data(ClassAST* class_ast) {
	ClassData* result = (ClassData*)malloc(sizeof(ClassData));
	class_count++;

	result->name = _wcsdup(class_ast->class_name);
	result->parent_class_name = _wcsdup(class_ast->parent_class_name);
	result->index = class_count;

	result->member_variables = create_symbol_table();
	result->member_functions = create_symbol_table();

	return result;
}

void insert_class_symbol(ClassAST* ast) {
	ClassData* data = create_class_data(ast);
	unsigned int _hash = hash(data->name);

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = ast->class_name;
	symbol->hash = _hash;
	class_symbol_table->size++;
	class_symbol_table->table[_hash] = symbol;
}

void remove_class_symbol(const wchar_t* name) {
	unsigned int _hash = hash(name);
	class_symbol_table->size--;

	Symbol* target_symbol = class_symbol_table->table[_hash];
	class_symbol_table->table[_hash] = class_symbol_table->table[_hash]->next;
	free(target_symbol);
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

void* create_function_call_ast(Token* tok, wchar_t* str) {
	FunctionCallAST* result = (FunctionCallAST*)malloc(sizeof(FunctionCallAST));
	wchar_t* function_name = _wcsdup(tok->str);
	result->function_name = function_name;
	result->TYPE = AST_FunctionCall;
	result->parameter_count = 0;
	consume(str, TokLParen);

	while (peek_token(str)->type != TokRParen) {
		void* parameter = parse_expression(str);

		if (result->parameter_count == 0) {
			result->parameters = (void**)malloc(sizeof(void*));
		}
		else {
			result->parameters = (void**)realloc(result->parameters, sizeof(void*) * (result->parameter_count + 1));
		}

		int index = result->parameter_count;
		result->parameters[index] = parameter;

		if (peek_token(str)->type == TokComma) {
			consume(str, TokComma);
		}

		result->parameter_count++;
	}

	consume(str, TokRParen);

	result->function_name = tok->str;
	return result;
}

void* create_array_access_ast(void* target_array, Token* tok, wchar_t* str) {
	ArrayAccessAST* array_access_ast = (ArrayAccessAST*)malloc(sizeof(ArrayAccessAST));
	array_access_ast->TYPE = AST_ArrayAccess;
	array_access_ast->access_count = 0;
	array_access_ast->indexes = NULL;

	int access_count = 0;
	void** indexes = NULL;

	while (peek_token(str)->type == TokLSquareBracket) {
		consume(str, TokLSquareBracket);
		void* index = parse_expression(str);
		consume(str, TokRSquareBracket);

		indexes = (void**)realloc(indexes, (access_count + 1) * sizeof(void*));
		indexes[access_count] = index;

		access_count++;
	}

	array_access_ast->indexes = indexes;

	array_access_ast->target_array = target_array;
	array_access_ast->access_count = access_count;

	return array_access_ast;
}

void* create_identifier_ast(Token* tok, wchar_t* str, int is_attribute_identifier) {
	void* result = NULL;

	Token* next_token = peek_token(str);
	if (next_token->type == TokLParen) { // identifier ( 
		result = create_function_call_ast(tok, str);

		((FunctionCallAST*)result)->attribute = NULL;

		if (peek_token(str)->type == TokDot) {
			consume(str, TokDot);
			((FunctionCallAST*)result)->attribute = create_identifier_ast(pull_token(str), str, 1);
		}

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

			((IdentifierAST*)result)->attribute = NULL;

			if (peek_token(str)->type == TokDot) {
				consume(str, TokDot);
				((IdentifierAST*)result)->attribute = create_identifier_ast(pull_token(str), str, 1);
			}
		}
	}

	if (peek_token(str)->type == TokLSquareBracket) {
		result = create_array_access_ast(result, tok, str);

		((ArrayAccessAST*)result)->attribute = NULL;
		if (peek_token(str)->type == TokDot) {
			consume(str, TokDot);
			((ArrayAccessAST*)result)->attribute = create_identifier_ast(pull_token(str), str, 1);
		}
	}

	if (!is_attribute_identifier && peek_token(str)->type == TokAssign) {
		consume(str, TokAssign);

		void* right_term = parse_expression(str);

		BinExprAST* bin_expr_ast = (BinExprAST*)malloc(sizeof(BinExprAST));
		bin_expr_ast->TYPE = AST_BinExpr;
		bin_expr_ast->left = result;
		bin_expr_ast->right = right_term;
		bin_expr_ast->opType = OpASSIGN;

		consume(str, TokSemiColon);

		return bin_expr_ast;
	}
	return result;
}

int is_same_type(Type* t1, Type* t2) {
	if (!wcscmp(t1->type_str, t2->type_str)) {
		if (!wcscmp(t1->type_str, L"array")) {
			return is_same_type(t1->array_element_type, t2->array_element_type);
		}

		return 1;
	}
	else {
		// handle error
		printf("[Temporary error] Error at parser.c you can\'t cast between %S and %S.\n", t1->type_str, t2->type_str);

		return 0;
	}
}

int check_castability(Type* from, Type* to) {
	int primitive_count = 0;

	if (is_primitive_type(from)) primitive_count++;
	if (is_primitive_type(to)) primitive_count++;

	if (primitive_count == 2) { // both are primitive type.
		return 1;
	}
	else if (primitive_count == 1) { // only one type is primitive type.
		return 0;
	}

	// check for the castability between non primitives.
	Symbol* from_symbol = find_symbol(class_hierarchy, from->type_str);
	Symbol* to_symbol = find_symbol(class_hierarchy, to->type_str);

	if (from_symbol) { // are they classes?
		ClassType* from_type = ((ClassType*)from_symbol->data);
		ClassType* to_type = ((ClassType*)to_symbol->data);
		while (1) {
			// to -> next search find from.
			// up casting.
			if (is_same_type(from, to)) {
				return 1;
			}

			if (!from_type->parent_type) {
				return 0;
			}

			from_type = from_type->parent_type;
		}
		return 1;
	}
	else {
		if (!wcscmp(from->type_str, L"null")) { // null to class
			if (to_symbol) {
				return 1;
			}
		}

		if (is_same_type(from, to))return 1;

		printf("[Temporary error] Error at parser.c you can\'t cast between %S and %S.\n", from->type_str, to->type_str);
		return 0;
	}

	return 0;
}

Symbol* create_type_symbol(const wchar_t* type_str, ClassType* type) {
	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));

	unsigned int _hash = hash(type_str);

	symbol->data = type;
	symbol->symbol = type_str;
	symbol->hash = _hash;
}

void insert_type_symbol(ClassType* target_type, const wchar_t* type_str) {
	ClassType* child = (ClassType*)malloc(sizeof(ClassType));
	child->type_str = type_str;
	child->parent_type = target_type;
	child->child_types = NULL;

	Symbol* child_symbol = create_type_symbol(type_str, child);
	child_symbol->next = class_hierarchy->table[child_symbol->hash];
	class_hierarchy->table[child_symbol->hash] = child_symbol;

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
	class_hierarchy->size--;

	Symbol* target_symbol = class_hierarchy->table[_hash];
	class_hierarchy->table[_hash] = class_hierarchy->table[_hash]->next;
}

void insert_function_symbol(SymbolTable* function_symbol_table, FunctionDeclarationAST* ast) {
	FunctionData* data = create_function_data(function_symbol_table, ast->function_name, ast->return_type, ast->parameters);
	unsigned int _hash = hash(data->name);

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = ast->function_name;
	symbol->hash = _hash;
	function_symbol_table->size++;
	function_symbol_table->table[_hash] = symbol;
}

void remove_function_symbol(SymbolTable* function_symbol_table, const wchar_t* mangled_name) {
	unsigned int _hash = hash(mangled_name);
	function_symbol_table->size--;

	Symbol* target_symbol = function_symbol_table->table[_hash];
	function_symbol_table->table[_hash] = function_symbol_table->table[_hash]->next;
	free(target_symbol);
}

Type* get_type(Token* tok, wchar_t* str) {
	wchar_t* type = pull_token(str)->str;

	Type* result = (Type*)malloc(sizeof(Type));
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

void* create_function_declaration_ast(Token* tok, wchar_t* str) {
	// func add(a: int, b: int): int {}
	FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)malloc(sizeof(FunctionDeclarationAST));
	function_declaration_ast->TYPE = AST_FunctionDeclaration;
	function_declaration_ast->body_count = 0;
	function_declaration_ast->access_modifier = L"default";

	wchar_t* function_name = pull_token(str)->str;
	function_declaration_ast->function_name = function_name;

	current_function_name = _wcsdup(function_name);

	VariableDeclarationBundleAST* parameters = create_function_parameters(tok, str);
	function_declaration_ast->parameters = parameters;

	consume(str, TokColon); // consume :

	Type* return_type_element = get_type(tok, str);

	function_declaration_ast->return_type = return_type_element;

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

	consume(str, TokRBracket); // consume }

	if (!wcscmp(current_class, L"")) {
		insert_function_symbol(function_symbol_table, function_declaration_ast);
	}
	else {
		SymbolTable* member_function_symbol_table = ((ClassData*)find_symbol(class_symbol_table, current_class)->data)->member_functions;
		insert_function_symbol(member_function_symbol_table, function_declaration_ast);
	}
	current_function_name = L"";

	return function_declaration_ast;
}

void* create_for_statement_ast(Token* tok, wchar_t* str) {
	ForStatementAST* for_statement = (ForStatementAST*)malloc(sizeof(ForStatementAST));

	for_statement->TYPE = AST_ForStatement;
	for_statement->body_count = 0;

	consume(str, TokLParen); // consume (

	void* init = parse_expression(str);
	consume(str, TokSemiColon);

	void* condition = parse_expression(str);
	consume(str, TokSemiColon);

	void* step = parse_expression(str);

	consume(str, TokRParen); // consume )

	for_statement->init = init;
	for_statement->condition = condition;
	for_statement->step = step;

	consume(str, TokLBracket); // consume {

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse(str);

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

		wchar_t* type_element = get_type(tok, str);

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

		variable->variable_type = type_element;

		variable->declaration = declaration;
		variable->access_modifier = L"default";

		if (bundles->variable_count == 0) {
			bundles->variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
		}
		else {
			bundles->variable_declarations
				= (VariableDeclarationAST*)realloc(bundles->variable_declarations, sizeof(VariableDeclarationAST) * (bundles->variable_count + 1));
		}

		bundles->variable_declarations[bundles->variable_count] = variable;
		bundles->variable_count++;

		Token* tok = peek_token(str);
		if (tok->type == TokSemiColon) {
			break;
		}
	}

	return bundles;
}

void* create_class_ast(Token* tok, wchar_t* str) {
	/*
	class name extends parent {
		constructor(){
		}

		public var i: float = 4.5f;

		private func test(): void {
		}
	}
	*/

	wchar_t* class_name = pull_token(str)->str;
	wchar_t* parent_name = L"";

	if (peek_token(str)->type == TokExtends) {
		consume(str, TokExtends);
		parent_name = pull_token(str)->str;
	}

	ClassAST* class_ast = (ClassAST*)malloc(sizeof(ClassAST));

	class_ast->TYPE = AST_Class;
	class_ast->class_name = class_name;
	class_ast->parent_class_name = parent_name;
	class_ast->constructor = NULL;

	class_ast->member_function_count = 0;
	class_ast->member_variable_bundle_count = 0;

	// initialize for constructor
	class_ast->constructor = (ConstructorAST*)malloc(sizeof(ConstructorAST));
	class_ast->constructor->TYPE = AST_Constructor;
	class_ast->constructor->body_count = 0;
	VariableDeclarationBundleAST* empty_parameters = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
	empty_parameters->variable_count = 0;
	empty_parameters->variable_declarations = NULL;
	class_ast->constructor->parameters = empty_parameters;

	insert_type_symbol(!wcscmp(parent_name, L"") ? NULL : parent_name, class_name);
	insert_class_symbol(class_ast);

	current_class = class_ast->class_name;

	consume(str, TokLBracket);

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse(str);
		ASTType body_type = *((ASTType*)body_element);

		switch (body_type) {
		case AST_FunctionDeclaration:
			class_ast->member_function_count++;

			if (class_ast->member_function_count == 1) {
				class_ast->member_functions = (void**)malloc(sizeof(void*));
			}
			else {
				class_ast->member_functions = (void**)realloc(class_ast->member_functions, class_ast->member_function_count * sizeof(void*));
			}

			class_ast->member_functions[class_ast->member_function_count - 1] = body_element;

			break;
		case AST_VariableDeclarationBundle:
			class_ast->member_variable_bundle_count++;

			if (class_ast->member_variable_bundle_count == 1) {
				class_ast->member_variables = (void**)malloc(sizeof(void*));
			}
			else {
				class_ast->member_variables = (void**)realloc(class_ast->member_variables, class_ast->member_variable_bundle_count * sizeof(void*));
			}

			class_ast->member_variables[class_ast->member_variable_bundle_count - 1] = body_element;

			break;
		case AST_Constructor:
			class_ast->constructor = body_element;

			break;
		}
	}

	ClassData* class_data = ((ClassData*)find_symbol(class_symbol_table, current_class)->data);
	Type* constructor_return_type_element = (Type*)malloc(sizeof(Type));
	constructor_return_type_element->type_str = L"void";
	constructor_return_type_element->is_array = 0;
	constructor_return_type_element->array_element_type = NULL;
	class_data->constructor_data = create_function_data(class_data->member_functions, L"constructor", constructor_return_type_element, class_ast->constructor->parameters);

	current_class = L"";

	consume(str, TokRBracket);

	return class_ast;
}

VariableDeclarationBundleAST* create_function_parameters(Token* tok, wchar_t* str) {
	consume(str, TokLParen); // consume (

	VariableDeclarationBundleAST* parameters = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
	parameters->variable_count = 0;
	parameters->variable_declarations = NULL;

	while (peek_token(str)->type != TokRParen) {
		Token* parameter_name_token = pull_token(str);
		wchar_t* parameter_name = parameter_name_token->str;
		// assert parameter_name_token is type identifier

		consume(str, TokColon); // consume :

		Type* parameter_type_element = get_type(peek_token(str), str);
		// assert parameter_name_token is type string for type.

		if (peek_token(str)->type == TokComma) {
			consume(str, TokComma); // consume ,
		}

		VariableDeclarationAST* variable = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST));
		variable->TYPE = AST_VariableDeclaration;
		variable->variable_name = parameter_name;
		variable->variable_type = parameter_type_element;
		variable->declaration = NULL;
		variable->access_modifier = L"default";

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

	consume(str, TokRParen); // consume )

	return parameters;
}

void* create_constructor_ast(Token* tok, wchar_t* str) {
	ConstructorAST* constructor_ast = (ConstructorAST*)malloc(sizeof(ConstructorAST));

	constructor_ast->TYPE = AST_Constructor;
	current_function_name = L"constructor";
	constructor_ast->body_count = 0;
	constructor_ast->parameters = create_function_parameters(tok, str);

	consume(str, TokLBracket); // consume {
	open_scope();

	while (peek_token(str)->type != TokRBracket) {
		void* body_element = parse(str);

		if (constructor_ast->body_count == 0) {
			constructor_ast->body = (void**)malloc(sizeof(void*));
		}
		else {
			constructor_ast->body = (void**)realloc(constructor_ast->body, sizeof(void*) * (constructor_ast->body_count + 1));
		}

		constructor_ast->body[constructor_ast->body_count] = body_element;

		constructor_ast->body_count++;
	}

	consume(str, TokRBracket); // consume }
	close_scope();

	current_function_name = L"";

	return constructor_ast;
}

void* create_new_ast(Token* tok, wchar_t* str) {
	NewAST* result = (NewAST*)malloc(sizeof(NewAST));

	wchar_t* class_name = _wcsdup(pull_token(str)->str);

	result->TYPE = AST_New;
	result->parameter_count = 0;
	result->class_name = class_name;

	ClassData* class_data = find_symbol(class_symbol_table, class_name);

	consume(str, TokLParen);

	while (peek_token(str)->type != TokRParen) {
		void* parameter = parse_expression(str);

		if (result->parameter_count == 0) {
			result->parameters = (void**)malloc(sizeof(void*));
		}
		else {
			result->parameters = (void**)realloc(result->parameters, sizeof(void*) * (result->parameter_count + 1));
		}

		int index = result->parameter_count;
		result->parameters[index] = parameter;

		if (peek_token(str)->type == TokComma) {
			consume(str, TokComma);
		}

		result->parameter_count++;
	}

	consume(str, TokRParen);

	return result;
}

void* create_null_ast(Token* tok, wchar_t* str) {
	NullAST* result = (NullAST*)malloc(sizeof(NullAST));

	result->TYPE = AST_Null;

	return result;
}

void* create_array_declaration_ast(Token* tok, wchar_t* str) {
	ArrayDeclarationAST* result = (ArrayDeclarationAST*)malloc(sizeof(ArrayDeclarationAST));
	result->TYPE = AST_ArrayDeclaration;

	int element_count = 0;
	void** elements = NULL;

	while (peek_token(str)->type != TokRBracket) {
		elements = (void**)realloc(elements, sizeof(void*) * (element_count + 1));
		void* element = parse(str);

		elements[element_count] = element;
		element_count++;

		Token* next_token = peek_token(str);

		if (next_token->type == TokLBracket) {
			break;
		}
		else if (next_token->type == TokComma) {
			consume(str, TokComma);
		}
	}

	result->element_count = element_count;
	result->elements = elements;
	consume(str, TokRBracket);

	return result;
}

void* parse(const wchar_t* str) {
	Token* tok = pull_token(str);

	switch (tok->type) {

	case TokLBracket:
		return create_array_declaration_ast(tok, str);

	case TokNew:
		return create_new_ast(tok, str);

	case TokLParen:
		return create_paren_group_ast(tok, str);

	case TokStringLiteral:
		return create_string_literal_ast(tok, str);

	case TokNumberLiteral:
		return create_number_literal_ast(tok, str);

	case TokReturn:
		return create_return_ast(tok, str);

	case TokIdent:
		return create_identifier_ast(tok, str, 0);

	case TokFunc:
		return create_function_declaration_ast(tok, str);

	case TokFor:
		return create_for_statement_ast(tok, str);

	case TokIf:
		return create_if_statement_ast(tok, str);

	case TokVar:
		return create_variable_declaration_ast(tok, str);

	case TokClass:
		return create_class_ast(tok, str);

	case TokPrivate:
	case TokPublic:
	case TokProtected: {
		void* element = parse(str);
		wchar_t* access_modifier = L"default";

		switch (tok->type)
		{
		case TokPrivate:
			access_modifier = L"private";
			break;
		case TokProtected:
			access_modifier = L"protected";
			break;
		case TokPublic:
			access_modifier = L"public";
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
		return create_constructor_ast(tok, str);
	}
	}
}