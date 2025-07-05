#include "includes/ir.h"

extern SymbolTable* variable_symbol_table;
extern SymbolTable* function_symbol_table;
extern SymbolTable* type_symbol_table;
extern SymbolTable* class_symbol_table;
extern Set* primitive_types;
static int label_id = 0;

static int is_class_initializer = 0;
static wchar_t* current_class = L"";

void insert_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name, VariableData* data) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size++;

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = name;
	symbol->hash = _hash;
	symbol->next = variable_symbol_table->table[_hash];

	variable_symbol_table->table[_hash] = symbol;
}

void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size--;
	Symbol* target_symbol = variable_symbol_table->table[_hash];
	variable_symbol_table->table[_hash] = variable_symbol_table->table[_hash]->next;
	free(target_symbol);
}

VariableData* create_variable_data(SymbolTable* variable_symbol_table, const wchar_t* type, const wchar_t* name, const wchar_t* access_modifier) {
	VariableData* result = (VariableData*)malloc(sizeof(VariableData));
	result->type = (wchar_t*)malloc(sizeof(wchar_t) * 128);
	result->name = (wchar_t*)malloc(sizeof(wchar_t) * 256);
	result->index = variable_symbol_table->size + get_prev_variable_index_size(variable_symbol_table) + 1;
	result->access_modifier = _wcsdup(access_modifier);

	wcscpy_s(result->type, 128, type);
	wcscpy_s(result->name, 256, name);

	return result;
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

FunctionData* get_member_function_data(const wchar_t* class_name, const wchar_t* function_name) {
	Symbol* class_symbol = find_symbol(class_symbol_table, class_name);

	if (class_symbol == NULL) return -1;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
	if (member_symbol != NULL) {
		FunctionData* function_data = member_symbol->data;
		return function_data;
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_function_data(class_data->parent_class_name, function_name);
		}

		return NULL;
	}
	return NULL;
}

int get_member_function_index(const wchar_t* class_name, const wchar_t* function_name) {
	Symbol* class_symbol = find_symbol(class_symbol_table, class_name);

	if (class_symbol == NULL) return -1;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
	if (member_symbol != NULL) {
		FunctionData* function_data = member_symbol->data;
		return function_data->index + get_parent_member_function_count(class_name);
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_function_index(class_data->parent_class_name, function_name);
		}

		return -1;
	}
	return -1;
}

VariableData* get_member_variable_data(const wchar_t* class_name, const wchar_t* variable_name) {
	Symbol* class_symbol = find_symbol(class_symbol_table, class_name);

	if (class_symbol == NULL) return -1;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
	if (member_symbol != NULL) {
		VariableData* variable_data = member_symbol->data;
		return variable_data;
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_variable_data(class_data->parent_class_name, variable_name);
		}

		return NULL;
	}
	return NULL;
}

int get_member_variable_index(const wchar_t* class_name, const wchar_t* variable_name) {
	Symbol* class_symbol = find_symbol(class_symbol_table, class_name);

	if (class_symbol == NULL) return -1;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
	if (member_symbol != NULL) {
		VariableData* variable_data = member_symbol->data;
		return variable_data->index + get_parent_member_variable_count(class_name);
	}
	else {

		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_variable_index(class_data->parent_class_name, variable_name);
		}

		return -1;
	}
	return -1;
}

int get_parent_member_variable_count(const wchar_t* class_name) {
	int result = 0;

	if (wcscmp(class_name, L"")) {
		ClassData* current_class_data = find_symbol(class_symbol_table, class_name)->data;

		if (wcscmp(current_class_data->parent_class_name, L"")) {
			ClassData* parent_class_data = find_symbol(class_symbol_table, current_class_data->parent_class_name)->data;
			result += parent_class_data->member_variables->size;
			result += get_parent_member_variable_count(current_class_data->parent_class_name);
		}
	}

	return result;
}

int get_parent_member_function_count(const wchar_t* class_name) {
	int result = 0;

	if (wcscmp(class_name, L"")) {
		ClassData* current_class_data = find_symbol(class_symbol_table, class_name)->data;

		if (wcscmp(current_class_data->parent_class_name, L"")) {
			ClassData* parent_class_data = find_symbol(class_symbol_table, current_class_data->parent_class_name)->data;
			result += parent_class_data->member_functions->size;
			result += get_parent_member_function_count(current_class_data->parent_class_name);
		}
	}

	return result;
}

void new_line(wchar_t** result, int indentation) {
	int i;
	*result = join_string(*result, L"\n");
	for (i = 0; i < indentation; i++) {
		*result = join_string(*result, L"  ");
	}
}

wchar_t* create_class_initializer(int indentation, ClassAST* class_ast) {
	wchar_t* result = L"";
	wchar_t* buffer[128];

	new_line(&result, indentation + 1);
	swprintf(buffer, 128, L"$initializer {");
	result = join_string(result, buffer);
	is_class_initializer = 1;

	int i;
	for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
		result = join_string(result, generate_ir(class_ast->member_variables[i], indentation + 2));
	}

	is_class_initializer = 0;
	new_line(&result, indentation + 1);
	result = join_string(result, L"}");

	return result;
}

wchar_t* create_class_constructor(int indentation, ClassAST* class_ast) {
	wchar_t* result = L"";
	wchar_t* buffer[128];

	new_line(&result, indentation + 1);
	swprintf(buffer, 128, L"$constructor {");
	result = join_string(result, buffer);
	is_class_initializer = 1;

	int i;
	for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
		result = join_string(result, generate_ir(class_ast->member_variables[i], indentation + 2));
	}

	is_class_initializer = 0;
	new_line(&result, indentation + 1);
	result = join_string(result, L"}");

	return result;
}

wchar_t* create_parameter_buffer(VariableDeclarationBundleAST* parameters_ast) {
	wchar_t* parameter_buffer = L"";

	int i;
	for (i = 0; i < parameters_ast->variable_count; i++) {
		VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

		VariableData* variable_data = create_variable_data(variable_symbol_table, parameter->variable_type, parameter->variable_name, parameter->access_modifier);
		insert_variable_symbol(variable_symbol_table, parameter->variable_name, variable_data);

		wchar_t single_parameter_buffer[512];
		swprintf(single_parameter_buffer, 512, L"%ls ", parameter->variable_type);

		parameter_buffer = join_string(parameter_buffer, single_parameter_buffer);

	}

	return parameter_buffer;
}

VariableData* find_variable_data(const wchar_t* class_name, IdentifierAST* identifier_ast) {

	Symbol* local_symbol = find_symbol(variable_symbol_table, identifier_ast->identifier);
	if (local_symbol == NULL) {
		return get_member_variable_data(class_name, identifier_ast->identifier);
	}
	else {
		return local_symbol->data;
	}
}

FunctionData* find_function_data(const wchar_t* class_name, const wchar_t* function_name, FunctionCallAST* function_call_ast) {
	Symbol* local_symbol = find_symbol(function_symbol_table, function_name);
	if (local_symbol == NULL) {
		return get_member_function_data(class_name, function_name);
	}
	else {
		FunctionData* function_data = local_symbol->data;
		return function_data;
	}
}

wchar_t* infer_type(void* ast, wchar_t* search_point_class_name) {
	ASTType type = *((ASTType*)(ast));
	switch (type) {
	case AST_NumberLiteral:
		NumberLiteralAST* number_ast = (NumberLiteralAST*)ast;
		return number_ast->numeric_type;
	case AST_StringLiteral:
		return L"string";
	case AST_Identifier:
		IdentifierAST* ident_ast = (IdentifierAST*)ast;

		if (!wcscmp(ident_ast->identifier, L"this")) {
			return current_class;
		}

		VariableData* data = find_variable_data(current_class, ast);

		return data->type;
	case AST_BinExpr: {
		BinExprAST* bin_expr_ast = (BinExprAST*)ast;

		wchar_t* left_type = infer_type(bin_expr_ast->left, search_point_class_name);
		wchar_t* right_type = infer_type(bin_expr_ast->right, search_point_class_name);

		if (check_castability(left_type, right_type)) {
			return right_type;
		}
		else if (check_castability(right_type, left_type)) {
			return left_type;
		}
		else {
			// Both types are can not be computed.
			printf(L"[Temporary error] in parser.c Type %s and %s are can not be computed.", left_type, right_type);
			abort();
		}
		break;
	}

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;

		return find_function_data(search_point_class_name, function_call_ast->function_name, ast);;
	}

	case AST_New: {
		NewAST* new_ast = (NewAST*)ast;

		return new_ast->class_name;
	}

	}
}

void create_attribute_ir(const wchar_t* target_class_name, void* attribute, wchar_t** result, int indentation) {
	Symbol* target_class_symbol = find_symbol(class_symbol_table, target_class_name);
	ClassData* target_class = target_class_symbol->data;
	Symbol* member_symbol = NULL;

	switch (*((ASTType*)attribute)) {
	case AST_Identifier: {
		int member_variable_index = get_member_variable_index(target_class_name, ((IdentifierAST*)attribute)->identifier);
		VariableData* member_variable_data = get_member_variable_data(target_class_name, ((IdentifierAST*)attribute)->identifier);

		new_line(result, indentation);
		wchar_t attr_str_buffer[128];
		swprintf(attr_str_buffer, 128, L"@attr %d", member_variable_index);
		*result = join_string(*result, attr_str_buffer);

		if (((IdentifierAST*)attribute)->attribute != NULL) {
			create_attribute_ir(member_variable_data->name, ((IdentifierAST*)attribute)->attribute, result, indentation);
		}

		break;
	}

	case AST_FunctionCall:
		FunctionCallAST* function_call_ast = ((FunctionCallAST*)attribute);
		int member_function_index = get_member_function_index(target_class_name, function_call_ast->function_name);
		FunctionData* member_function_data = get_member_function_data(target_class_name, function_call_ast->function_name);

		wchar_t function_call_buffer[128];
		swprintf(function_call_buffer, 128, L"@attr_call %d %d", member_function_index, function_call_ast->parameter_count);

		if (member_function_data->parameter_count != function_call_ast->parameter_count) {
			printf("[Temporary error] at ir.c Type not matched\n");
			abort();
		}

		int i;
		for (i = 0; i < member_function_data->parameter_count; i++) {
			*result = join_string(*result, generate_ir(function_call_ast->parameters[i], indentation));

			wchar_t* infered_type = infer_type(function_call_ast->parameters[i], target_class_name);

			if (!check_castability(infered_type, member_function_data->parameter_types[i])) {
				printf("[Temporary error] at ir.c Type not matched : %S, %S\n", infered_type, member_function_data->parameter_types[i]);
				abort();
			}
		}

		new_line(result, indentation);
		*result = join_string(*result, function_call_buffer);

		if (function_call_ast->attribute != NULL) {
			create_attribute_ir(member_function_data->return_type, function_call_ast->attribute, result, indentation);
		}
		break;
	}
}

void create_assign_ir(void* left_ast, void* right_ast, wchar_t** result, int indentation) {
	*result = join_string(*result, generate_ir(right_ast, indentation));

	// for non-attribute, direct assign
	// a = 5;
	switch (*((ASTType*)left_ast)) {
	case AST_Identifier:
		if (((IdentifierAST*)left_ast)->attribute == NULL) {
			new_line(result, indentation);
			IdentifierAST* identifier_ast = (IdentifierAST*)left_ast;
			wchar_t store_str_buffer[128];

			Symbol* local_symbol = find_symbol(variable_symbol_table, identifier_ast->identifier);

			if (local_symbol == NULL) {
				swprintf(store_str_buffer, 128, L"@mstore %d", get_member_variable_index(current_class, identifier_ast->identifier));
				*result = join_string(*result, store_str_buffer);
			}
			else {
				swprintf(store_str_buffer, 128, L"@store %d", ((VariableData*)local_symbol->data)->index);
				*result = join_string(*result, store_str_buffer);
			}
		}
		return;
	}

	void* temp_left_ast = left_ast;
	void* attribute_backup = left_ast;
	void* last_ast = NULL;

	int loop = 1;

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
		}
	}

	*result = join_string(*result, generate_ir(temp_left_ast, indentation));

	switch (*((ASTType*)last_ast)) {
	case AST_Identifier: {
		wchar_t* target_class_name = infer_type(temp_left_ast, L"");
		Symbol* target_class_symbol = find_symbol(class_symbol_table, target_class_name);
		ClassData* target_class = target_class_symbol->data;

		int member_variable_index = get_member_variable_index(target_class_name, ((IdentifierAST*)last_ast)->identifier);
		VariableData* member_variable_data = get_member_variable_data(target_class_name, ((IdentifierAST*)last_ast)->identifier);

		new_line(result, indentation);
		wchar_t attr_str_buffer[128];
		swprintf(attr_str_buffer, 128, L"@attr_store %d", member_variable_index);
		*result = join_string(*result, attr_str_buffer);
		break;
	}
	}
}

wchar_t* generate_ir(void* ast, int indentation) {

	wchar_t* result = L"";

	switch (*((ASTType*)ast)) {

	case AST_Return: {
		ReturnAST* return_ast = (ReturnAST*)ast;

		result = join_string(result, generate_ir(return_ast->expression, indentation));

		new_line(&result, indentation);
		wchar_t* ret_str_buffer[128];
		swprintf(ret_str_buffer, 128, L"@ret");
		result = join_string(result, ret_str_buffer);
		break;
	}

	case AST_BinExpr: {
		BinExprAST* bin_expr_ast = (BinExprAST*)ast;

		if (bin_expr_ast->opType == OpASSIGN) {
			create_assign_ir(bin_expr_ast->left, bin_expr_ast->right, &result, indentation);
			break;
		}

		result = join_string(result, generate_ir(bin_expr_ast->left, indentation));
		result = join_string(result, generate_ir(bin_expr_ast->right, indentation));

		wchar_t* operator_str_buffer[128];
		switch (bin_expr_ast->opType) {
		case OpADD:
			swprintf(operator_str_buffer, 128, L"@add");
			break;
		case OpSUB:
			swprintf(operator_str_buffer, 128, L"@sub");
			break;
		case OpMUL:
			swprintf(operator_str_buffer, 128, L"@mul");
			break;
		case OpDIV:
			swprintf(operator_str_buffer, 128, L"@div");
			break;
		case OpEQUAL:
			swprintf(operator_str_buffer, 128, L"@equal");
			break;
		case OpNOTEQUAL:
			swprintf(operator_str_buffer, 128, L"@notequal");
			break;
		case OpGREATER:
			swprintf(operator_str_buffer, 128, L"@greater");
			break;
		case OpLESSER:
			swprintf(operator_str_buffer, 128, L"@lesser");
			break;
		case OpEQUALGREATER:
			swprintf(operator_str_buffer, 128, L"@eqgreater");
			break;
		case OpEQUALLESSER:
			swprintf(operator_str_buffer, 128, L"@eqlesser");
			break;
		case OpOR:
			swprintf(operator_str_buffer, 128, L"@or");
			break;
		case OpAND:
			swprintf(operator_str_buffer, 128, L"@and");
			break;
		}

		new_line(&result, indentation);
		result = join_string(result, operator_str_buffer);

		break;
	}

	case AST_Identifier: {
		IdentifierAST* identifier_ast = (IdentifierAST*)ast;

		new_line(&result, indentation);
		wchar_t identifier_str_buffer[128];

		int is_special_identifier = !wcscmp(identifier_ast->identifier, L"this");

		if (is_special_identifier) {
			swprintf(identifier_str_buffer, 128, L"@this");
			result = join_string(result, identifier_str_buffer);
		}
		else {

			Symbol* local_symbol = find_symbol(variable_symbol_table, identifier_ast->identifier);

			if (local_symbol == NULL) {
				swprintf(identifier_str_buffer, 128, L"@mload %d", get_member_variable_index(current_class, identifier_ast->identifier));
				result = join_string(result, identifier_str_buffer);
			}
			else {
				swprintf(identifier_str_buffer, 128, L"@load %d", ((VariableData*)local_symbol->data)->index);
				result = join_string(result, identifier_str_buffer);
			}
		}

		if (identifier_ast->attribute != NULL) {
			wchar_t* target_class_name = infer_type(identifier_ast, current_class);
			create_attribute_ir(target_class_name, identifier_ast->attribute, &result, indentation);
		}

		break;
	}

	case AST_StringLiteral: {
		StringLiteralAST* string_literal_ast = (StringLiteralAST*)ast;

		wchar_t* string_literal = string_literal_ast->string_literal;
		wchar_t push_str_buffer[128];

		new_line(&result, indentation);

		swprintf(push_str_buffer, 128, L"@push string %s", string_literal);

		result = join_string(result, push_str_buffer);

		break;
	}

	case AST_New: {
		NewAST* new_ast = (NewAST*)ast;

		Symbol* class_symbol = find_symbol(class_symbol_table, new_ast->class_name);
		ClassData* class_data = class_symbol->data;

		FunctionData* constructor_data = class_data->constructor_data;

		if (new_ast->parameter_count != constructor_data->parameter_count) {
			printf("[Temporary error] at ir.c Type not matched\n");
			abort();
		}

		int i;
		for (i = 0; i < new_ast->parameter_count; i++) {
			result = join_string(result, generate_ir(new_ast->parameters[i], indentation));

			wchar_t* infered_type = infer_type(new_ast->parameters[i], current_class);

			if (!check_castability(infered_type, constructor_data->parameter_types[i])) {
				printf("[Temporary error] at ir.c Type not matched : %S, %S\n", infered_type, constructor_data->parameter_types[i]);
				abort();
			}
		}

		new_line(&result, indentation);

		wchar_t* new_buffer[512];
		swprintf(new_buffer, 128, L"@new %d %d", class_data->index, constructor_data->parameter_count);
		result = join_string(result, new_buffer);

		break;
	}

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;
		FunctionData* function_data = find_function_data(current_class, function_call_ast->function_name, function_call_ast);

		wchar_t* function_call_buffer[512];

		Symbol* local_symbol = find_symbol(function_symbol_table, function_call_ast->function_name);

		if (local_symbol == NULL) {
			swprintf(function_call_buffer, 128, L"@mcall %d %d", get_member_function_index(current_class, function_call_ast->function_name), function_call_ast->parameter_count);
		}
		else {
			swprintf(function_call_buffer, 128, L"@call %d %d", ((FunctionData*)local_symbol->data)->index, function_call_ast->parameter_count);
		}

		if (function_data->parameter_count != function_call_ast->parameter_count) {
			printf("[Temporary error] at ir.c Type not matched\n");
			abort();
		}

		int i;
		for (i = 0; i < function_data->parameter_count; i++) {
			result = join_string(result, generate_ir(function_call_ast->parameters[i], indentation));

			wchar_t* infered_type = infer_type(function_call_ast->parameters[i], current_class);

			if (!check_castability(infered_type, function_data->parameter_types[i])) {
				printf("[Temporary error] at ir.c Type not matched : %S, %S\n", infered_type, function_data->parameter_types[i]);
				abort();
			}
		}

		new_line(&result, indentation);
		result = join_string(result, function_call_buffer);

		break;
	}

	case AST_NumberLiteral: {
		NumberLiteralAST* number_literal_ast = (NumberLiteralAST*)ast;

		wchar_t* number_literal = number_literal_ast->number_literal;
		wchar_t push_str_buffer[128];

		new_line(&result, indentation);
		swprintf(push_str_buffer, 128, L"@push %s %s", number_literal_ast->numeric_type, number_literal);

		result = join_string(result, push_str_buffer);

		break;
	}

	case AST_Constructor: {
		ConstructorAST* constructor_ast = (ConstructorAST*)ast;

		wchar_t* parameter_buffer = L"";

		if (constructor_ast->parameters != NULL) {
			parameter_buffer = create_parameter_buffer(constructor_ast->parameters);
		}

		new_line(&result, indentation + 1);
		wchar_t buffer[256];
		swprintf(buffer, 256, L"$constructor %ls {", parameter_buffer);
		result = join_string(result, buffer);

		int i;
		for (i = 0; i < constructor_ast->body_count; i++) {
			result = join_string(result, generate_ir(constructor_ast->body[i], indentation + 2));
		}

		new_line(&result, indentation + 1);
		result = join_string(result, L"}");

		break;
	}

	case AST_FunctionDeclaration: {
		FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)ast;

		Symbol* function_symbol = find_symbol(function_symbol_table, function_declaration_ast->function_name);

		int should_find_member_function = function_symbol == NULL && wcscmp(current_class, L"");
		if (should_find_member_function) {
			ClassData* current_class_data = find_symbol(class_symbol_table, current_class)->data;
			function_symbol = find_symbol(current_class_data->member_functions, function_declaration_ast->function_name);
		}

		wchar_t* parameter_buffer = create_parameter_buffer(((VariableDeclarationBundleAST*)function_declaration_ast->parameters));
		FunctionData* function_data = function_symbol->data;

		open_scope();

		new_line(&result, indentation);
		wchar_t buffer[256];
		swprintf(buffer, 256, L".%d %ls{", (function_data->index), parameter_buffer);
		result = join_string(result, buffer);

		int i;
		for (i = 0; i < function_declaration_ast->body_count; i++) {
			result = join_string(result, generate_ir(function_declaration_ast->body[i], indentation + 1));
		}

		new_line(&result, indentation);
		result = join_string(result, L"}");

		close_scope();

		break;
	}

	case AST_Class: {
		ClassAST* class_ast = (ClassAST*)ast;

		ClassData* class_data = find_symbol(class_symbol_table, class_ast->class_name)->data;
		Symbol* parent_symbol = find_symbol(class_symbol_table, class_ast->parent_class_name);
		ClassData* parent_data = NULL;

		open_scope();

		current_class = class_ast->class_name;

		if (parent_symbol != NULL) {
			parent_data = parent_symbol->data;
		}

		wchar_t buffer[128];
		new_line(&result, indentation);
		swprintf(buffer, 128, L"class %d %d {", class_data->index, parent_data ? parent_data->index : 0);
		result = join_string(result, buffer);

		result = join_string(result, generate_ir(class_ast->constructor, indentation));
		result = join_string(result, create_class_initializer(indentation, class_ast));

		int i;
		for (i = 0; i < class_ast->member_function_count; i++) {
			result = join_string(result, generate_ir(class_ast->member_functions[i], indentation + 1));
		}

		new_line(&result, indentation);
		result = join_string(result, L"}\n");

		current_class = L"";
		close_scope();

		break;
	}

	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)ast;

		int i;
		for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
			result = join_string(result, generate_ir(variable_declaration_bundle_ast->variable_declarations[i], indentation));
		}

		break;
	}

	case AST_VariableDeclaration: {
		VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)ast;

		VariableData* data = NULL;

		if (is_class_initializer) {
			SymbolTable* member_variable_symbol_table = ((ClassData*)find_symbol(class_symbol_table, current_class)->data)->member_variables;
			data = create_variable_data(member_variable_symbol_table, variable_declaration_ast->variable_type, variable_declaration_ast->variable_name, variable_declaration_ast->access_modifier);
			insert_variable_symbol(member_variable_symbol_table, variable_declaration_ast->variable_name, data);
		}
		else {
			data = create_variable_data(variable_symbol_table, variable_declaration_ast->variable_type, variable_declaration_ast->variable_name, variable_declaration_ast->access_modifier);
			insert_variable_symbol(variable_symbol_table, variable_declaration_ast->variable_name, data);
		}

		if (variable_declaration_ast->declaration) {
			result = join_string(result, generate_ir(variable_declaration_ast->declaration, indentation));
		}

		// indexing for additional parent class member variables.
		int parent_variable_count = get_parent_member_variable_count(current_class);

		wchar_t store_str_buffer[128];

		new_line(&result, indentation);

		if (is_class_initializer) {
			swprintf(store_str_buffer, 128, L"%s %d", L"@mstore", data->index + parent_variable_count);
		}
		else {
			swprintf(store_str_buffer, 128, L"%s %d", L"@store", data->index);
		}

		result = join_string(result, store_str_buffer);

		break;
	}

	case AST_IdentIncrease: {
		IdentIncreaseAST* ident_increase_ast = (IdentIncreaseAST*)ast;

		new_line(&result, indentation);
		wchar_t ident_increase_str_buffer[128];
		swprintf(ident_increase_str_buffer, 128, L"@inc %d", ident_increase_ast->index);
		result = join_string(result, ident_increase_str_buffer);

		break;
	}

	case AST_IdentDecrease: {
		IdentDecreaseAST* ident_decrease_ast = (IdentDecreaseAST*)ast;

		new_line(&result, indentation);
		wchar_t ident_decrease_str_buffer[128];
		swprintf(ident_decrease_str_buffer, 128, L"@dec %d", ident_decrease_ast->index);
		result = join_string(result, ident_decrease_str_buffer);

		break;
	}

	case AST_IfStatement: {
		IfStatementAST* if_statement_ast = (IfStatementAST*)ast;

		open_scope();

		close_scope();

		break;
	}

	case AST_ForStatement: {
		ForStatementAST* for_statement_ast = (ForStatementAST*)ast;

		open_scope();

		result = join_string(result, generate_ir(for_statement_ast->init, indentation));

		label_id++;
		int end_label_id = label_id;
		label_id++;
		int begin_label_id = label_id;

		wchar_t goto_str_buffer[64];
		wchar_t label_str_buffer[64];
		wchar_t for_str_buffer[64];

		swprintf(goto_str_buffer, 64, L"@goto %x", end_label_id);
		new_line(&result, indentation);
		result = join_string(result, goto_str_buffer);

		swprintf(label_str_buffer, 64, L"@label %x", begin_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		int i;
		for (i = 0; i < for_statement_ast->body_count; i++) {
			result = join_string(result, generate_ir(for_statement_ast->body[i], indentation));
		}


		result = join_string(result, generate_ir(for_statement_ast->step, indentation));

		swprintf(label_str_buffer, 64, L"@label %x", end_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		result = join_string(result, generate_ir(for_statement_ast->condition, indentation));

		swprintf(label_str_buffer, 64, L"@for %x", begin_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		close_scope();

		break;
	}
	}

	return result;
}