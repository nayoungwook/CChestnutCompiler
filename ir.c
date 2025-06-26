#include "ir.h"

int label_id = 0;
SymbolTable* symbol_table;

VariableData* create_variable_data(const wchar_t* type, const wchar_t* name) {
	VariableData* result = (VariableData*)malloc(sizeof(VariableData));

	result->type = type;
	result->name = name;
	result->index = symbol_table->size + get_prev_index_size() + 1;

	return result;
}

int get_prev_index_size() {
	SymbolTable* searcher_table = symbol_table;

	int _size = 0;

	while (searcher_table->prev != NULL) {
		searcher_table = searcher_table->prev;
		_size += searcher_table->size;
	}

	return _size;
}

void insert_variable_symbol(const wchar_t* name, VariableData* data) {
	unsigned int _hash = hash(name);
	symbol_table->size++;

	Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = name;
	symbol->hash = _hash;
	symbol->next = symbol_table->table[_hash];

	symbol_table->table[_hash] = symbol;
}

void remove_symbol(const wchar_t* name) {
	unsigned int _hash = hash(name);
	symbol_table->size--;
	Symbol* target_symbol = symbol_table->table[_hash];
	symbol_table->table[_hash] = symbol_table->table[_hash]->next;
	free(target_symbol);
}

Symbol* find_symbol(const wchar_t* name) {
	unsigned int _hash = hash(name);

	SymbolTable* cur_symbol_table = symbol_table;
	Symbol* result = cur_symbol_table->table[_hash];

	while (result == NULL) {
		cur_symbol_table = cur_symbol_table->prev;
		result = cur_symbol_table->table[_hash];
	}

	while (strcmp(name, result->symbol)) {
		result = result->next;
	}

	if (result == NULL || strcmp(name, result->symbol)) {
		return NULL;
	}

	return result;
}

unsigned int hash(const wchar_t* str) {
	unsigned int hash = 0;
	while (*str) {
		hash = (hash << 5) + *str++;
	}
	return hash % TABLE_SIZE;
}

void open_scope() {
	SymbolTable* current_symbol_table = symbol_table;

	SymbolTable* new_symbol_table = (SymbolTable*)malloc(sizeof(SymbolTable));
	new_symbol_table->prev = current_symbol_table;
	new_symbol_table->size = 0;

	memset(new_symbol_table->table, NULL, sizeof(Symbol*) * TABLE_SIZE);

	symbol_table = new_symbol_table;
}

void close_scope() {
	SymbolTable* current_symbol_table = symbol_table;
	symbol_table = symbol_table->prev;
	free(current_symbol_table);
}

wchar_t* join_string(const wchar_t* str1, const wchar_t* str2) {
	if (!str1) str1 = L"";
	if (!str2) str2 = L"";

	size_t len1 = wcslen(str1);
	size_t len2 = wcslen(str2);

	wchar_t* result = (wchar_t*)malloc((len1 + len2 + 1) * sizeof(wchar_t));
	if (!result) {
		return NULL;
	}

	wcscpy_s(result, len1 + len2 + 1, str1);
	wcscat_s(result, len1 + len2 + 1, str2);

	return result;
}

int is_decimal(wchar_t* str) {
	while (*str != L'\0') {
		if (*str == L'.' || *str == L'f') return 1;
		str++;
	}
	return 0;
}

void new_line(wchar_t** result, int indentation) {
	int i;
	*result = join_string(*result, L"\n");
	for (i = 0; i < indentation; i++) {
		*result = join_string(*result, L"  ");
	}
}

wchar_t* generate_ir(void* ast, int indentation) {

	wchar_t* result = L"";
	int i;

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

		Symbol* symbol = find_symbol(identifier_ast->identifier);

		new_line(&result, indentation);
		wchar_t identifier_str_buffer[128];
		swprintf(identifier_str_buffer, 128, L"@load %d", ((VariableData*)symbol->data)->index);
		result = join_string(result, identifier_str_buffer);

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

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;

		for (int i = 0; i < function_call_ast->parameter_count; i++) {
			result = join_string(result, generate_ir(function_call_ast->parameters[i], indentation));
		}

		break;
	}

	case AST_NumberLiteral: {
		NumberLiteralAST* number_literal_ast = (NumberLiteralAST*)ast;

		wchar_t* number_literal = number_literal_ast->number_literal;
		wchar_t push_str_buffer[128];

		new_line(&result, indentation);

		if (is_decimal(number_literal)) {
			if (number_literal[wcslen(number_literal) - 1] == L'f') {
				swprintf(push_str_buffer, 128, L"@push float %s", number_literal);
			}
			else {
				swprintf(push_str_buffer, 128, L"@push double %s", number_literal);
			}
		}
		else {
			swprintf(push_str_buffer, 128, L"@push int %s", number_literal);
		}

		result = join_string(result, push_str_buffer);

		break;
	}

	case AST_FunctionDeclaration: {
		FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)ast;

		wchar_t* parameter_buffer = L"";

		open_scope();

		VariableDeclarationBundleAST* parameters_ast = ((VariableDeclarationBundleAST*)function_declaration_ast->parameters);

		for (i = 0; i < parameters_ast->variable_count; i++) {
			VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

			VariableData* variable_data = create_variable_data(parameter->variable_type, parameter->variable_name);
			insert_variable_symbol(parameter->variable_name, variable_data);

			wchar_t single_parameter_buffer[512];
			swprintf(single_parameter_buffer, 512, L"%ls %d ", parameter->variable_type, ((VariableData*)variable_data)->index);

			parameter_buffer = join_string(parameter_buffer, single_parameter_buffer);

		}

		wchar_t buffer[256];
		swprintf(buffer, 256, L". %ls %ls{", function_declaration_ast->function_name, parameter_buffer);
		result = join_string(result, buffer);

		for (i = 0; i < function_declaration_ast->body_count; i++) {
			result = join_string(result, generate_ir(function_declaration_ast->body[i], indentation + 1));
		}

		result = join_string(result, L"\n}");

		close_scope();

		break;
	}

	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)ast;

		for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
			result = join_string(result, generate_ir(variable_declaration_bundle_ast->variable_declarations[i], indentation));
		}

		break;
	}

	case AST_VariableDeclaration: {
		VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)ast;

		VariableData* variable_data = create_variable_data(variable_declaration_ast->variable_type, variable_declaration_ast->variable_name);
		insert_variable_symbol(variable_declaration_ast->variable_name, variable_data);

		if (variable_declaration_ast->declaration) {
			result = join_string(result, generate_ir(variable_declaration_ast->declaration, indentation));
		}

		wchar_t store_str_buffer[128];

		new_line(&result, indentation);
		swprintf(store_str_buffer, 128, L"@store %d", variable_data->index);

		result = join_string(result, store_str_buffer);

		break;
	}

	case AST_IdentIncrease: {
		IdentIncreaseAST* ident_increase_ast = (IdentIncreaseAST*)ast;

		Symbol* symbol = find_symbol(ident_increase_ast->identifier);

		new_line(&result, indentation);
		wchar_t ident_increase_str_buffer[128];
		swprintf(ident_increase_str_buffer, 128, L"@inc %d", ((VariableData*)symbol->data)->index);
		result = join_string(result, ident_increase_str_buffer);

		break;
	}

	case AST_IdentDecrease: {
		IdentDecreaseAST* ident_decrease_ast = (IdentDecreaseAST*)ast;

		Symbol* symbol = find_symbol(ident_decrease_ast->identifier);

		new_line(&result, indentation);
		wchar_t ident_decrease_str_buffer[128];
		swprintf(ident_decrease_str_buffer, 128, L"@dec %d", ((VariableData*)symbol->data)->index);
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