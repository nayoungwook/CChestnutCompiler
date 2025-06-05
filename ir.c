#include "ir.h"

void insert_symbol(const wchar_t* name, Symbol* symbol) {
	unsigned int _hash = hash(name);
	symbol_table->size++;

	symbol->index = symbol_table->size;
	symbol->next = symbol_table->table[_hash];
	symbol->hash = _hash;
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

	Symbol* result = symbol_table->table[_hash];

	while (strcmp(name, result->symbol)) {
		result = result->next;
	}
	
	if (strcmp(result, name)) {
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
	symbol_table = current_symbol_table;
}

void close_scope() {
	SymbolTable* current_symbol_table = symbol_table;
	symbol_table = current_symbol_table->prev;
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

	wcscpy(result, str1);
	wcscat(result, str2);

	return result;
}

wchar_t* generate_ir(void* ast) {

	wchar_t* result = L"";
	wchar_t buffer[1024];
	int i;

	switch (*((ASTType*)ast)) {
	case AST_FunctionDeclaration:
		FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)ast;

		wchar_t* parameter_buffer = L"";
	
		open_scope();

		VariableDeclarationBundleAST* parameters_ast = ((VariableDeclarationBundleAST*)function_declaration_ast->parameters);

		for (i = 0; i < parameters_ast->variable_count; i++) {
			VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

			Symbol* symbol = (Symbol*)malloc(sizeof(Symbol));
			symbol->symbol = parameter->variable_name;
			symbol->type = parameter->variable_type;
			insert_symbol(parameter->variable_name, symbol);

			wchar_t single_parameter_buffer[512];
			swprintf(single_parameter_buffer, 512, L"%ls %d ", parameter->variable_type, symbol->index);

			parameter_buffer = join_string(parameter_buffer, single_parameter_buffer);

		}
	
		swprintf(buffer, 1024, L". %ls %ls{", function_declaration_ast->function_name, parameter_buffer);
		result = join_string(result, buffer);

		result = join_string(result, L"\n}");
		
		close_scope();

		break;
	}

	return result;
}