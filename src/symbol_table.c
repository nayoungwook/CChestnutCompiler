#include "includes/ir.h"
#include "includes/parser.h"

void insert_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name, VariableData* data) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size++;

	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
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

void insert_class_symbol(ParserContext* parser_context, ClassData* data) {
	unsigned int _hash = hash(data->name);

	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = _wcsdup(data->name);
	symbol->hash = _hash;
	symbol->next = parser_context->class_symbol_table->table[_hash];

	parser_context->class_symbol_table->size++;
	parser_context->class_symbol_table->table[_hash] = symbol;
}

void remove_class_symbol(ParserContext* parser_context, const wchar_t* name) {
	unsigned int _hash = hash(name);
	parser_context->class_symbol_table->size--;

	Symbol* target_symbol = parser_context->class_symbol_table->table[_hash];
	parser_context->class_symbol_table->table[_hash] = parser_context->class_symbol_table->table[_hash]->next;
	free(target_symbol);
}