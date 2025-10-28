#include "includes/ir.h"
#include "includes/parser.h"
#include "includes/util.h"

void insert_set_symbol(Set* target_set, const wchar_t* str) {
	unsigned int _hash = hash(str);
	target_set->size++;

	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
	symbol->data = str;
	symbol->symbol = str;
	symbol->hash = _hash;
	symbol->next = target_set->table[_hash];

	target_set->table[_hash] = symbol;
}

void insert_symbol(SymbolTable* symbol_table, const wchar_t* name, void* data) {
	unsigned int hash_of_data = hash(name);
	symbol_table->size++;

	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
	symbol->data = data;
	symbol->symbol = _wcsdup(name);
	symbol->hash = hash_of_data;
	symbol->next = symbol_table->table[hash_of_data];

	symbol_table->table[hash_of_data] = symbol;
}

// variable symbol table management
void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name) {
	unsigned int _hash = hash(name);
	variable_symbol_table->size--;
	Symbol* target_symbol = variable_symbol_table->table[_hash];
	variable_symbol_table->table[_hash] = variable_symbol_table->table[_hash]->next;
	free(target_symbol);
}

// class symbol table management
void remove_class_symbol(ParserContext* parser_context, const wchar_t* name) {
	unsigned int _hash = hash(name);
	parser_context->class_symbol_table->size--;

	Symbol* target_symbol = parser_context->class_symbol_table->table[_hash];
	parser_context->class_symbol_table->table[_hash] = parser_context->class_symbol_table->table[_hash]->next;
	free(target_symbol);
}

// type symbol table management
Symbol* create_type_symbol(const wchar_t* type_str, ClassType* type) {
	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));

	unsigned int _hash = hash(type_str);

	symbol->data = type;
	symbol->symbol = type_str;
	symbol->hash = _hash;

	return symbol;
}

void insert_type_symbol(ParserContext* parser_context, ClassType* target_type, const wchar_t* type_str) {
	ClassType* child = (ClassType*)safe_malloc(sizeof(ClassType));
	child->type_str = type_str;
	child->parent_type = target_type;
	child->child_types = NULL;

	Symbol* child_symbol = create_type_symbol(type_str, child);
	child_symbol->next = parser_context->class_hierarchy->table[child_symbol->hash];
	parser_context->class_hierarchy->table[child_symbol->hash] = child_symbol;

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

void remove_type_symbol(ParserContext* parser_context, const wchar_t* type_str) {
	unsigned int _hash = hash(type_str);
	parser_context->class_hierarchy->size--;

	Symbol* target_symbol = parser_context->class_hierarchy->table[_hash];
	parser_context->class_hierarchy->table[_hash] = parser_context->class_hierarchy->table[_hash]->next;
}

// function symbol table management
void insert_function_symbol(SymbolTable* function_symbol_table, FunctionData* function_data) {
	unsigned int _hash = hash(function_data->name);

	Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
	symbol->data = function_data;
	symbol->symbol = _wcsdup(function_data->name);
	symbol->hash = _hash;
	symbol->next = function_symbol_table->table[_hash];

	function_symbol_table->size++;
	function_symbol_table->table[_hash] = symbol;
}

void remove_function_symbol(SymbolTable* function_symbol_table, const wchar_t* name) {
	unsigned int _hash = hash(name);
	Symbol* current = function_symbol_table->table[_hash];
	Symbol* prev = NULL;

	while (current != NULL) {
		if (wcscmp(current->symbol, name) == 0) {
			if (prev == NULL) {
				function_symbol_table->table[_hash] = current->next;
			}
			else {
				prev->next = current->next;
			}

			free(current);
			function_symbol_table->size--;
			return;
		}
		prev = current;
		current = current->next;
	}
}
