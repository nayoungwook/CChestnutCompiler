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

// ir symbol table management
void insert_ir_symbol(SymbolTable* ir_symbol_table, IrData* ir_data) {
  unsigned int _hash = hash(ir_data->name);

  Symbol* symbol = (Symbol*)safe_malloc(sizeof(Symbol));
  symbol->data = ir_data;
  symbol->symbol = _wcsdup(ir_data->name);
  symbol->hash = _hash;
  symbol->next = ir_symbol_table->table[_hash];

  ir_symbol_table->size++;
  ir_symbol_table->table[_hash] = symbol;
}

void remove_ir_symbol(SymbolTable* ir_symbol_table, const wchar_t* name) {
  unsigned int _hash = hash(name);
  Symbol* current = ir_symbol_table->table[_hash];
  Symbol* prev = NULL;

  while (current != NULL) {
    if (wcscmp(current->symbol, name) == 0) {
      if (prev == NULL) {
	ir_symbol_table->table[_hash] = current->next;
      }
      else {
	prev->next = current->next;
      }

      free(current);
      ir_symbol_table->size--;
      return;
    }
    prev = current;
    current = current->next;
  }
}

int get_ir_word(const wchar_t* name) {
  IrData* ir_data = (IrData*)(find_symbol(ir_byte_table, name)->data);
  return (ir_data)->data;
}
