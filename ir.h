#pragma once

#define TABLE_SIZE 512

#include "ast.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* generate_ir(void* AST, int indentation);
wchar_t* join_string(const wchar_t* str1, const wchar_t* str2);
int is_decimal(wchar_t* str);
void new_line(wchar_t** result, int indentation);

typedef struct _VariableData {
	const wchar_t* type;
	const wchar_t* name;
	unsigned int index;
} VariableData;

VariableData* create_variable_data(const wchar_t* type, const wchar_t* name);

typedef struct _FunctionData {
	const wchar_t* return_type;
	const wchar_t* name;
	const wchar_t* mangled_name;
} FunctionData;

typedef struct _Symbol {
	const wchar_t* symbol;
	unsigned int hash;
	void* data;

	struct _Symbol* next; // for chaining
} Symbol;

typedef struct _SymbolTable {
	Symbol* table[TABLE_SIZE];
	struct _SymbolTable* prev;
	unsigned int size;
} SymbolTable;

void open_scope();
void close_scope();

void insert_variable_symbol(const wchar_t* name, VariableData* data);
void remove_symbol(const wchar_t* name);
Symbol* find_symbol(const wchar_t* name);
unsigned int hash(const wchar_t* str);
int get_prev_index_size();