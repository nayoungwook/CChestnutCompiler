#pragma once

#define TABLE_SIZE 512

#include "ast.h"

#include <stdio.h>
#include <stdlib.h>

wchar_t* generate_ir(void* AST);
wchar_t* join_string(const wchar_t* str1, const wchar_tr2);

typedef struct _Symbol {
	const wchar_t* symbol;
	const wchar_t* type;
	unsigned int hash;
	unsigned int index;

	struct _Symbol* next; // for chaining
} Symbol;

typedef struct _SymbolTable {
	Symbol* table[TABLE_SIZE];
	struct _SymbolTable * prev;
	unsigned int size;
} SymbolTable;

SymbolTable* symbol_table;

void open_scope();
void close_scope();

void insert_symbol(const wchar_t* name, Symbol* symbol);
void remove_symbol(const wchar_t* name);
Symbol* find_symbol(const wchar_t* name);
unsigned int hash(const wchar_t* str);
