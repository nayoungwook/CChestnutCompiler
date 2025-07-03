#pragma once

#include "ast.h"

#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <io.h>
#include <direct.h>

#define TABLE_SIZE 1024

wchar_t* get_working_directory();
wchar_t* read_file(const wchar_t* path);

// For strings.
wchar_t* join_string(const wchar_t* str1, const wchar_t* str2);
int is_decimal(wchar_t* str);

// Data Store
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
Symbol* find_symbol(SymbolTable* cur_symbol_table, const wchar_t* name);
unsigned int hash(const wchar_t* str);

typedef struct _Set {
	Symbol* table[TABLE_SIZE];
	unsigned int size;
} Set;
Symbol* find_symbol_from_set(Set* target_set, const wchar_t* name);

typedef struct _VariableData {
	const wchar_t* type;
	const wchar_t* name;
	unsigned int index;
	wchar_t* access_modifier;
} VariableData;

typedef struct _FunctionData {
	const wchar_t* name;
	const wchar_t* return_type;
	const wchar_t* mangled_name;
	unsigned int index;
	wchar_t** parameter_types;
	wchar_t* access_modifier;
} FunctionData;

const wchar_t* create_mangled_name(const wchar_t* name, VariableDeclarationBundleAST* parameters);

typedef struct _ClassData {
	const wchar_t* name;
	const wchar_t* parent_class_name;
	unsigned int index;
	SymbolTable* member_functions;
	SymbolTable* member_variables;
} ClassData;

SymbolTable* create_symbol_table();
Set* create_set();

int is_primitive_type(const wchar_t* type);