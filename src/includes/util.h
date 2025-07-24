#pragma once

#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <io.h>
#include <direct.h>

#define TABLE_SIZE 1024

void* safe_malloc(size_t size);
void* safe_relloc(void* ptr, size_t size);

wchar_t* get_working_directory();
wchar_t* read_file(const wchar_t* path);

wchar_t* substr(const wchar_t* str, int s, int e);

// For strings.
wchar_t* join_string(const wchar_t* str1, const wchar_t* str2);
int is_decimal(wchar_t* str);

typedef struct _Type {
	wchar_t* type_str;
	struct _Type* array_element_type;
	int is_array;
} Type;

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
	Type* type;
	const wchar_t* name;
	wchar_t* access_modifier;
	unsigned int index;
} VariableData;

typedef struct _FunctionData {
	const wchar_t* name;
	Type* return_type;
	unsigned int parameter_count;
	Type** parameter_types;
	unsigned int index;
	wchar_t* access_modifier;
} FunctionData;

typedef struct _ClassData {
	const wchar_t* name;
	const wchar_t* parent_class_name;
	unsigned int index;
	SymbolTable* member_functions;
	SymbolTable* member_variables;
	FunctionData* constructor_data;
} ClassData;

SymbolTable* create_symbol_table();
Set* create_set();

