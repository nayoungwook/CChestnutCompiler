#pragma once

#include "ast.h"
#include "util.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* create_class_initializer(int indentation, ClassAST* class_ast);

void open_scope();
void close_scope();

int get_prev_variable_index_size(SymbolTable* variable_symbol_table);

VariableData* create_variable_data(SymbolTable* variable_symbol_table, Type* type, const wchar_t* name, const wchar_t* access_modifier);
void insert_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name, VariableData* data);
void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name);

Type* infer_type(void* ast, wchar_t* search_point_class_name);
void check_function_call_condition(FunctionData* function_data, const void** parameters, int parameter_count);
void new_line(wchar_t** result, int indentation);
Type* get_type_of_last_element(void* ast, const wchar_t* search_point_class_name);

wchar_t* generate_ir(void* AST, int indentation);
wchar_t* create_parameter_buffer(VariableDeclarationBundleAST* parameters_ast);
void create_attribute_ir(const wchar_t* target_class_name, void* attribute, wchar_t** result, int indentation);
void create_assign_ir(void* left_ast, void* right_ast, wchar_t** result, int indentation);

int get_member_variable_index(const wchar_t* class_name, const wchar_t* ident);
int get_parent_member_variable_count(const wchar_t* class_name);

int get_member_function_index(const wchar_t* class_name, const wchar_t* function_name);
int get_parent_member_function_count(const wchar_t* class_name);

FunctionData* get_member_function_data(const wchar_t* class_name, const wchar_t* function_name);
VariableData* get_member_variable_data(const wchar_t* class_name, const wchar_t* variable_name);
VariableData* find_variable_data(const wchar_t* class_name, const wchar_t* identifier);
