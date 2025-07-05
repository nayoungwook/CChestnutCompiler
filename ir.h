#pragma once

#include "ast.h"
#include "util.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* create_class_initializer(int indentation, ClassAST* class_ast);

wchar_t* infer_type(void* ast, wchar_t* current_class_name);
void new_line(wchar_t** result, int indentation);

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
VariableData* find_variable_data(const wchar_t* class_name, IdentifierAST* identifier_ast);
