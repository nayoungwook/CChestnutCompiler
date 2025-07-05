#pragma once

#include "token.h"
#include "ast.h"
#include "util.h"

#define MEMBER_VARIABLE -1
#define MEMBER_FUNCTION -1

void* parse(wchar_t* str);
void* consume(wchar_t* str, TokenType expected_type);

void* parse_expression(wchar_t* str);
void* parse_unary_expression(wchar_t* str);
void* parse_simple_expression(wchar_t* str);
void* parse_term(wchar_t* str);

int check_castability(const wchar_t* from, const wchar_t* to);

typedef struct _Type {
	wchar_t* type_str;
	SymbolTable* child_types;
	struct _Type* parent_type;
} Type;
void insert_type_symbol(Type* target_type, const wchar_t* type_str);
void remove_type_symbol(const wchar_t* type_str);

void insert_set_symbol(Set* target_set, const wchar_t* str);

FunctionData* create_function_data(SymbolTable* function_symbol_table, const wchar_t* name, const wchar_t* return_type, VariableDeclarationBundleAST* parameters);
void insert_function_symbol(SymbolTable* function_symbol_table, FunctionDeclarationAST* ast);
void remove_function_symbol(SymbolTable* function_symbol_table, const wchar_t* mangled_name);

ClassData* create_class_data(ClassAST* class_ast);
void insert_class_symbol(ClassAST* ast);
void remove_class_symbol(const wchar_t* name);

void open_scope();
void close_scope();

int get_prev_variable_index_size(SymbolTable* variable_symbol_table);

VariableData* create_variable_data(SymbolTable* variable_symbol_table, const wchar_t* type, const wchar_t* name);
void insert_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name, VariableData* data);
void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name);

// AST creating functions
void* create_if_statement_ast(Token* tok, wchar_t* str);
void* create_paren_group_ast(Token* tok, wchar_t* str);
void* create_string_literal_ast(Token* tok, wchar_t* str);
void* create_number_literal_ast(Token* tok, wchar_t* str);
void* create_return_ast(Token* tok, wchar_t* str);
void* create_identifier_ast(Token* tok, wchar_t* str, int is_attribute_identifier);
void* create_function_declaration_ast(Token* tok, wchar_t* str);
void* create_for_statement_ast(Token* tok, wchar_t* str);
void* create_variable_declaration_ast(Token* tok, wchar_t* str);
void* create_class_ast(Token* tok, wchar_t* str);
void* create_constructor_ast(Token* tok, wchar_t* str);
void* create_new_ast(Token* tok, wchar_t* str);
VariableDeclarationBundleAST* create_function_parameters(wchar_t* str);