#pragma once

#include "token.h"
#include "ast.h"
#include "util.h"

void* parse(wchar_t* str);
void* consume(wchar_t* str, TokenType expected_type);

void* parse_expression(wchar_t* str);
void* parse_unary_expression(wchar_t* str);
void* parse_simple_expression(wchar_t* str);
void* parse_term(wchar_t* str);

void insert_function_symbol(FunctionDeclarationAST* ast);
void remove_function_symbol(const wchar_t* mangled_name);
int get_prev_function_index_size();

VariableData* create_variable_data(const wchar_t* type, const wchar_t* name);
FunctionData* create_function_data(const wchar_t* name, const wchar_t* return_type, VariableDeclarationBundleAST* parameters);

wchar_t* infer_type(void* ast);

typedef struct _Type {
	wchar_t* type_str;
	SymbolTable* child_types;
	struct _Type* parent_type;
} Type;

void open_scope();
void close_scope();

void insert_variable_symbol(const wchar_t* name, VariableData* data);
void remove_variable_symbol(const wchar_t* name);
unsigned int hash(const wchar_t* str);
int get_prev_variable_index_size();

void insert_type_symbol(Type* target_type, const wchar_t* type_str);
void remove_type_symbol(const wchar_t* type_str);

// AST creating functions
void* create_if_statement_ast(Token* tok, wchar_t* str);
void* create_paren_group_ast(Token* tok, wchar_t* str);
void* create_string_literal_ast(Token* tok, wchar_t* str);
void* create_number_literal_ast(Token* tok, wchar_t* str);
void* create_return_ast(Token* tok, wchar_t* str);
void* create_identifier_ast(Token* tok, wchar_t* str);
void* create_function_declaration_ast(Token* tok, wchar_t* str);
void* create_for_statement_ast(Token* tok, wchar_t* str);
void* create_variable_declaration_ast(Token* tok, wchar_t* str);
