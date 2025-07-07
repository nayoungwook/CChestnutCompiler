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

int is_same_type(Type* t1, Type* t2);
int check_castability(Type* from, Type* to);

typedef struct _ClassNode {
	wchar_t* type_str;
	SymbolTable* child_types;
	struct _ClassNode* parent_type;
} ClassType;

Type* get_type(Token* tok, wchar_t* str);

void insert_type_symbol(ClassType* target_type, const wchar_t* type_str);
void remove_type_symbol(const wchar_t* type_str);

void insert_set_symbol(Set* target_set, const wchar_t* str);

FunctionData* create_function_data(SymbolTable* function_symbol_table, const wchar_t* name, Type* return_type, VariableDeclarationBundleAST* parameters);
void insert_function_symbol(SymbolTable* function_symbol_table, FunctionDeclarationAST* ast);
void remove_function_symbol(SymbolTable* function_symbol_table, const wchar_t* name);

ClassData* create_class_data(ClassAST* class_ast);
void insert_class_symbol(ClassAST* ast);
void remove_class_symbol(const wchar_t* name);

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
void* create_array_declaration_ast(Token* tok, wchar_t* str);
void* create_array_access_ast(Token* tok, wchar_t* str);

VariableDeclarationBundleAST* create_function_parameters(Token* tok, wchar_t* str);