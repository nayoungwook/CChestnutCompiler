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
int get_prev_function_index_size();

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
