#pragma once

#include "ast.h"
#include "util.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define IDENTIFIER_NOT_FOUND -1

wchar_t* create_class_initializer(ParserContext* parser_context, int indentation, ClassAST* class_ast);

void open_scope(ParserContext* parser_context);
void close_scope(ParserContext* parser_context);

int get_prev_variable_index_size(SymbolTable* variable_symbol_table);
int check_accessibility(ParserContext* parser_context, const wchar_t* target_class_name, int access_modifier);
int check_super_class(ParserContext* parser_context, const wchar_t* from, const wchar_t* to);

VariableData* create_variable_data(SymbolTable* variable_symbol_table, Type* type, const wchar_t* name, int access_modifier);
void insert_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name, VariableData* data);
void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name);

Type* infer_type(ParserContext* parser_context, void* ast, wchar_t* search_point_class_name);
void check_function_call_condition(ParserContext* parser_context, FunctionData* function_data, const void** parameters, int parameter_count);
void new_line(wchar_t** result, int indentation);
Type* get_type_of_last_element(ParserContext* parser_context, void* ast, const wchar_t* search_point_class_name);

wchar_t* create_ir(ParserContext* parser_context, void* AST, int indentation);
wchar_t* create_parameter_buffer(ParserContext* parser_context, VariableDeclarationBundleAST* parameters_ast);
void create_attribute_ir(ParserContext* parser_context, const wchar_t* target_class_name, void* attribute, wchar_t** result, int indentation);
void create_assign_ir(ParserContext* parser_context, void* left_ast, void* right_ast, wchar_t** result, int indentation);

int get_member_variable_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* ident);
int get_parent_member_variable_count(ParserContext* parser_context, const wchar_t* class_name);

int get_member_function_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name);
int get_parent_member_function_count(ParserContext* parser_context, const wchar_t* class_name);

FunctionData* get_member_function_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name);
FunctionData* find_function_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* function_name, FunctionCallAST* function_call_ast);
VariableData* find_variable_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* identifier);

wchar_t* create_if_statement_block(ParserContext* parser_context, IfStatementAST* if_statement_ast, int indentation, int end_label_id);

wchar_t* create_array_declaration_ir(ParserContext* parser_context, ArrayDeclarationAST* array_declaration_ast, int indentation);
wchar_t* create_return_ir(ParserContext* parser_context, ReturnAST* return_ast, int indentation);
wchar_t* create_bin_expr_ir(ParserContext* parser_context, BinExprAST* bin_expr_ast, int indentation);
wchar_t* create_identifier_ir(ParserContext* parser_context, IdentifierAST* identifier_ast, int indentation);
wchar_t* create_string_literal_ir(ParserContext* parser_context, StringLiteralAST* string_literal_ast, int indentation);
wchar_t* create_new_ir(ParserContext* parser_context, NewAST* new_ast, int indentation);
