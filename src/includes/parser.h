#pragma once

#include "token.h"
#include "ast.h"
#include "util.h"
#include "error.h"

// for access modifier
#define AM_DEFAULT 1
#define AM_PUBLIC 2
#define AM_PRIVATE 3
#define AM_PROTECTED 4

#define VARIABLE_ARGUMENTS -1

typedef struct _ParserContext {
	wchar_t* current_class;
	wchar_t* current_function_name;
	wchar_t* current_file_name;
	wchar_t* file_str;
	int class_count;

	SymbolTable* function_symbol_table;
	SymbolTable* class_symbol_table;
	SymbolTable* class_hierarchy;
	SymbolTable* variable_symbol_table;

	Set* primitive_types;
} ParserContext;

typedef struct _FunctionCallParameterContext {
	void** parameters;
	int parameter_count;
} FunctionCallParameterContext;
FunctionCallParameterContext* parse_function_call_parameter(ParserContext* parser_context, wchar_t* str);
void free_function_call_parameter(FunctionCallParameterContext* function_call_parameter);

void set_file_string(ParserContext* parser_context, const wchar_t* str);

void initialize_parser_context(ParserContext* parser_context);
ParserContext* create_parser_context();

bool is_primitive_type(ParserContext* parser_context, Type* type);

void* parse(ParserContext* parser_context, const wchar_t* str);
void consume(wchar_t* str, TokenType expected_type);

void* parse_compare_expression(ParserContext* parser_context, wchar_t* str);
void* parse_expression(ParserContext* parser_context, wchar_t* str);
void* parse_unary_expression(ParserContext* parser_context, wchar_t* str);
void* parse_simple_expression(ParserContext* parser_context, wchar_t* str);
void* parse_term(ParserContext* parser_context, wchar_t* str);

bool is_same_type(Type* t1, Type* t2);
bool check_castability(ParserContext* parser_context, Type* from, Type* to);

typedef struct _ClassNode {
	wchar_t* type_str;
	SymbolTable* child_types;
	struct _ClassNode* parent_type;
} ClassType;

Type* get_type(Token* tok, wchar_t* str);

void insert_type_symbol(ParserContext* parser_context, ClassType* target_type, const wchar_t* type_str);
void remove_type_symbol(ParserContext* parser_context, const wchar_t* type_str);

void insert_set_symbol(Set* target_set, const wchar_t* str);

FunctionData* create_function_data(SymbolTable* function_symbol_table, const wchar_t* name, Type* return_type, VariableDeclarationBundleAST* parameters);
void insert_function_symbol(SymbolTable* function_symbol_table, FunctionData* function_data);
void remove_function_symbol(SymbolTable* function_symbol_table, const wchar_t* name);

ClassData* create_class_data(ParserContext* parser_context, ClassAST* class_ast);
void insert_class_symbol(ParserContext* parser_context, ClassData* class_data);
void remove_class_symbol(ParserContext* parser_context, const wchar_t* name);
void create_class_constructor_data(ParserContext* parser_context, ClassAST* class_ast);

Token* get_parent_name(wchar_t* str);
void initialize_constructor_of_class(ClassAST* class_ast);

// AST creating functions
void* create_if_statement_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_paren_group_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_string_literal_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_number_literal_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_function_call_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_identifier_ast(ParserContext* parser_context, Token* tok, wchar_t* str, bool is_attribute_identifier);
void* create_function_declaration_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_for_statement_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_variable_declaration_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_class_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_constructor_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_new_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_array_declaration_ast(ParserContext* parser_context, Token* tok, wchar_t* str);
void* create_array_access_ast(ParserContext* parser_context, void* target_array, Token* tok, wchar_t* str);
void* create_bool_literal_ast(ParserContext* parser_context, Token* tok, const wchar_t* str);
void* create_neg_ast(ParserContext* parser_context, Token* tok, const wchar_t* str);

VariableDeclarationBundleAST* create_function_parameters(Token* tok, wchar_t* str);