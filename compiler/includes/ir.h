#pragma once

#include "ast.h"
#include "util.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

typedef struct _StringBuilder {
  wchar_t* str;
} StringBuilder;

StringBuilder* create_string_builder();
void append_string(StringBuilder* string_builder, const wchar_t* ctx);

typedef struct _IrGenContext {
  int label_id;
  bool is_class_initializer;
  wchar_t* current_class;
  StringBuilder* string_builder;
} IrGenContext;

IrGenContext* create_ir_context();

void create_class_initializer(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* class_ast);
void initialize_class_data(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* class_ast);

SymbolTable* ir_byte_table;
void initialize_byte_table();

void open_scope(ParserContext* parser_context);
void close_scope(ParserContext* parser_context);

int get_prev_variable_index_size(SymbolTable* variable_symbol_table);
bool check_accessibility(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* target_class_name, int access_modifier);
bool check_instanceof(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* target_class_name);

void check_type_of_variable_declaration(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationAST* variable_declaration_ast);;

VariableData* create_variable_data(SymbolTable* variable_symbol_table, Type* type, const wchar_t* name, int access_modifier);
void remove_variable_symbol(SymbolTable* variable_symbol_table, const wchar_t* name);

Token* get_token_of_ast(void* attribute);

Type* infer_type(IrGenContext* ir_context, ParserContext* parser_context, void* ast, wchar_t* search_point_class_name);

void check_function_call_condition
(IrGenContext* ir_context, ParserContext* parser_context, FunctionData* function_data, void** parameters, int parameter_count);
void new_line(StringBuilder* string_builder);
Type* get_type_of_last_element(IrGenContext* ir_context, ParserContext* parser_context, void* ast, const wchar_t* search_point_class_name);

void create_parameter_buffer(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationBundleAST* parameters_ast);

void create_ir(IrGenContext* ir_context, ParserContext* parser_context, void* ast);
void create_attribute_ir(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* target_class_name, void* attribute);
void create_assign_ir(IrGenContext* ir_context, ParserContext* parser_context, void* left_ast, void* right_ast);

IrData* create_ir_data(const wchar_t* name, int data);

int get_parent_member_variable_count(ParserContext* parser_context, const wchar_t* class_name);

VariableData* get_member_variable_data(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* class_name, const wchar_t* variable_name);
int get_member_function_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name);
int get_parent_member_function_count(ParserContext* parser_context, const wchar_t* class_name);

FunctionData* get_member_function_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name);
FunctionData* find_function_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* function_name, FunctionCallAST* function_call_ast);
VariableData* find_variable_data(IrGenContext* ir_context, ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* identifier);

void create_variable_initializer(IrGenContext* ir_context, ParserContext* parser_context, Type* variable_type);

void create_if_statement_block(IrGenContext* ir_context, ParserContext* parser_context, IfStatementAST* if_statement_ast, int end_label_id);

void create_array_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, ArrayDeclarationAST* array_declaration_ast);
void create_return_ir(IrGenContext* ir_context, ParserContext* parser_context, ReturnAST* return_ast);
void create_bin_expr_ir(IrGenContext* ir_context, ParserContext* parser_context, BinExprAST* bin_expr_ast);
void create_identifier_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentifierAST* identifier_ast);
void create_string_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, StringLiteralAST* string_literal_ast);
void create_new_ir(IrGenContext* ir_context, ParserContext* parser_context, NewAST* new_ast);
void create_array_access_ir(IrGenContext* ir_context, ParserContext* parser_context, ArrayAccessAST* ast);
void create_function_call_ir(IrGenContext* ir_context, ParserContext* parser_context, FunctionCallAST* function_call_ast);
void create_number_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, NumberLiteralAST* number_literal_ast);
void create_constructor_ir(IrGenContext* ir_context, ParserContext* parser_context, ConstructorAST* constructor_ast);
void create_function_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, FunctionDeclarationAST* function_declaration_ast);
void create_class_ir(IrGenContext* ir_context, ParserContext* parser_context, ClassAST* ast);
void create_variable_declaration_ir(IrGenContext* ir_context, ParserContext* parser_context, VariableDeclarationAST* variable_declaration_ast);
void create_ident_increase_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentIncreaseAST* ident_increase_ast);
void create_ident_decrease_ir(IrGenContext* ir_context, ParserContext* parser_context, IdentDecreaseAST* ident_decrease_ast);
void create_if_statement_ir(IrGenContext* ir_context, ParserContext* parser_context, IfStatementAST* if_statement_ast);
void create_for_statement_ir(IrGenContext* ir_context, ParserContext* parser_context, ForStatementAST* for_statement_ast);
void create_bool_literal_ir(IrGenContext* ir_context, ParserContext* parser_context, BoolLiteralAST* bool_literal_ast);
void create_neg_ir(IrGenContext* ir_context, ParserContext* parser_context, NegAST* negative_ast);
