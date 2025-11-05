#include "includes/ir.h"
#include "includes/parser.h"

FunctionData* create_function_data(SymbolTable* function_symbol_table, const wchar_t* name, Type* return_type, VariableDeclarationBundleAST* parameters) {
  FunctionData* result = (FunctionData*)safe_malloc(sizeof(FunctionData));

  result->name = _wcsdup(name);
  result->return_type = return_type;
  result->index = function_symbol_table->size;
  result->access_modifier = AM_DEFAULT;
  result->parameter_types = NULL;
  result->parameter_count = parameters->variable_count;
  result->is_builtin_function = 0;

  int i;
  for (i = 0; i < parameters->variable_count; i++) {
    result->parameter_types = (Type**)safe_realloc(result->parameter_types, sizeof(Type*) * (i + 1));

    VariableDeclarationAST* variable_declaration = parameters->variable_declarations[i];
    result->parameter_types[i] = variable_declaration->variable_type;
  }

  return result;
}

ClassData* create_class_data(ParserContext* parser_context, ClassAST* class_ast) {
  ClassData* result = (ClassData*)safe_malloc(sizeof(ClassData));
  parser_context->class_count++;

  result->name = _wcsdup(class_ast->class_name_token->str);
  result->parent_class_name = _wcsdup(class_ast->parent_class_name_token == NULL ? L"" : class_ast->parent_class_name_token->str);
  result->index = parser_context->class_count;

  result->member_variables = create_symbol_table();
  result->member_functions = create_symbol_table();

  return result;
}

VariableData* create_variable_data(SymbolTable* variable_symbol_table, Type* type, const wchar_t* name, int access_modifier) {
  VariableData* result = (VariableData*)safe_malloc(sizeof(VariableData));
  result->type = type;
  result->name = _wcsdup(name);
  result->index = variable_symbol_table->size + get_prev_variable_index_size(variable_symbol_table) + 1;
  result->access_modifier = access_modifier;

  return result;
}

IrData* create_ir_data(const wchar_t* name, int data) {
  IrData* result = (IrData*)safe_malloc(sizeof(IrData));

  result->name = _wcsdup(name);
  result->data = data;

  return result;
}
