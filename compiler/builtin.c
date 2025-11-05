#include "includes/builtin.h"

FunctionData* create_builtin_function_data(unsigned int id) {
  FunctionData* result = (FunctionData*)safe_malloc(sizeof(FunctionData));
  result->index = id;
  Type* return_type = (Type*)safe_malloc(sizeof(Type));
  return_type->array_element_type = NULL;
  return_type->is_array = 0;
  return_type->type_str = L"void";

  switch (id)
    {
    case BUILTIN_PRINT:
      result->name = L"print";
      result->parameter_count = POSITIONAL_ARGUMENTS;
      result->parameter_types = NULL;
      break;
    case BUILTIN_INPUT:
      result->name = L"input";
      result->parameter_count = 0;
      result->parameter_types = NULL;

      return_type->type_str = L"string";
      break;
    }

  result->return_type = return_type;
  result->is_builtin_function = 1;
  return result;
}

void initialize_builtin_functions(ParserContext* parser_context) {
  FunctionData* function_data = create_builtin_function_data(BUILTIN_PRINT);
  insert_symbol(parser_context->function_symbol_table, function_data->name, function_data);
}
