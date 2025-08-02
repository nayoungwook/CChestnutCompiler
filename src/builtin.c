#include "includes/builtin.h"

void initialize_builtin_functions(ParserContext* parser_context) {
	insert_function_symbol(parser_context->function_symbol_table, create_builtin_function_data(BUILTIN_PRINT));
}