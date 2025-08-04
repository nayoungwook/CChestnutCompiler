#pragma once

#include "parser.h"

#define BUILTIN_PRINT 01
#define BUILTIN_INPUT 02

FunctionData* create_builtin_function_data(unsigned int id);
void initialize_builtin_functions(ParserContext* parser_context);