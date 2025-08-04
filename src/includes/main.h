#pragma once

#include <stdio.h>
#include <locale.h>
#include <limits.h>
#include <signal.h>

#include "util.h"
#include "token.h"
#include "parser.h"
#include "ir.h"
#include "builtin.h"

#pragma warning (disable : 5287) // operands are different enum types 'TokenType' and 'TokenType';
#pragma warning (disable : 5286) // operands are different enum types 'TokenType' and 'TokenType';


void initialize_primitive_types(ParserContext* parser_context);
void parse_file(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* file_name);