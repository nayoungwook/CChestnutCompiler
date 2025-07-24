#pragma once

#include <stdio.h>
#include <locale.h>
#include <limits.h>
#include <signal.h>

#include "util.h"
#include "token.h"
#include "parser.h"
#include "ir.h"

void initialize_primitive_types(ParserContext* parser_context);
void parse_file(ParserContext* parser_context, const wchar_t* file_name);