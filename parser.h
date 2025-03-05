#pragma once

#include "token.h"
#include "ast.h"

void* parse(wchar_t* str);
void* parse_expression(wchar_t* str);
void* parse_term(wchar_t* str);
void* parse_simple_expression(wchar_t* str);