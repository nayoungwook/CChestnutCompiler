#pragma once

#include "token.h"
#include "ast.h"

void* parse(wchar_t* str);
void* consume(wchar_t* str, TokenType expected_type);

void* parse_expression(wchar_t* str);
void* parse_unary_expression(wchar_t* str);
void* parse_simple_expression(wchar_t* str);
void* parse_term(wchar_t* str);