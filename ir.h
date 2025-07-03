#pragma once

#include "ast.h"
#include "util.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* create_class_initializer(int indentation, ClassAST* class_ast);

wchar_t* generate_ir(void* AST, int indentation);
void new_line(wchar_t** result, int indentation);
int get_parent_member_variable_count(const wchar_t* class_name);
int get_member_variable_index(const wchar_t* class_name, const wchar_t* ident);