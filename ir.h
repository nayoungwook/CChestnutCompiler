#pragma once

#include "ast.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* generate_ir(void* AST, int indentation);
void new_line(wchar_t** result, int indentation);

void open_scope();
void close_scope();

void insert_variable_symbol(const wchar_t* name, VariableData* data);
void remove_symbol(const wchar_t* name);
unsigned int hash(const wchar_t* str);
int get_prev_variable_index_size();