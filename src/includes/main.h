#pragma once

#include <stdio.h>
#include <locale.h>
#include <limits.h>
#include <signal.h>

#include "util.h"
#include "token.h"
#include "parser.h"
#include "ir.h"

void initialize_global_symbol_table();
void compile_file(const wchar_t* file_name);