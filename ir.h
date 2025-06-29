#pragma once

#include "ast.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wchar_t* generate_ir(void* AST, int indentation);
void new_line(wchar_t** result, int indentation);
