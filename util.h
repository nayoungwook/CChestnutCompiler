#pragma once

#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <io.h>
#include <direct.h>

wchar_t* get_working_directory();
wchar_t* read_file(const wchar_t* path);