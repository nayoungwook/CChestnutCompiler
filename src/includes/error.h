#pragma once

#include "token.h"

typedef enum _ErrorCode {
	ER1001,// unexpected token
	ER1002, // failed to find class
	ER1003, // failed to find function
	ER1004, // failed to find variable
} ErrorCode;

const wchar_t* get_error_message(ErrorCode error_code, Token* error_token);
void handle_error(ErrorCode error_code, Token* error_token, const wchar_t* file_name, const wchar_t* str);