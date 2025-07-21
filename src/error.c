#include "includes/error.h"

const wchar_t* get_error_message(ErrorCode error_code, Token* error_token) {
	wchar_t error_message_buffer[512];
	switch (error_code)
	{
	case ER1001: {
		swprintf(error_message_buffer, 512, L"Unexpected Token : %s.", error_token->str);
		break;
	}

	case ER1002: {
		swprintf(error_message_buffer, 512, L"Class Not found : %s.", error_token->str);
		break;
	}

	case ER1003: {
		swprintf(error_message_buffer, 512, L"Function Not found : %s.", error_token->str);
		break;
	}

	case ER1004: {
		swprintf(error_message_buffer, 512, L"Variable Not found : %s.", error_token->str);
		break;
	}

	default:
		swprintf(error_message_buffer, 512, L"Unexpected Error");
		break;
	}
	return error_message_buffer;
}

void handle_error(ErrorCode error_code, Token* error_token, const wchar_t* file_name, const wchar_t* str) {
	wprintf(L"Chestnut Compile Error.\n");
	wprintf(L"=======================\n");

	wprintf(L"%s\n\n", get_error_message(error_code, error_token));

	int line_number = error_token->line_number;

	wprintf(L"%d %s", line_number, substr(str, error_token->line_start_index, error_token->line_end_index));
	int index = error_token->str_index - error_token->line_start_index - wcslen(error_token->str);

	int i;
	for (i = 0; i < index; i++) {
		wprintf(L" ");
	}

	wprintf(L"/\\ here.\n");

	exit(1);
}