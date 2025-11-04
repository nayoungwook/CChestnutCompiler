#include "includes/error.h"

ErrorMessageEntry error_messages[] = {
  {ER_UnexpectedToken, L"Unexpected Token : %s."},
  {ER_FailedToFindClass, L"Class Not found : %s."},
  {ER_FailedToFindFunction, L"Function Not found : %s."},
  {ER_FailedToFindVariable, L"Variable Not found : %s."},
  {ER_UndefinedOperator,  L"Undefined operator : %s."},
  {ER_FailedToFindAttribute,  L"Failed to find attribute : %s."},
  {ER_TypeUnmatch,  L"Type unmatch error : %s."},
  {ER_TypeNotExist,  L"Type not exist : %s."},
  {ER_Terminate,  NULL},
};

const wchar_t* get_error_message(ErrorCode error_code, Token* error_token) {

  wchar_t* complete_error_message_buffer = (wchar_t*) safe_malloc(sizeof(wchar_t) * 512);
  wchar_t* error_message = L"";

  int i;
  for (i = 0; i < error_messages[i].err_code != ER_Terminate; i++) {
    if (error_messages[i].err_code == error_code) {
      error_message = _wcsdup(error_messages[i].str);
      break;
    }
  }

  swprintf(complete_error_message_buffer, error_message, error_token->str);

  return complete_error_message_buffer;
}

void handle_error(ErrorCode error_code, Token* error_token, const wchar_t* file_name, const wchar_t* str) {
  wprintf(L"Chestnut Compile Error.\n", error_code);
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
