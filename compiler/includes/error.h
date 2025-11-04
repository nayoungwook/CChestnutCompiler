#pragma once

#include "token.h"
#include <wchar.h>

typedef enum _ErrorCode {
  ER_Terminate,

  ER_UnexpectedToken,
  ER_FailedToFindClass,
  ER_FailedToFindFunction,
  ER_FailedToFindVariable,
  ER_UndefinedOperator,
  ER_FailedToFindMemberVariable,
  ER_FailedToFindMemberFunction,
  ER_FailedToFindAttribute,
  ER_TypeUnmatch,
  ER_TypeNotExist,
} ErrorCode;

typedef struct _ErrorMessageEntry {
  ErrorCode err_code;
  const wchar_t* str;
} ErrorMessageEntry;

const wchar_t* get_error_message(ErrorCode error_code, Token* error_token);
void handle_error(ErrorCode error_code, Token* error_token, const wchar_t* file_name, const wchar_t* str);
