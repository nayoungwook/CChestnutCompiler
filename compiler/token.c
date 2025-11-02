#include "includes/token.h"

short is_special_character(const wchar_t wc) {
  if (wc == L'_') return 0;

  if ((wc >= L'!' && wc <= L'/') ||
      (wc >= L':' && wc <= L'@') ||
      (wc >= L'[' && wc <= L'`') ||
      (wc >= L'{' && wc <= L'~')) {
    return 1;
  }
  return 0;
}

KeywordEntry keyword_table[] = {
  {L"var", TokVar},
  {L"if", TokIf},
  {L"for", TokFor},
  {L"func", TokFunc},
  {L"return", TokReturn},
  {L"else", TokElse},
  {L"class", TokClass},
  {L"extends", TokExtends},
  {L"private", TokPrivate},
  {L"public", TokPublic},
  {L"protected", TokProtected},
  {L"constructor", TokConstructor},
  {L"new", TokNew},
  {L"true", TokTrue},
  {L"false", TokFalse},
  {NULL, TokEOF},
};

TokenType
get_token_type_of_special_character(wchar_t* str, int* str_len, int* read_index, wchar_t* c) {
  TokenType type = TokEOF;

  switch (*c) {

  case L'\"': {
    type = TokStringLiteral;

    while (*(c + 1) != L'\"') {
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }

    c++;

    str[*str_len] = *c;
    (*str_len)++;
    (*read_index)++;

    break;
  }

  case L'=': {
    type = TokAssign;
    if (*(c + 1) == L'=') {
      type = TokEqual;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }

  case L'!': {
    type = TokNot;
    if (*(c + 1) == L'=') {
      type = TokNotEqual;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }

  case L'#': {
    type = TokSharp;
    break;
  }

  case L'<': {
    type = TokLesser;

    if (*(c + 1) == L'=') {
      type = TokEqualLesser;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }

  case L'>': {
    type = TokGreater;
    if (*(c + 1) == L'=') {
      type = TokEqualGreater;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }

  case L'(': {
    type = TokLParen;
    break;
  }

  case L')': {
    type = TokRParen;
    break;
  }

  case L'{': {
    type = TokLBracket;
    break;
  }

  case L'}': {
    type = TokRBracket;
    break;
  }

  case L'[': {
    type = TokLSquareBracket;
    break;
  }

  case L']': {
    type = TokRSquareBracket;
    break;
  }

  case L':': {
    type = TokColon;
    break;
  }

  case L';': {
    type = TokSemiColon;
    break;
  }

  case L'.': {
    type = TokDot;
    break;
  }

  case L',': {
    type = TokComma;
    break;
  }

  case L'|': {
    type = TokBitOr;

    if (*(c + 1) == L'|') {
      type = TokOr;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }

  case L'&': {
    type = TokBitAnd;

    if (*(c + 1) == L'&') {
      type = TokAnd;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }
#pragma region BASIC_OPS

  case L'+': {
    type = TokAdd;
    if (*(c + 1) == L'=') {
      type = TokPlusAssign;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    if (*(c + 1) == L'+') {
      type = TokIncrease;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }

    break;
  }

  case L'-': {
    type = TokSub;
    if (*(c + 1) == L'=') {
      type = TokMinusAssign;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    if (*(c + 1) == L'-') {
      type = TokDecrease;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }

    break;
  }

  case L'*': {
    type = TokMul;
    if (*(c + 1) == L'=') {
      type = TokMultAssign;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }
    if (*(c + 1) == L'*') {
      type = TokPow;
      c++;

      str[*str_len] = *c;
      (*str_len)++;
      (*read_index)++;
    }

    break;
  }

  case L'/': {
    type = TokDiv;
    if (*(c + 1) == L'=') {
      type = TokDivAssign;
      c++;

      str[*str_len] = c;
      (*str_len)++;
      (*read_index)++;
    }
    break;
  }
#pragma endregion BASIC_OPS
  }

  return type;
}

static int read_index = 0;
static Token* token_cache = NULL;

Token* peek_token(wchar_t* line) {
  int backup_read_index = read_index;
  Token* result = token_cache;
  if (!result) {
    result = pull_token(line);
    read_index = backup_read_index;
    token_cache = result;
  }

  return result;
}

extern int* line_index_data;
extern int* line_number_data;
static int line_number_index = 0;

void update_line_number_index() {
  while (line_index_data[line_number_index - 1] < read_index) {
    line_number_index++;
  }
}

Token* pull_token(wchar_t* line) {
  token_cache = NULL;
  int result_len = 0;

  wchar_t* str = (wchar_t*)safe_malloc(sizeof(wchar_t) * MAX_TOKEN_STR);
  int str_len = 0;
  TokenType type = TokEOF;
  wchar_t c = *(line + read_index);

  while (c != '\0') {
    str_len = 0;
    memset(str, L'\0', sizeof(wchar_t) * MAX_TOKEN_STR);
    if (isspace(c)) {
      read_index++;
      c = *(line + read_index);
      continue;
    }

    if (isalpha(c)) {
      while (isalnum(c) || c == L'_') {
	str[str_len] = c;
	str_len++;
	read_index++;
	c = *(line + read_index);
      }
      str[str_len] = L'\0';

      type = TokIdent;

      int i;
      for (i = 0; keyword_table[i].keyword != NULL; i++) {
	if (!wcscmp(str, keyword_table[i].keyword)) {
	  type = keyword_table[i].type;
	  break;
	}
      }
    }
    else if (is_special_character(c)) {
      str[str_len] = c;
      str_len++;

      type = get_token_type_of_special_character(str, &str_len, &read_index, line + read_index);
      read_index++;
      c = *(line + read_index);

      str[str_len] = L'\0';
    }
    else if (isdigit(c)) {
      while (isdigit(c) || c == L'.' || c == L'f') {
	str[str_len] = c;
	str_len++;
	read_index++;
	c = *(line + read_index);
      }
      str[str_len] = L'\0';

      type = TokNumberLiteral;
    }

    update_line_number_index();
    Token* tok = (Token*)safe_malloc(sizeof(Token));
    tok->str = str;
    tok->type = type;
    tok->line_number = line_number_data[line_number_index - 1];
    tok->line_start_index = line_index_data[line_number_index - 2];
    tok->line_end_index = line_index_data[line_number_index - 1];
    tok->str_index = read_index;

    return tok;
  }

  update_line_number_index();
  Token* eof_token = (Token*)safe_malloc(sizeof(Token));
  eof_token->str = L"EOF";
  eof_token->type = TokEOF;
  eof_token->line_number = line_number_data[line_number_index - 1];
  eof_token->line_start_index = line_index_data[line_number_index - 2];
  eof_token->line_end_index = line_index_data[line_number_index - 1];
  eof_token->str_index = read_index;


  return eof_token;
}
