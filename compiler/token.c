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

wchar_t* read_file(TokenizerContext* tokenizer_context, const wchar_t* file_path) {
  FILE* fp = _wfopen(file_path, L"r");
  if (!fp) {
    perror("Failed to open file");
    return 1;
  }

  fseek(fp, 0, SEEK_END);
  long byte_len = ftell(fp);
  rewind(fp);

  size_t wchar_estimate = byte_len + 1;
  wchar_t* str = (wchar_t*)safe_malloc(sizeof(wchar_t) * wchar_estimate);

  int capacity = 1;
  int size = 0;
  tokenizer_context->line_index_data = (int*)safe_malloc(sizeof(int));
  tokenizer_context->line_number_data = (int*)safe_malloc(sizeof(int));
  int line_number = 0;

  size_t i = 0;
  while (fgetws(str + i, (int)(wchar_estimate - i), fp)) {
    i = wcslen(str);

    if (size >= capacity) {
      capacity *= 2;
      tokenizer_context->line_index_data = (int*)safe_realloc(tokenizer_context->line_index_data, sizeof(int) * capacity);
      tokenizer_context->line_number_data = (int*)safe_realloc(tokenizer_context->line_number_data, sizeof(int) * capacity);
    }

    line_number++;
    tokenizer_context->line_index_data[size] = i;
    tokenizer_context->line_number_data[size] = line_number;

    size++;
  }
  
  return str;
}

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

TokenizerContext* create_tokenizer_context(){
  TokenizerContext* result = (TokenizerContext*)safe_malloc(sizeof(TokenizerContext));

  result->read_index = 0;
  result->token_cache = NULL;
  result->line_index_data = NULL;
  result->line_number_data = NULL;
  result->line_number_index = 1;
  
  return result;
}

Token* peek_token(TokenizerContext* tokenizer_context, wchar_t* line) {
  int backup_read_index = tokenizer_context->read_index;
  Token* result = tokenizer_context->token_cache;
  if (!result) {
    result = pull_token(tokenizer_context, line);
    tokenizer_context->read_index = backup_read_index;
    tokenizer_context->token_cache = result;
  }

  return result;
}

void update_line_number_index(TokenizerContext* tokenizer_context) {
  while (tokenizer_context->line_index_data[tokenizer_context->line_number_index - 1] < tokenizer_context->read_index) {
    tokenizer_context->line_number_index++;
  }
}

Token* pull_token(TokenizerContext* tokenizer_context, wchar_t* line) {
  tokenizer_context->token_cache = NULL;

  wchar_t* str = (wchar_t*)safe_malloc(sizeof(wchar_t) * MAX_TOKEN_STR);
  int str_len = 0;
  TokenType type = TokEOF;
  wchar_t c = *(line + tokenizer_context->read_index);

  while (c != '\0') {
    str_len = 0;
    memset(str, L'\0', sizeof(wchar_t) * MAX_TOKEN_STR);
    if (isspace(c)) {
      tokenizer_context->read_index++;
      c = *(line + tokenizer_context->read_index);
      continue;
    }

    if (isalpha(c)) {
      while (isalnum(c) || c == L'_') {
	str[str_len] = c;
	str_len++;
	tokenizer_context->read_index++;
	c = *(line + tokenizer_context->read_index);
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

      type = get_token_type_of_special_character(str, &str_len, &tokenizer_context->read_index, line + tokenizer_context->read_index);
      tokenizer_context->read_index++;
      c = *(line + tokenizer_context->read_index);

      str[str_len] = L'\0';
    }
    else if (isdigit(c)) {
      while (isdigit(c) || c == L'.' || c == L'f') {
	str[str_len] = c;
	str_len++;
	tokenizer_context->read_index++;
	c = *(line + tokenizer_context->read_index);
      }
      str[str_len] = L'\0';

      type = TokNumberLiteral;
    }

    update_line_number_index(tokenizer_context);

    Token* tok = (Token*)safe_malloc(sizeof(Token));
    tok->str = str;
    tok->type = type;
    tok->line_number = tokenizer_context->line_number_data[tokenizer_context->line_number_index - 1];
    tok->line_start_index = tokenizer_context->line_index_data[tokenizer_context->line_number_index - 2];
    tok->line_end_index = tokenizer_context->line_index_data[tokenizer_context->line_number_index - 1];
    tok->str_index = tokenizer_context->read_index;

    return tok;
  }

  update_line_number_index(tokenizer_context);
  Token* eof_token = (Token*)safe_malloc(sizeof(Token));
  eof_token->str = L"EOF";
  eof_token->type = TokEOF;
  eof_token->line_number = tokenizer_context->line_number_data[tokenizer_context->line_number_index - 1];
  eof_token->line_start_index = tokenizer_context->line_index_data[tokenizer_context->line_number_index - 2];
  eof_token->line_end_index = tokenizer_context->line_index_data[tokenizer_context->line_number_index - 1];
  eof_token->str_index = tokenizer_context->read_index;


  return eof_token;
}
