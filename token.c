#include "token.h"

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

enum TokenType
	get_token_type_of_special_character(wchar_t* str, int* str_len, int* read_index, wchar_t* c) {

	enum TokenType type = TokEOF;

	switch (*c) {

	case L'=': {
		type = TokAssign;
		if (*(c + 1) == L'=') {
			type = TokEqual;
			c++;

			str[*str_len] = c;
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

			str[*str_len] = c;
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

			str[*str_len] = c;
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

			str[*str_len] = c;
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

#pragma region BASIC_OPS

	case L'+': {
		type = TokAdd;
		if (*(c + 1) == L'=') {
			type = TokPlusAssign;
			c++;

			str[*str_len] = c;
			(*str_len)++;
			(*read_index)++;
		}
		if (*(c + 1) == L'+') {
			type = TokIncrease;
			c++;

			str[*str_len] = c;
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

			str[*str_len] = c;
			(*str_len)++;
			(*read_index)++;
		}
		if (*(c + 1) == L'-') {
			type = TokDecrease;
			c++;

			str[*str_len] = c;
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

			str[*str_len] = c;
			(*str_len)++;
			(*read_index)++;
		}
		if (*(c + 1) == L'*') {
			type = TokPow;
			c++;

			str[*str_len] = c;
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

Token* peek_token(wchar_t* line) {
	int backup_read_index = read_index;
	Token* result = pull_token(line);
	read_index = backup_read_index;
	return result;
}

Token* pull_token(wchar_t* line) {
	int result_len = 0;

	wchar_t* str = (wchar_t*)malloc(sizeof(wchar_t) * MAX_TOKEN_STR);
	int str_len = 0;
	enum TokenType type = TokEOF;
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

			if (!wcscmp(str, L"var")) {
				type = TokVar;
			}
			else if (!wcscmp(str, L"if")) {
				type = TokIf;
			}
			else if (!wcscmp(str, L"for")) {
				type = TokFor;
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
			while (isdigit(c) || c == L'.') {
				str[str_len] = c;
				str_len++;
				read_index++;
				c = *(line + read_index);
			}
			str[str_len] = L'\0';

			type = TokNumberLiteral;
		}

		Token* tok = (Token*)malloc(sizeof(Token));
		tok->str = str;
		tok->type = type;

		return tok;
	}

	Token* eof_token = (Token*)malloc(sizeof(Token));
	eof_token->str = L"EOF";
	eof_token->type = TokEOF;

	return &eof_token;
}