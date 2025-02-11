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
	get_token_type_of_special_character(wchar_t* c) {

	enum TokenType type = TokEOF;

	switch (*c) {

	case L'=': {
		type = TokAssign;
		if (*(c + 1) == L'=') {
			type = TokEqual;
			c++;
		}
		break;
	}

	case L'!': {
		type = TokNot;
		if (*(c + 1) == L'=') {
			type = TokNotEqual;
			c++;
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
		}
		break;
	}

	case L'>': {
		type = TokGreater;
		if (*(c + 1) == L'=') {
			type = TokEqualGreater;
			c++;
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
		type = TokPlus;
		if (*(c + 1) == L'=') {
			type = TokPlusAssign;
			c++;
		}
		if (*(c + 1) == L'+') {
			type = TokIncrease;
			c++;
		}

		break;
	}

	case L'-': {
		type = TokMinus;
		if (*(c + 1) == L'=') {
			type = TokMinusAssign;
			c++;
		}
		if (*(c + 1) == L'-') {
			type = TokDecrease;
			c++;
		}

		break;
	}

	case L'*': {
		type = TokMult;
		if (*(c + 1) == L'=') {
			type = TokMultAssign;
			c++;
		}
		if (*(c + 1) == L'*') {
			type = TokPow;
			c++;
		}

		break;
	}

	case L'/': {
		type = TokDiv;
		if (*(c + 1) == L'=') {
			type = TokDivAssign;
			c++;
		}
		break;
	}
#pragma endregion BASIC_OPS
	}

	return type;
}

Token pull_token(wchar_t** line) {
	int result_len = 0;

	wchar_t* str = (wchar_t*)malloc(sizeof(wchar_t) * MAX_TOKEN_STR);
	wchar_t* c = *line;
	int str_len = 0;
	enum TokenType type = TokEOF;

	while (*c != '\0') {
		str_len = 0;
		memset(str, L' ', sizeof(wchar_t) * MAX_TOKEN_STR);

		if (isspace(*c)) {
			c++;
			continue;
		}

		if (isalpha(*c)) {
			while (isalnum(*c) || *c == L'_') {
				str[str_len] = *c;
				str_len++;
				c++;
			}
			str[str_len] = L'\0';

			type = TokIdent;

			if (!strcmp(str, L"var")) {
				type = TokVar;
			}
			else if (!strcmp(str, L"if")) {
				type = TokIf;
			}
			else if (!strcmp(str, L"for")) {
				type = TokFor;
			}
		}
		else if (is_special_character(*c)) {
			str[str_len] = *c;
			str_len++;

			type = get_token_type_of_special_character(c);
			c++;

			str[str_len] = L'\0';
		}
		else if (isdigit(*c)) {
			while (isdigit(*c) || *c == L'.') {
				str[str_len] = *c;
				str_len++;
				c++;
			}
			str[str_len] = L'\0';

			type = TokNumberLiteral;
		}

		*line = c;
		Token tok = { str, type };

		return tok;
	}

	line = c;
	Token eof_token = { L"EOF" , TokEOF };

	return eof_token;
}