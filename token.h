#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define MAX_TOKEN_STR 512

enum TokenType {
	TokEOF = -1,
	TokIdent = 0,
	TokNumberLiteral = 1,
	TokStringLiteral = 2,
	TokCharacterLiteral = 3,
	TokBoolLiteral = 4,

	TokEqual = 5,
	TokNotEqual = 6,
	TokSharp = 7,
	TokLesser = 8,
	TokEqualLesser = 9,
	TokGreater = 10,
	TokEqualGreater = 11,
	TokPlus = 12,
	TokMinus = 13,
	TokMult = 14,
	TokDiv = 15,
	TokLParen = 16,
	TokRParen = 17,
	TokLBracket = 18,
	TokRBracket = 19,
	TokLSquareBracket = 20,
	TokRSquareBracket = 21,
	TokAssign = 22,
	TokSemiColon = 24,
	TokColon = 25,
	TokComma = 26,
	TokDot = 27,

	TokPlusAssign = 28,
	TokMinusAssign = 29,
	TokMultAssign = 30,
	TokDivAssign = 31,

	TokIncrease = 32,
	TokDecrease = 33,
	TokPow = 33,

	TokNot = 34,

	TokVar = 35,
	TokIf = 36,
	TokFor = 37,
};

typedef struct _Token {
	wchar_t* str;
	enum TokenType type;
} Token;

short is_special_character(const wchar_t wc);
Token* pull_token(wchar_t** line);