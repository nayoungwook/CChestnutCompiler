#pragma once

typedef struct  _NumberLiteralAST {
	wchar_t* number_literal;
} NumberListerlAST;

typedef struct  _IdentifierAST {
	wchar_t* identifier;
} IdentifierAST;

typedef struct _VariableDeclarationAST {
	wchar_t** varaible_names;
	void** declaration;
} VariableDeclarationAST;