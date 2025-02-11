#pragma once

enum ASTType {
	AST_NumberLiteral = 0,
	AST_Identifier = 1,
	AST_VariableDeclaration = 2,
	AST_VariableDeclarationBundle = 3,
};

typedef struct  _NumberLiteralAST {
	enum ASTType TYPE;
	wchar_t* number_literal;
} NumberLiteralAST;

typedef struct  _IdentifierAST {
	enum ASTType TYPE;
	wchar_t* identifier;
} IdentifierAST;

typedef struct _VariableDeclarationAST {
	enum ASTType TYPE;
	wchar_t* varaible_name;
	void* declaration;
} VariableDeclarationAST;

typedef struct _VariableDeclarationBundleAST {
	enum ASTType TYPE;
	VariableDeclarationAST* variable_declarations;
	int variable_count;
} VariableDeclarationBundleAST;
