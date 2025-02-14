#pragma once

typedef enum {
	AST_NumberLiteral = 0,
	AST_Identifier = 1,
	AST_VariableDeclaration = 2,
	AST_VariableDeclarationBundle = 3,
	AST_BinExpr = 4,
}ASTType;

typedef struct {
	ASTType TYPE;
	wchar_t* number_literal;
} NumberLiteralAST;

typedef struct {
	ASTType TYPE;
	wchar_t* identifier;
} IdentifierAST;

typedef struct {
	ASTType TYPE;
	wchar_t* variable_name;
	void* declaration;
} VariableDeclarationAST;

typedef struct {
	ASTType TYPE;
	VariableDeclarationAST** variable_declarations;
	int variable_count;
} VariableDeclarationBundleAST;

typedef enum { OpADD, OpSUB, OpMUL, OpDIV } OperatorType;

typedef struct _BinExprAST {
	ASTType TYPE;
	void* left, * right;
	OperatorType opType;
}BinExprAST;
