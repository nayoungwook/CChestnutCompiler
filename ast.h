#pragma once

typedef enum {
	AST_NumberLiteral = 0,
	AST_Identifier = 1,
	AST_VariableDeclaration = 2,
	AST_VariableDeclarationBundle = 3,
	AST_BinExpr = 4,
	AST_IfStatement = 5,
	AST_UnaryExpr = 6,
	AST_FunctionDeclaration = 7,
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
	wchar_t* variable_type;
	void* declaration;
} VariableDeclarationAST;

typedef struct {
	ASTType TYPE;
	VariableDeclarationAST** variable_declarations;
	int variable_count;
} VariableDeclarationBundleAST;

typedef enum { OpADD, OpSUB, OpMUL, OpDIV, OpEQUAL, OpNOTEQUAL, OpGREATER, OpLESSER, OpEQUALGREATER, OpEQUALLESSER } OperatorType;

typedef struct {
	ASTType TYPE;
	void* left, * right;
	OperatorType opType;
}BinExprAST;

typedef struct {
	ASTType TYPE;
	void* expr;
}UnaryExprAST;

typedef struct {
	ASTType TYPE;
	void* condition;
	void** body;
	int body_count;
} IfStatementAST;

typedef struct {
	ASTType TYPE;
	wchar_t* function_name;
	wchar_t* return_type;
	VariableDeclarationBundleAST* parameters;
	void** body;
	int body_count;
} FunctionDeclarationAST;