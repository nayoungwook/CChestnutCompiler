#pragma once

#include "util.h"
#include "token.h"
#include <stdio.h>

typedef enum {
	AST_NumberLiteral = 0,
	AST_Identifier = 1,
	AST_VariableDeclaration = 2,
	AST_VariableDeclarationBundle = 3,
	AST_BinExpr = 4,
	AST_IfStatement = 5,
	AST_UnaryExpr = 6,
	AST_FunctionDeclaration = 7,
	AST_FunctionCall = 8,
	AST_StringLiteral = 9,
	AST_ForStatement = 10,
	AST_IdentIncrease = 11,
	AST_IdentDecrease = 12,
	AST_Return = 13,
	AST_Class = 14,
	AST_Constructor = 15,
	AST_New = 16,
	AST_Null = 17,
	AST_ArrayDeclaration = 18,
	AST_ArrayAccess = 19,
	AST_BoolLiteral = 20,
}ASTType;

typedef struct Type;

typedef struct {
	ASTType TYPE;
	Token* number_literal;
	const wchar_t* numeric_type;
} NumberLiteralAST;

typedef struct {
	ASTType TYPE;
	Token* string_literal;
} StringLiteralAST;

typedef struct {
	ASTType TYPE;
	Token* identifier;
	void* attribute;
} IdentifierAST;

typedef struct {
	ASTType TYPE;
	Token* tok;
	char bool_type;
} BoolLiteralAST;

typedef struct {
	ASTType TYPE;
	Token* tok;
	Token* variable_name;
	Type* variable_type;
	void* declaration;
	int access_modifier;
} VariableDeclarationAST;

typedef struct {
	ASTType TYPE;
	Token* tok;
	VariableDeclarationAST** variable_declarations;
	int variable_count;
} VariableDeclarationBundleAST;

typedef enum { OpNone, OpADD, OpSUB, OpMUL, OpDIV, OpEQUAL, OpNOTEQUAL, OpGREATER, OpLESSER, OpEQUALGREATER, OpEQUALLESSER, OpASSIGN, OpOR, OpAND } OperatorType;

typedef struct {
	ASTType TYPE;
	void* left, * right;
	OperatorType opType;
}BinExprAST;

typedef struct {
	ASTType TYPE;
	void* expr;
}UnaryExprAST;

typedef enum { StmtIf, StmtElseIf, StmtElse } IfStatementType;

typedef struct {
	ASTType TYPE;
	IfStatementType if_type;
	void* condition;
	void* next_statement;
	void** body;
	int body_count;
} IfStatementAST;

typedef struct {
	ASTType TYPE;
	Token* function_name;
	Type* return_type;
	VariableDeclarationBundleAST* parameters;
	void** body;
	int body_count;
	int access_modifier;
} FunctionDeclarationAST;

typedef struct {
	ASTType TYPE;
	Token* function_name;
	void** parameters;
	int parameter_count;
	void* attribute;
} FunctionCallAST;

typedef struct {
	ASTType TYPE;
	Token* identifier;
} IdentIncreaseAST;

typedef struct {
	ASTType TYPE;
	Token* identifier;
} IdentDecreaseAST;

typedef struct {
	ASTType TYPE;
	void* init;
	void* condition;
	void* step;
	void** body;
	int body_count;
} ForStatementAST;

typedef struct {
	ASTType TYPE;
	void* expression;
} ReturnAST;

typedef struct {
	ASTType TYPE;
	VariableDeclarationBundleAST* parameters;
	void** body;
	int body_count;
	int access_modifier;
} ConstructorAST;

typedef struct {
	ASTType TYPE;

	void* initializer;
	ConstructorAST* constructor;

	VariableDeclarationBundleAST** member_variables;
	FunctionDeclarationAST** member_functions;

	unsigned int member_variable_bundle_count;
	unsigned int member_function_count;

	Token* class_name;
	Token* parent_class_name;

} ClassAST;

typedef struct {
	ASTType TYPE;
	Token* class_name;
	void** parameters;
	int parameter_count;
} NewAST;

typedef struct {
	ASTType	TYPE;
}NullAST;

typedef struct {
	ASTType TYPE;
	unsigned int element_count;
	void** elements;
	Type* element_type;
} ArrayDeclarationAST;

typedef struct {
	ASTType TYPE;
	void** indexes;
	int access_count;
	IdentifierAST* target_array;
	void* attribute;
}ArrayAccessAST;