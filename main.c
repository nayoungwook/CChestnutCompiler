#include "main.h"

void print_ast(void* ast) {
	if (!ast) {
		printf("(null)\n");
		return;
	}

	ASTType type = *(ASTType*)ast; // 첫 번째 필드는 ASTType이므로 이를 기반으로 타입 판별

	switch (type) {
	case AST_NumberLiteral: {
		NumberLiteralAST* num = (NumberLiteralAST*)ast;
		wprintf(L"NumberLiteral: %ls\n", num->number_literal);
		break;
	}
	case AST_Identifier: {
		IdentifierAST* id = (IdentifierAST*)ast;
		wprintf(L"Identifier: %ls\n", id->identifier);
		break;
	}
	case AST_VariableDeclaration: {
		VariableDeclarationAST* varDecl = (VariableDeclarationAST*)ast;
		wprintf(L"VariableDeclaration: %ls\n", varDecl->variable_name);
		print_ast(varDecl->declaration);
		break;
	}
	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* bundle = (VariableDeclarationBundleAST*)ast;
		printf("VariableDeclarationBundle: %d variables\n", bundle->variable_count);
		for (int i = 0; i < bundle->variable_count; i++) {
			print_ast(bundle->variable_declarations[i]);
		}
		break;
	}
	case AST_BinExpr: {
		BinExprAST* binExpr = (BinExprAST*)ast;
		printf("Binary Expression: ");
		switch (binExpr->opType) {
		case OpADD: printf("+"); break;
		case OpSUB: printf("-"); break;
		case OpMUL: printf("*"); break;
		case OpDIV: printf("/"); break;
		}
		printf("\nLeft: \n");
		print_ast(binExpr->left);
		printf("Right: \n");
		print_ast(binExpr->right);
		break;
	}
	default:
		printf("Unknown AST Type!\n");
		break;
	}
}

int main(int arc, char* args[]) {

	wchar_t* str = L"var i: int = 10 + 5 * 2 + 5;";

	print_ast(parse(str));

	return 0;
}