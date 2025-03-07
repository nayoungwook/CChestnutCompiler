#include "main.h"

void printAST(void* node, int depth) {
	if (!node) return;

	for (int i = 0; i < depth; i++) {
		printf("  ");
	}

	ASTType type = *(ASTType*)node;
	switch (type) {
	case AST_NumberLiteral: {
		NumberLiteralAST* num = (NumberLiteralAST*)node;
		wprintf(L"Number: %ls\n", num->number_literal);
		break;
	}
	case AST_Identifier: {
		IdentifierAST* ident = (IdentifierAST*)node;
		wprintf(L"Identifier: %ls\n", ident->identifier);
		break;
	}
	case AST_VariableDeclaration: {
		VariableDeclarationAST* varDecl = (VariableDeclarationAST*)node;
		wprintf(L"Variable Declaration: %ls\n", varDecl->variable_name);
		printAST(varDecl->declaration, depth + 1);
		break;
	}
	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* bundle = (VariableDeclarationBundleAST*)node;
		printf("Variable Declaration Bundle:\n");
		for (int i = 0; i < bundle->variable_count; i++) {
			printAST(bundle->variable_declarations[i], depth + 1);
		}
		break;
	}
	case AST_BinExpr: {
		BinExprAST* binExpr = (BinExprAST*)node;
		printf("Binary Expression: ");
		switch (binExpr->opType) {
		case OpADD: printf("+"); break;
		case OpSUB: printf("-"); break;
		case OpMUL: printf("*"); break;
		case OpDIV: printf("/"); break;
		case OpEQUAL: printf("=="); break;
		case OpNOTEQUAL: printf("!="); break;
		case OpGREATER: printf(">"); break;
		case OpLESSER: printf("<"); break;
		case OpEQUALGREATER: printf(">="); break;
		case OpEQUALLESSER: printf("<="); break;
		}
		printf("\n");
		printAST(binExpr->left, depth + 1);
		printAST(binExpr->right, depth + 1);
		break;
	}
	case AST_UnaryExpr: {
		UnaryExprAST* unaryExpr = (UnaryExprAST*)node;
		printf("Unary Expression: !\n");
		printAST(unaryExpr->expr, depth + 1);
		break;
	}
	default:
		printf("Unknown AST Node\n");
		break;
	}
}

void print_tokens(wchar_t* str) {
	while (1) {
		Token* tok = pull_token(str);

		printf("%S %d\n", tok->str, tok->type);

		if (tok->type == TokEOF) break;
	}
}

int main(int arc, char* args[]) {
	wchar_t* str = L"func main(): void {}";

	printAST(parse(str), 0);

	return 0;
}