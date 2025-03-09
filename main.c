#include "main.h"

void print_indent(int depth) {
	for (int i = 0; i < depth; i++) {
		wprintf(L"  ");
	}
}

void print_ast(void* node, ASTType type, int depth) {
	if (!node) return;

	print_indent(depth);
	switch (type) {
	case AST_NumberLiteral: {
		NumberLiteralAST* ast = (NumberLiteralAST*)node;
		wprintf(L"NumberLiteral: %ls\n", ast->number_literal);
		break;
	}
	case AST_Identifier: {
		IdentifierAST* ast = (IdentifierAST*)node;
		wprintf(L"Identifier: %ls\n", ast->identifier);
		break;
	}
	case AST_VariableDeclaration: {
		VariableDeclarationAST* ast = (VariableDeclarationAST*)node;
		wprintf(L"VariableDeclaration: %ls : %ls\n", ast->variable_name, ast->variable_type);
		if (ast->declaration) {
			print_indent(depth + 1);
			wprintf(L"Value:\n");
			print_ast(ast->declaration, ((NumberLiteralAST*)ast->declaration)->TYPE, depth + 2);
		}
		break;
	}
	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* ast = (VariableDeclarationBundleAST*)node;
		wprintf(L"VariableDeclarationBundle:\n");
		int i;
		for (i = 0; i < ast->variable_count; i++) {
			print_ast(ast->variable_declarations[i], AST_VariableDeclaration, depth + 1);
		}
		break;
	}
	case AST_BinExpr: {
		BinExprAST* ast = (BinExprAST*)node;
		wprintf(L"BinaryExpression: %d\n", ast->opType);
		print_indent(depth + 1);
		wprintf(L"Left:\n");
		print_ast(ast->left, ((NumberLiteralAST*)ast->left)->TYPE, depth + 2);
		print_indent(depth + 1);
		wprintf(L"Right:\n");
		print_ast(ast->right, ((NumberLiteralAST*)ast->right)->TYPE, depth + 2);
		break;
	}
	case AST_UnaryExpr: {
		UnaryExprAST* ast = (UnaryExprAST*)node;
		wprintf(L"UnaryExpression:\n");
		print_ast(ast->expr, ((NumberLiteralAST*)ast->expr)->TYPE, depth + 1);
		break;
	}
	case AST_IfStatement: {
		IfStatementAST* ast = (IfStatementAST*)node;
		wprintf(L"IfStatement:\n");
		print_indent(depth + 1);
		wprintf(L"Condition:\n");
		print_ast(ast->condition, ((NumberLiteralAST*)ast->condition)->TYPE, depth + 2);
		wprintf(L"Body:\n");
		for (int i = 0; i < ast->body_count; i++) {
			print_ast(ast->body[i], ((NumberLiteralAST*)ast->body[i])->TYPE, depth + 1);
		}
		break;
	}
	case AST_FunctionDeclaration: {
		FunctionDeclarationAST* ast = (FunctionDeclarationAST*)node;
		wprintf(L"FunctionDeclaration: %ls -> %ls\n", ast->function_name, ast->return_type);
		print_indent(depth + 1);
		wprintf(L"Parameters:\n");
		print_ast(ast->parameters, AST_VariableDeclarationBundle, depth + 2);
		print_indent(depth + 1);
		wprintf(L"Body:\n");
		for (int i = 0; i < ast->body_count; i++) {
			print_ast(ast->body[i], ((NumberLiteralAST*)ast->body[i])->TYPE, depth + 2);
		}
		break;
	}
	default:
		wprintf(L"Unknown AST Type\n");
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
	wchar_t* str = L"func add(a: int, b: int): void { var a: int = 10; }";

	void* ast = parse(str);
	print_ast(ast, *((ASTType*)ast), 0);

	return 0;
}