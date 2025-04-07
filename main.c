#include "main.h"

void print_indent(int indent) {
	for (int i = 0; i < indent; i++) {
		wprintf(L"  ");
	}
}

void print_ast(void* ast, ASTType type, int indent) {
	if (!ast) return;

	print_indent(indent);

	switch (type) {
	case AST_NumberLiteral: {
		NumberLiteralAST* node = (NumberLiteralAST*)ast;
		wprintf(L"NumberLiteral: %ls\n", node->number_literal);
		break;
	}
	case AST_StringLiteral: {
		StringLiteralAST* node = (StringLiteralAST*)ast;
		wprintf(L"StringLiteral: \"%ls\"\n", node->string_literal);
		break;
	}
	case AST_Identifier: {
		IdentifierAST* node = (IdentifierAST*)ast;
		wprintf(L"Identifier: %ls\n", node->identifier);
		break;
	}
	case AST_VariableDeclaration: {
		VariableDeclarationAST* node = (VariableDeclarationAST*)ast;
		wprintf(L"VariableDeclaration: %ls : %ls\n", node->variable_name, node->variable_type);
		if (node->declaration) {
			print_indent(indent + 1);
			wprintf(L"Initialization:\n");
			print_ast(node->declaration, ((NumberLiteralAST*)node->declaration)->TYPE, indent + 2);
		}
		break;
	}
	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* node = (VariableDeclarationBundleAST*)ast;
		wprintf(L"VariableDeclarationBundle:\n");
		for (int i = 0; i < node->variable_count; i++) {
			print_ast(node->variable_declarations[i], AST_VariableDeclaration, indent + 1);
		}
		break;
	}
	case AST_BinExpr: {
		BinExprAST* node = (BinExprAST*)ast;
		wprintf(L"BinaryExpression (%d):\n", node->opType);
		print_ast(node->left, ((NumberLiteralAST*)node->left)->TYPE, indent + 1);
		print_ast(node->right, ((NumberLiteralAST*)node->right)->TYPE, indent + 1);
		break;
	}
	case AST_UnaryExpr: {
		UnaryExprAST* node = (UnaryExprAST*)ast;
		wprintf(L"UnaryExpression:\n");
		print_ast(node->expr, ((NumberLiteralAST*)node->expr)->TYPE, indent + 1);
		break;
	}
	case AST_IfStatement: {
		IfStatementAST* node = (IfStatementAST*)ast;
		wprintf(L"IfStatement:\n");
		print_indent(indent + 1);
		wprintf(L"Condition:\n");
		print_ast(node->condition, ((NumberLiteralAST*)node->condition)->TYPE, indent + 2);
		print_indent(indent + 1);
		wprintf(L"Body:\n");
		for (int i = 0; i < node->body_count; i++) {
			print_ast(node->body[i], ((NumberLiteralAST*)node->body[i])->TYPE, indent + 2);
		}
		break;
	}
	case AST_FunctionDeclaration: {
		FunctionDeclarationAST* node = (FunctionDeclarationAST*)ast;
		wprintf(L"FunctionDeclaration: %ls -> %ls\n", node->function_name, node->return_type);
		print_indent(indent + 1);
		wprintf(L"Parameters:\n");
		print_ast(node->parameters, AST_VariableDeclarationBundle, indent + 2);
		print_indent(indent + 1);
		wprintf(L"Body:\n");
		for (int i = 0; i < node->body_count; i++) {
			print_ast(node->body[i], ((NumberLiteralAST*)node->body[i])->TYPE, indent + 2);
		}
		break;
	}
	case AST_FunctionCall: {
		FunctionCallAST* node = (FunctionCallAST*)ast;
		wprintf(L"FunctionCall: %ls\n", node->function_name);
		print_indent(indent + 1);
		wprintf(L"Parameters : \n");

		int i;
		for (i = 0; i < node->parameter_count; i++) {
			print_ast(node->parameters[i], *((ASTType*)node->parameters[i]), indent + 2);
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
	wchar_t* str = L"func add(a: int, b: int): void { var a: int = 0; print(a + 3); }";

	//	print_tokens(str);

	void* ast = parse(str);
	print_ast(ast, *((ASTType*)ast), 0);

	printf("%S", generate_ir(ast));

	return 0;
}