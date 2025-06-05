#include "main.h"

#include <limits.h>

void print_indent(int indent) {
	int i;
	for (i = 0; i < indent; i++) {
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
		int i;
		for (i = 0; i < node->variable_count; i++) {
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
	case AST_ForStatement: {
		ForStatementAST* node = (ForStatementAST*)ast;
		wprintf(L"ForStatement:\n");

		print_indent(indent + 1);
		wprintf(L"Init:\n");
		print_ast(node->init, ((NumberLiteralAST*)node->init)->TYPE, indent + 2);

		print_indent(indent + 1);
		wprintf(L"Condition:\n");
		print_ast(node->condition, ((NumberLiteralAST*)node->condition)->TYPE, indent + 2);

		print_indent(indent + 1);
		wprintf(L"Step:\n");
		print_ast(node->step, ((NumberLiteralAST*)node->step)->TYPE, indent + 2);

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
	case AST_IdentIncrease: {
		IdentIncreaseAST* node = (IdentIncreaseAST*)ast;
		wprintf(L"Identifier Increase: %ls\n", node->identifier);
		break;
	}
	case AST_IdentDecrease: {
		IdentDecreaseAST* node = (IdentDecreaseAST*)ast;
		wprintf(L"Identifier Decrease: %ls\n", node->identifier);
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

wchar_t* read_file(char* const file_path) {
	FILE* fp = fopen(file_path, "r, ccs=UTF-8");
	if (!fp) {
		perror("파일 열기 실패");
		return 1;
	}

	fseek(fp, 0, SEEK_END);
	long byte_len = ftell(fp);
	rewind(fp);

	size_t wchar_estimate = byte_len + 1;
	wchar_t* str = (wchar_t*)malloc(sizeof(wchar_t) * wchar_estimate);
	if (str == NULL) {
		perror("메모리 할당 실패");
		fclose(fp);
		return 1;
	}

	size_t i = 0;
	while (fgetws(str + i, (int)(wchar_estimate - i), fp)) {
		i = wcslen(str);
	}

	return str;
}

int main(int arc, char* args[]) {
	setlocale(LC_ALL, "");
	
	wchar_t* file = read_file("main.cn");

	void* ast = parse(file);

	print_ast(ast, *((ASTType*)ast), 0);

	symbol_table = (SymbolTable*)malloc(sizeof(SymbolTable)); // for global scope.
	symbol_table->size = 0;
	printf("%S", generate_ir(ast));

	return 0;
}