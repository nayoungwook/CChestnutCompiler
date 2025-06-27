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
		wprintf(L"Statement Type : %d\n", node->if_type);

		if (node->condition != NULL) {
			print_indent(indent + 1);
			wprintf(L"Condition:\n");
			print_ast(node->condition, ((NumberLiteralAST*)node->condition)->TYPE, indent + 2);
		}

		print_indent(indent + 1);
		wprintf(L"Body:\n");
		for (int i = 0; i < node->body_count; i++) {
			print_ast(node->body[i], ((NumberLiteralAST*)node->body[i])->TYPE, indent + 2);
		}
		if (node->next_statement != NULL) {
			print_ast(node->next_statement, ((IfStatementAST*)node->next_statement)->TYPE, indent);
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
	case AST_Return: {
		ReturnAST* node = (ReturnAST*)ast;

		wprintf(L"Return:\n");
		print_ast(node->expression, *((ASTType*)node->expression), indent + 2);

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

extern SymbolTable* variable_symbol_table;
extern SymbolTable* function_symbol_table;

void initialize_global_symbol_table() {
	variable_symbol_table = (SymbolTable*)malloc(sizeof(SymbolTable)); // for global scope.
	if (variable_symbol_table) {
		variable_symbol_table->size = 0;
		variable_symbol_table->prev = NULL;
	}

	function_symbol_table = (SymbolTable*)malloc(sizeof(SymbolTable)); // for global scope
	if (function_symbol_table) {
		function_symbol_table->size = 0;
		function_symbol_table->prev = NULL;
	}
}

int main(int arc, char* args[]) {
	setlocale(LC_ALL, "");

	wchar_t* file = read_file("main.nut");

	initialize_global_symbol_table();

	while (peek_token(file)->type != TokEOF) {
		void* ast = parse(file);

		//print_ast(ast, *((ASTType*)ast), 0);

		printf("%S\n", generate_ir(ast, 0));
	}

	return 0;
}