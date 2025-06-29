#include "ir.h"

extern SymbolTable* variable_symbol_table;
static int label_id = 0;

void new_line(wchar_t** result, int indentation) {
	int i;
	*result = join_string(*result, L"\n");
	for (i = 0; i < indentation; i++) {
		*result = join_string(*result, L"  ");
	}
}

wchar_t* generate_ir(void* ast, int indentation) {

	wchar_t* result = L"";
	int i;

	switch (*((ASTType*)ast)) {

	case AST_Return: {
		ReturnAST* return_ast = (ReturnAST*)ast;

		result = join_string(result, generate_ir(return_ast->expression, indentation));

		new_line(&result, indentation);
		wchar_t* ret_str_buffer[128];
		swprintf(ret_str_buffer, 128, L"@ret");
		result = join_string(result, ret_str_buffer);
		break;
	}

	case AST_BinExpr: {
		BinExprAST* bin_expr_ast = (BinExprAST*)ast;

		result = join_string(result, generate_ir(bin_expr_ast->left, indentation));
		result = join_string(result, generate_ir(bin_expr_ast->right, indentation));

		wchar_t* operator_str_buffer[128];
		switch (bin_expr_ast->opType) {
		case OpADD:
			swprintf(operator_str_buffer, 128, L"@add");
			break;
		case OpSUB:
			swprintf(operator_str_buffer, 128, L"@sub");
			break;
		case OpMUL:
			swprintf(operator_str_buffer, 128, L"@mul");
			break;
		case OpDIV:
			swprintf(operator_str_buffer, 128, L"@div");
			break;
		case OpEQUAL:
			swprintf(operator_str_buffer, 128, L"@equal");
			break;
		case OpNOTEQUAL:
			swprintf(operator_str_buffer, 128, L"@notequal");
			break;
		case OpGREATER:
			swprintf(operator_str_buffer, 128, L"@greater");
			break;
		case OpLESSER:
			swprintf(operator_str_buffer, 128, L"@lesser");
			break;
		case OpEQUALGREATER:
			swprintf(operator_str_buffer, 128, L"@eqgreater");
			break;
		case OpEQUALLESSER:
			swprintf(operator_str_buffer, 128, L"@eqlesser");
			break;
		case OpOR:
			swprintf(operator_str_buffer, 128, L"@or");
			break;
		case OpAND:
			swprintf(operator_str_buffer, 128, L"@and");
			break;
		}

		new_line(&result, indentation);
		result = join_string(result, operator_str_buffer);

		break;
	}

	case AST_Identifier: {
		IdentifierAST* identifier_ast = (IdentifierAST*)ast;

		new_line(&result, indentation);
		wchar_t identifier_str_buffer[128];
		swprintf(identifier_str_buffer, 128, L"@load %d", identifier_ast->index);
		result = join_string(result, identifier_str_buffer);

		break;
	}

	case AST_StringLiteral: {
		StringLiteralAST* string_literal_ast = (StringLiteralAST*)ast;


		wchar_t* string_literal = string_literal_ast->string_literal;
		wchar_t push_str_buffer[128];

		new_line(&result, indentation);

		swprintf(push_str_buffer, 128, L"@push string %s", string_literal);

		result = join_string(result, push_str_buffer);

		break;
	}

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;

		for (int i = 0; i < function_call_ast->parameter_count; i++) {
			result = join_string(result, generate_ir(function_call_ast->parameters[i], indentation));
		}

		break;
	}

	case AST_NumberLiteral: {
		NumberLiteralAST* number_literal_ast = (NumberLiteralAST*)ast;

		wchar_t* number_literal = number_literal_ast->number_literal;
		wchar_t push_str_buffer[128];

		new_line(&result, indentation);
		swprintf(push_str_buffer, 128, L"@push %s %s", number_literal_ast->numeric_type, number_literal);

		result = join_string(result, push_str_buffer);

		break;
	}

	case AST_FunctionDeclaration: {
		FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)ast;

		wchar_t* parameter_buffer = L"";

		VariableDeclarationBundleAST* parameters_ast = ((VariableDeclarationBundleAST*)function_declaration_ast->parameters);

		for (i = 0; i < parameters_ast->variable_count; i++) {
			VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

			wchar_t single_parameter_buffer[512];
			swprintf(single_parameter_buffer, 512, L"%ls %d ", parameter->variable_type, parameter->index);

			parameter_buffer = join_string(parameter_buffer, single_parameter_buffer);

		}

		wchar_t buffer[256];
		swprintf(buffer, 256, L". %ls %ls{", function_declaration_ast->function_name, parameter_buffer);
		result = join_string(result, buffer);

		for (i = 0; i < function_declaration_ast->body_count; i++) {
			result = join_string(result, generate_ir(function_declaration_ast->body[i], indentation + 1));
		}

		result = join_string(result, L"\n}");

		break;
	}

	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)ast;

		for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
			result = join_string(result, generate_ir(variable_declaration_bundle_ast->variable_declarations[i], indentation));
		}

		break;
	}

	case AST_VariableDeclaration: {
		VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)ast;

		if (variable_declaration_ast->declaration) {
			result = join_string(result, generate_ir(variable_declaration_ast->declaration, indentation));
		}

		wchar_t store_str_buffer[128];

		new_line(&result, indentation);
		swprintf(store_str_buffer, 128, L"@store %d", variable_declaration_ast->index);

		result = join_string(result, store_str_buffer);

		break;
	}

	case AST_IdentIncrease: {
		IdentIncreaseAST* ident_increase_ast = (IdentIncreaseAST*)ast;

		new_line(&result, indentation);
		wchar_t ident_increase_str_buffer[128];
		swprintf(ident_increase_str_buffer, 128, L"@inc %d", ident_increase_ast->index);
		result = join_string(result, ident_increase_str_buffer);

		break;
	}

	case AST_IdentDecrease: {
		IdentDecreaseAST* ident_decrease_ast = (IdentDecreaseAST*)ast;

		new_line(&result, indentation);
		wchar_t ident_decrease_str_buffer[128];
		swprintf(ident_decrease_str_buffer, 128, L"@dec %d", ident_decrease_ast->index);
		result = join_string(result, ident_decrease_str_buffer);

		break;
	}

	case AST_IfStatement: {
		IfStatementAST* if_statement_ast = (IfStatementAST*)ast;

		open_scope();



		close_scope();

		break;
	}

	case AST_ForStatement: {
		ForStatementAST* for_statement_ast = (ForStatementAST*)ast;

		result = join_string(result, generate_ir(for_statement_ast->init, indentation));

		label_id++;
		int end_label_id = label_id;
		label_id++;
		int begin_label_id = label_id;

		wchar_t goto_str_buffer[64];
		wchar_t label_str_buffer[64];
		wchar_t for_str_buffer[64];

		swprintf(goto_str_buffer, 64, L"@goto %x", end_label_id);
		new_line(&result, indentation);
		result = join_string(result, goto_str_buffer);

		swprintf(label_str_buffer, 64, L"@label %x", begin_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		for (i = 0; i < for_statement_ast->body_count; i++) {
			result = join_string(result, generate_ir(for_statement_ast->body[i], indentation));
		}

		result = join_string(result, generate_ir(for_statement_ast->step, indentation));

		swprintf(label_str_buffer, 64, L"@label %x", end_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		result = join_string(result, generate_ir(for_statement_ast->condition, indentation));

		swprintf(label_str_buffer, 64, L"@for %x", begin_label_id);
		new_line(&result, indentation);
		result = join_string(result, label_str_buffer);

		break;
	}
	}

	return result;
}