#include "ir.h"

extern SymbolTable* variable_symbol_table;
extern SymbolTable* function_symbol_table;
extern SymbolTable* type_symbol_table;
extern SymbolTable* class_symbol_table;
extern Set* primitive_types;
static int label_id = 0;

static int is_class_initializer = 0;
static wchar_t* current_class = L"";

int get_member_variable_index(const wchar_t* class_name, const wchar_t* ident) {
	Symbol* class_symbol = find_symbol(class_symbol_table, class_name);

	if (class_symbol == NULL) return -1;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_variables, ident);
	if (member_symbol != NULL) {
		VariableData* variable_data = member_symbol->data;
		return variable_data->index + get_parent_member_variable_count(class_name);
	}
	else {

		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_variable_index(class_data->parent_class_name, ident);
		}

		return -1;
	}

}

int get_parent_member_variable_count(const wchar_t* class_name) {
	int result = 0;

	if (wcscmp(class_name, L"")) {
		ClassData* current_class_data = find_symbol(class_symbol_table, class_name)->data;

		if (wcscmp(current_class_data->parent_class_name, L"")) {
			ClassData* parent_class_data = find_symbol(class_symbol_table, current_class_data->parent_class_name)->data;
			result += parent_class_data->member_variables->size;
			result += get_parent_member_variable_count(current_class_data->parent_class_name);
		}
	}

	return result;
}

void new_line(wchar_t** result, int indentation) {
	int i;
	*result = join_string(*result, L"\n");
	for (i = 0; i < indentation; i++) {
		*result = join_string(*result, L"  ");
	}
}

wchar_t* create_class_initializer(int indentation, ClassAST* class_ast) {
	wchar_t* result = L"";
	wchar_t* buffer[128];

	new_line(&result, indentation + 1);
	swprintf(buffer, 128, L"$initializer {");
	result = join_string(result, buffer);
	is_class_initializer = 1;

	int i;
	for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
		result = join_string(result, generate_ir(class_ast->member_variables[i], indentation + 2));
	}

	is_class_initializer = 0;
	new_line(&result, indentation + 1);
	result = join_string(result, L"}");

	return result;
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

		if (identifier_ast->index == MEMBER_VARIABLE) {
			swprintf(identifier_str_buffer, 128, L"@mload %d", get_member_variable_index(current_class, identifier_ast->identifier));
			result = join_string(result, identifier_str_buffer);
		}
		else {
			swprintf(identifier_str_buffer, 128, L"@load %d", identifier_ast->index);
			result = join_string(result, identifier_str_buffer);
		}

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

		new_line(&result, indentation);
		wchar_t* function_call_buffer[512];
		swprintf(function_call_buffer, 512, L"@call %S %d", function_call_ast->function_name, function_call_ast->parameter_count);

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

		Symbol* function_symbol = find_symbol(function_symbol_table, function_declaration_ast->function_name);

		int should_find_member_function = function_symbol == NULL && wcscmp(current_class, L"");
		if (should_find_member_function) {
			ClassData* current_class_data = find_symbol(class_symbol_table, current_class)->data;
			function_symbol = find_symbol(current_class_data->member_functions, function_declaration_ast->function_name);
		}

		FunctionData* function_data = function_symbol->data;

		new_line(&result, indentation);
		wchar_t buffer[256];
		swprintf(buffer, 256, L".%d %ls{", (function_data->index), parameter_buffer);
		result = join_string(result, buffer);

		for (i = 0; i < function_declaration_ast->body_count; i++) {
			result = join_string(result, generate_ir(function_declaration_ast->body[i], indentation + 1));
		}

		new_line(&result, indentation);
		result = join_string(result, L"}");

		break;
	}

	case AST_Class: {
		ClassAST* class_ast = (ClassAST*)ast;

		ClassData* class_data = find_symbol(class_symbol_table, class_ast->class_name)->data;
		Symbol* parent_symbol = find_symbol(class_symbol_table, class_ast->parent_class_name);
		ClassData* parent_data = NULL;

		current_class = class_ast->class_name;

		if (parent_symbol != NULL) {
			parent_data = parent_symbol->data;
		}

		wchar_t buffer[128];
		new_line(&result, indentation);
		swprintf(buffer, 128, L"class %d %d {", class_data->index, parent_data ? parent_data->index : 0);
		result = join_string(result, buffer);

		result = join_string(result, create_class_initializer(indentation, class_ast));

		int i;
		for (i = 0; i < class_ast->member_function_count; i++) {
			result = join_string(result, generate_ir(class_ast->member_functions[i], indentation + 1));
		}

		new_line(&result, indentation);
		result = join_string(result, L"}\n");

		current_class = L"";

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

		// indexing for additional parent class member variables.
		int parent_variable_count = get_parent_member_variable_count(current_class);

		wchar_t store_str_buffer[128];

		new_line(&result, indentation);

		if (is_class_initializer) {
			swprintf(store_str_buffer, 128, L"%s %d", L"@mstore", variable_declaration_ast->index + parent_variable_count);
		}
		else {
			swprintf(store_str_buffer, 128, L"%s %d", L"@store", variable_declaration_ast->index);
		}

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


		break;
	}

	case AST_ForStatement: {
		ForStatementAST* for_statement_ast = (ForStatementAST*)ast;

		open_scope();

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

		close_scope();

		break;
	}
	}

	return result;
}