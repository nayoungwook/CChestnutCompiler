#include "includes/ir.h"

static int label_id = 0;

static int is_class_initializer = 0;
static wchar_t* current_class = L"";

int get_prev_variable_index_size(SymbolTable* variable_symbol_table) {
	SymbolTable* searcher_table = variable_symbol_table;

	int _size = 0;

	while (searcher_table->prev != NULL) {
		searcher_table = searcher_table->prev;
		_size += searcher_table->size;
	}

	return _size;
}

void open_scope(ParserContext* parser_context) {
	SymbolTable* current_symbol_table = parser_context->variable_symbol_table;

	SymbolTable* new_symbol_table = create_symbol_table();
	new_symbol_table->prev = current_symbol_table;
	new_symbol_table->size = 0;

	parser_context->variable_symbol_table = new_symbol_table;
}

void close_scope(ParserContext* parser_context) {
	SymbolTable* current_symbol_table = parser_context->variable_symbol_table;
	parser_context->variable_symbol_table = parser_context->variable_symbol_table->prev;
	free(current_symbol_table);
}

FunctionData* get_member_function_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name) {
	Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

	if (class_symbol == NULL) return NULL;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
	if (member_symbol != NULL) {
		FunctionData* function_data = member_symbol->data;
		return function_data;
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_function_data(parser_context, class_data->parent_class_name, function_name);
		}

		return NULL;
	}
	return NULL;
}

int get_member_function_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* function_name) {
	Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

	if (class_symbol == NULL) return IDENTIFIER_NOT_FOUND;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_functions, function_name);
	if (member_symbol != NULL) {
		FunctionData* function_data = member_symbol->data;
		return function_data->index + get_parent_member_function_count(parser_context, class_name);
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_function_index(parser_context, class_data->parent_class_name, function_name);
		}

		return IDENTIFIER_NOT_FOUND;
	}
	return IDENTIFIER_NOT_FOUND;
}

VariableData* get_member_variable_data(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* variable_name) {
	Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

	if (class_symbol == NULL) return NULL;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
	if (member_symbol != NULL) {
		VariableData* variable_data = member_symbol->data;
		return variable_data;
	}
	else {
		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_variable_data(parser_context, class_data->parent_class_name, variable_name);
		}

		return NULL;
	}
	return NULL;
}

int get_member_variable_index(ParserContext* parser_context, const wchar_t* class_name, const wchar_t* variable_name) {
	Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, class_name);

	if (class_symbol == NULL) return IDENTIFIER_NOT_FOUND;

	ClassData* class_data = class_symbol->data;

	Symbol* member_symbol = find_symbol(class_data->member_variables, variable_name);
	if (member_symbol != NULL) {
		VariableData* variable_data = member_symbol->data;
		return variable_data->index + get_parent_member_variable_count(parser_context, class_name);
	}
	else {

		if (wcscmp(class_data->parent_class_name, L"")) {
			return get_member_variable_index(parser_context, class_data->parent_class_name, variable_name);
		}

		return IDENTIFIER_NOT_FOUND;
	}
	return IDENTIFIER_NOT_FOUND;
}

int get_parent_member_variable_count(ParserContext* parser_context, const wchar_t* class_name) {
	int result = 0;

	if (wcscmp(class_name, L"")) {
		ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, class_name)->data;

		if (wcscmp(current_class_data->parent_class_name, L"")) {
			ClassData* parent_class_data = find_symbol(parser_context->class_symbol_table, current_class_data->parent_class_name)->data;
			result += parent_class_data->member_variables->size;
			result += get_parent_member_variable_count(parser_context, current_class_data->parent_class_name);
		}
	}

	return result;
}

int get_parent_member_function_count(ParserContext* parser_context, const wchar_t* class_name) {
	int result = 0;

	if (wcscmp(class_name, L"")) {
		ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, class_name)->data;

		if (wcscmp(current_class_data->parent_class_name, L"")) {
			ClassData* parent_class_data = find_symbol(parser_context->class_symbol_table, current_class_data->parent_class_name)->data;
			result += parent_class_data->member_functions->size;
			result += get_parent_member_function_count(parser_context, current_class_data->parent_class_name);
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

wchar_t* create_class_initializer(ParserContext* parser_context, int indentation, ClassAST* class_ast) {
	wchar_t* result = L"";
	wchar_t buffer[128];

	new_line(&result, indentation + 1);
	swprintf(buffer, 128, L"$initializer {");
	result = join_string(result, buffer);

	is_class_initializer = 1;

	int i;
	for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
		result = join_string(result, create_ir(parser_context, class_ast->member_variables[i], indentation + 2));
	}

	is_class_initializer = 0;
	new_line(&result, indentation + 1);
	result = join_string(result, L"}");

	return result;
}

wchar_t* create_class_constructor_ir(ParserContext* parser_context, int indentation, ClassAST* class_ast) {
	wchar_t* result = L"";
	wchar_t buffer[128];

	new_line(&result, indentation + 1);
	swprintf(buffer, 128, L"$constructor {");
	open_scope(parser_context);
	result = join_string(result, buffer);
	is_class_initializer = 1;

	int i;
	for (i = 0; i < class_ast->member_variable_bundle_count; i++) {
		result = join_string(result, create_ir(parser_context, class_ast->member_variables[i], indentation + 2));
	}

	is_class_initializer = 0;
	new_line(&result, indentation + 1);
	result = join_string(result, L"}");
	close_scope(parser_context);

	return result;
}

wchar_t* create_parameter_buffer(ParserContext* parser_context, VariableDeclarationBundleAST* parameters_ast) {
	wchar_t* parameter_buffer = L"";

	int i;
	for (i = 0; i < parameters_ast->variable_count; i++) {
		VariableDeclarationAST* parameter = parameters_ast->variable_declarations[i];

		VariableData* variable_data = create_variable_data(parser_context->variable_symbol_table, parameter->variable_type, parameter->variable_name, parameter->access_modifier);
		insert_variable_symbol(parser_context->variable_symbol_table, parameter->variable_name, variable_data);

		wchar_t single_parameter_buffer[512];
		swprintf(single_parameter_buffer, 512, L"%ls ", parameter->variable_type->type_str);

		parameter_buffer = join_string(parameter_buffer, single_parameter_buffer);
	}

	return parameter_buffer;
}

VariableData* find_variable_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* identifier) {
	Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier);
	VariableData* result = NULL;
	if (local_symbol == NULL) {
		result = get_member_variable_data(parser_context, class_name, identifier);
	}
	else {
		result = local_symbol->data;
	}

	if (result == NULL) {
		handle_error(ER1004, tok, parser_context->current_file_name, parser_context->file_str);
	}

	return result;
}

FunctionData* find_function_data(ParserContext* parser_context, Token* tok, const wchar_t* class_name, const wchar_t* function_name, FunctionCallAST* function_call_ast) {
	Symbol* local_symbol = find_symbol(parser_context->function_symbol_table, function_name);
	FunctionData* result = NULL;

	if (local_symbol == NULL) {
		result = get_member_function_data(parser_context, class_name, function_name);
	}
	else {
		FunctionData* function_data = local_symbol->data;
		result = function_data;
	}

	if (result == NULL) {
		handle_error(ER1003, tok, parser_context->current_file_name, parser_context->file_str);
	}

	return result;
}

Type* infer_type(ParserContext* parser_context, void* ast, wchar_t* search_point_class_name) {
	ASTType type = *((ASTType*)(ast));

	switch (type) {
	case AST_NumberLiteral: {
		NumberLiteralAST* number_ast = (NumberLiteralAST*)ast;
		Type* result = (Type*)safe_malloc(sizeof(Type));
		result->type_str = number_ast->numeric_type;
		result->array_element_type = NULL;
		result->is_array = 0;
		return result;
	}
	case AST_StringLiteral: {
		Type* result = (Type*)safe_malloc(sizeof(Type));
		result->type_str = L"string";
		result->array_element_type = NULL;
		result->is_array = 0;
		return result;
	}
	case AST_Identifier: {
		IdentifierAST* ident_ast = (IdentifierAST*)ast;

		if (!wcscmp(ident_ast->identifier, L"this")) {
			Type* result = (Type*)safe_malloc(sizeof(Type));
			result->type_str = current_class;
			result->is_array = 0;
			result->array_element_type = NULL;

			return result;
		}

		VariableData* data = find_variable_data(parser_context, ident_ast->tok, search_point_class_name, ident_ast->identifier);

		return data->type;
	}
	case AST_BinExpr: {
		BinExprAST* bin_expr_ast = (BinExprAST*)ast;

		Type* left_type = get_type_of_last_element(parser_context, bin_expr_ast->left, search_point_class_name);
		Type* right_type = get_type_of_last_element(parser_context, bin_expr_ast->right, search_point_class_name);

		if (check_castability(parser_context, left_type, right_type)) {
			return right_type;
		}
		else if (check_castability(parser_context, right_type, left_type)) {
			return left_type;
		}
		else {
			// Both types are can not be computed.
			printf(L"[Temporary error] in parser.c Type %s and %s are can not be computed.", left_type, right_type);
			abort();
		}
		break;

	case AST_VariableDeclaration: {
		VariableDeclarationAST* variable_declaration_ast = (VariableDeclarationAST*)ast;

		return variable_declaration_ast->variable_type;

		break;
	}
	}

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = (FunctionCallAST*)ast;

		FunctionData* function_data = find_function_data(parser_context, function_call_ast->tok, search_point_class_name, function_call_ast->function_name, ast);

		return function_data->return_type;
	}

	case AST_IdentIncrease:
	case AST_IdentDecrease: {
		if (type == AST_IdentIncrease) {
			IdentIncreaseAST* ident_increase = (IdentIncreaseAST*)ast;

			VariableData* data = find_variable_data(parser_context, ident_increase->tok, search_point_class_name, ident_increase->identifier);
			return data->type;
		}

		if (type == AST_IdentDecrease) {
			IdentDecreaseAST* ident_decrease = (IdentDecreaseAST*)ast;

			VariableData* data = find_variable_data(parser_context, ident_decrease->tok, search_point_class_name, ident_decrease->identifier);

			return data->type;
		}
	}

	case AST_New: {
		NewAST* new_ast = (NewAST*)ast;

		Type* result = (Type*)safe_malloc(sizeof(Type));
		result->type_str = new_ast->class_name;
		result->is_array = 0;
		result->array_element_type = NULL;

		return result;
	}

	case AST_ArrayAccess: {
		ArrayAccessAST* array_access_ast = (ArrayAccessAST*)ast;

		Type* result = infer_type(parser_context, array_access_ast->target_array, search_point_class_name);

		int i;
		for (i = 0; i < array_access_ast->access_count; i++) {
			result = result->array_element_type;
		}

		return result;
	}

	case AST_ArrayDeclaration: {
		ArrayDeclarationAST* array_declaration_ast = (ArrayDeclarationAST*)ast;

		Type* element_type = NULL;

		int i;
		for (i = 0; i < array_declaration_ast->element_count; i++) {
			if (element_type == NULL) {
				element_type = infer_type(parser_context, array_declaration_ast->elements[i], current_class);
			}
			else {
				int elements_type_check = is_same_type(element_type, infer_type(parser_context, array_declaration_ast->elements[i], current_class));
				if (!elements_type_check) {
					// Handle error for un-mathced types of elements
				}
			}
		}

		if (element_type == NULL) {
			element_type = (Type*)safe_malloc(sizeof(Type));
			element_type->type_str = L"null";
			element_type->is_array = 0;
			element_type->array_element_type = NULL;
		}

		Type* result = (Type*)safe_malloc(sizeof(Type));
		result->array_element_type = element_type;
		result->is_array = 1;
		result->type_str = L"array";

		return result;
	}
	}
}

int check_super_class(ParserContext* parser_context, const wchar_t* from, const wchar_t* to) {
	Symbol* from_symbol = find_symbol(parser_context->class_hierarchy, from);
	Symbol* to_symbol = find_symbol(parser_context->class_hierarchy, to);

	ClassType* from_type = ((ClassType*)from_symbol->data);
	ClassType* to_type = ((ClassType*)to_symbol->data);
	while (1) {
		// to -> next search find from.
		// up casting.
		if (is_same_type(from, to)) {
			return 1;
		}

		if (!from_type->parent_type) {
			return 0;
		}

		from_type = from_type->parent_type;
	}

	return 1;
}

int check_accessibility(ParserContext* parser_context, const wchar_t* target_class_name, int access_modifier) {
	if (access_modifier == AM_PUBLIC)return 1;

	int find_variable_in_class = !wcscmp(target_class_name, current_class);

	if (find_variable_in_class) {
		return 1;
	}
	else {
		if (!wcscmp(current_class, L"")) {
			return 0;
		}
		else {
			int is_parent = check_super_class(parser_context, target_class_name, current_class);

			return access_modifier == AM_PROTECTED && is_parent;
		}
	}
}

void create_attribute_ir(ParserContext* parser_context, const wchar_t* target_class_name, void* attribute, wchar_t** result, int indentation) {
	Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);
	ClassData* target_class = target_class_symbol->data;
	Symbol* member_symbol = NULL;

	switch (*((ASTType*)attribute)) {
	case AST_Identifier: {
		int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentifierAST*)attribute)->identifier);
		VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentifierAST*)attribute)->identifier);

		if (!check_accessibility(parser_context, target_class_name, member_variable_data->access_modifier)) {
			// handle error
			printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
			exit(1);
		}

		new_line(result, indentation);
		wchar_t attr_str_buffer[128];
		swprintf(attr_str_buffer, 128, L"@attr %d", member_variable_index);
		*result = join_string(*result, attr_str_buffer);

		if (((IdentifierAST*)attribute)->attribute != NULL) {
			create_attribute_ir(parser_context, member_variable_data->type->type_str, ((IdentifierAST*)attribute)->attribute, result, indentation);
		}

		break;
	}

	case AST_IdentIncrease: {
		IdentIncreaseAST* ident_increase_ast = (IdentIncreaseAST*)attribute;

		int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentIncreaseAST*)attribute)->identifier);
		VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentIncreaseAST*)attribute)->identifier);

		if (!check_accessibility(parser_context, target_class_name, member_variable_data->access_modifier)) {
			// handle error
			printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
			exit(1);
		}

		new_line(result, indentation);
		wchar_t ident_increase_str_buffer[128];
		swprintf(ident_increase_str_buffer, 128, L"@attr_inc %d", member_variable_data->index);
		*result = join_string(*result, ident_increase_str_buffer);

		break;
	}

	case AST_IdentDecrease: {
		IdentDecreaseAST* ident_decrease_ast = (IdentDecreaseAST*)attribute;

		int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentDecreaseAST*)attribute)->identifier);
		VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentDecreaseAST*)attribute)->identifier);

		if (!check_accessibility(parser_context, target_class_name, member_variable_data->access_modifier)) {
			// handle error
			printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
			exit(1);
		}

		new_line(result, indentation);
		wchar_t ident_decrease_str_buffer[128];
		swprintf(ident_decrease_str_buffer, 128, L"@attr_dec %d", member_variable_data->index);
		*result = join_string(*result, ident_decrease_str_buffer);

		break;
	}

	case AST_FunctionCall: {
		FunctionCallAST* function_call_ast = ((FunctionCallAST*)attribute);
		int member_function_index = get_member_function_index(parser_context, target_class_name, function_call_ast->function_name);
		FunctionData* member_function_data = get_member_function_data(parser_context, target_class_name, function_call_ast->function_name);

		wchar_t function_call_buffer[128];
		swprintf(function_call_buffer, 128, L"@attr_call %d %d", member_function_index, function_call_ast->parameter_count);

		if (!check_accessibility(parser_context, target_class_name, member_function_data->access_modifier)) {
			// handle error
			printf("Unable to access function : %S. access modifier of %S is %S.", member_function_data->name, member_function_data->name, member_function_data->access_modifier);
			exit(1);
		}

		check_function_call_condition(parser_context, member_function_data, function_call_ast->parameters, function_call_ast->parameter_count);

		int i;
		for (i = 0; i < member_function_data->parameter_count; i++) {
			*result = join_string(*result, create_ir(parser_context, function_call_ast->parameters[i], indentation));
		}

		new_line(result, indentation);
		*result = join_string(*result, function_call_buffer);

		if (function_call_ast->attribute != NULL) {
			create_attribute_ir(parser_context, member_function_data->return_type->type_str, function_call_ast->attribute, result, indentation);
		}
		break;
	}

	case AST_ArrayAccess: {
		ArrayAccessAST* array_access_ast = (ArrayAccessAST*)attribute;

		// In this case, doesn't need to check accessibility.
		// because it creates its target_array AST.
		// and it will automatically check the accessibility
		create_attribute_ir(parser_context, target_class_name, ((ArrayAccessAST*)attribute)->target_array, result, indentation);

		int i;
		for (i = 0; i < array_access_ast->access_count; i++) {
			*result = join_string(*result, create_ir(parser_context, array_access_ast->indexes[i], indentation));

			new_line(result, indentation);
			*result = join_string(*result, L"@array_load");
		}
		break;
	}
	}
}

Type* get_type_of_last_element(ParserContext* parser_context, void* ast, const wchar_t* search_point_class_name) {
	ASTType type = *((ASTType*)ast);

	Type* result = infer_type(parser_context, ast, search_point_class_name);

	if (type == AST_Identifier) {
		if (((IdentifierAST*)ast)->attribute != NULL) {
			result = get_type_of_last_element(parser_context, ((IdentifierAST*)ast)->attribute, result->type_str);
		}
	}

	if (type == AST_FunctionCall) {
		if (((FunctionCallAST*)ast)->attribute != NULL) {
			result = get_type_of_last_element(parser_context, ((FunctionCallAST*)ast)->attribute, result->type_str);
		}
	}

	if (type == AST_ArrayAccess) {
		if (((ArrayAccessAST*)ast)->attribute != NULL) {
			result = get_type_of_last_element(parser_context, ((ArrayAccessAST*)ast)->attribute, result->type_str);
		}
	}

	return result;
}

void create_assign_ir(ParserContext* parser_context, void* left_ast, void* right_ast, wchar_t** result, int indentation) {
	if (!check_castability(parser_context, get_type_of_last_element(parser_context, left_ast, current_class), get_type_of_last_element(parser_context, right_ast, current_class))) {
		// handle error for unmathed types of assign
		return;
	}

	*result = join_string(*result, create_ir(parser_context, right_ast, indentation));

	// for non-attribute, direct assign
	// a = 5; a[3] = 5
	switch (*((ASTType*)left_ast)) {
	case AST_Identifier: {
		if (((IdentifierAST*)left_ast)->attribute == NULL) {
			new_line(result, indentation);
			IdentifierAST* identifier_ast = (IdentifierAST*)left_ast;
			wchar_t store_str_buffer[128];

			Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier_ast->identifier);

			if (local_symbol == NULL) {
				swprintf(store_str_buffer, 128, L"@mstore %d", get_member_variable_index(parser_context, current_class, identifier_ast->identifier));
				*result = join_string(*result, store_str_buffer);
			}
			else {
				swprintf(store_str_buffer, 128, L"@store %d", ((VariableData*)local_symbol->data)->index);
				*result = join_string(*result, store_str_buffer);
			}
			return;
		}
		break;
	}

	case AST_ArrayAccess: {
		if (((ArrayAccessAST*)left_ast)->attribute == NULL) {
			IdentifierAST* identifier_ast = ((ArrayAccessAST*)left_ast)->target_array;

			*result = join_string(*result, create_ir(parser_context, identifier_ast, indentation));

			wchar_t store_str_buffer[128];

			int i;
			int access_count = ((ArrayAccessAST*)left_ast)->access_count;
			for (i = 0; i < access_count; i++) {
				*result = join_string(*result, create_ir(parser_context, ((ArrayAccessAST*)left_ast)->indexes[i], indentation));

				new_line(result, indentation);
				if (i == access_count - 1) {
					*result = join_string(*result, L"@array_store");
				}
				else {
					*result = join_string(*result, L"@array_load");
				}
			}
			return;
		}
		break;
	}

	default:
		// handle error for unable to store variable
		printf("Error at ir.c, unable to store variable\n");
		break;
	}

	void* temp_left_ast = left_ast;
	void* attribute_backup = left_ast;
	void* last_ast = NULL;

	int loop = 1;
	// extract last ast for store command.
	while (loop) {
		switch (*((ASTType*)left_ast)) {
		case AST_Identifier: {
			if (((IdentifierAST*)left_ast)->attribute == NULL) {
				if (*((ASTType*)attribute_backup) == AST_Identifier) {
					((IdentifierAST*)attribute_backup)->attribute = NULL;
				}
				else if (*((ASTType*)attribute_backup) == AST_FunctionCall) {
					((FunctionCallAST*)attribute_backup)->attribute = NULL;
				}
				else if (*((ASTType*)attribute_backup) == AST_ArrayAccess) {
					((ArrayAccessAST*)attribute_backup)->attribute = NULL;
				}


				last_ast = left_ast;
				loop = 0;
				break;
			}

			attribute_backup = left_ast;
			left_ast = ((IdentifierAST*)left_ast)->attribute;

			break;
		}

		case AST_FunctionCall: {
			attribute_backup = left_ast;
			left_ast = ((FunctionCallAST*)left_ast)->attribute;

			break;
		}
		case AST_ArrayAccess: {
			if (((ArrayAccessAST*)left_ast)->attribute == NULL) {
				if (*((ASTType*)attribute_backup) == AST_Identifier) {
					((IdentifierAST*)attribute_backup)->attribute = NULL;
				}
				else if (*((ASTType*)attribute_backup) == AST_FunctionCall) {
					((FunctionCallAST*)attribute_backup)->attribute = NULL;
				}
				else if (*((ASTType*)attribute_backup) == AST_ArrayAccess) {
					((ArrayAccessAST*)attribute_backup)->attribute = NULL;
				}

				last_ast = left_ast;
				loop = 0;
				break;
			}

			attribute_backup = left_ast;
			left_ast = ((ArrayAccessAST*)left_ast)->attribute;

			break;
		}

		default:
			loop = 0;
			break;
		}
	}

	if (last_ast == NULL) {
		// handle error for unable to store variable
		printf("Error at ir.c, unable to store variable\n");
		exit(1);
	}

	*result = join_string(*result, create_ir(parser_context, temp_left_ast, indentation));
	// process the last ast for store.
	switch (*((ASTType*)last_ast)) {
	case AST_Identifier: {
		wchar_t* target_class_name = infer_type(parser_context, temp_left_ast, L"")->type_str;
		Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);
		ClassData* target_class = target_class_symbol->data;

		int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((IdentifierAST*)last_ast)->identifier);
		VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((IdentifierAST*)last_ast)->identifier);

		if (!check_accessibility(parser_context, target_class_name, member_variable_data->access_modifier)) {
			// handle error
			printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
			exit(1);
		}

		new_line(result, indentation);
		wchar_t attr_str_buffer[128];
		swprintf(attr_str_buffer, 128, L"@attr_store %d", member_variable_index);
		*result = join_string(*result, attr_str_buffer);
		break;
	}
	case AST_ArrayAccess: {
		wchar_t* target_class_name = infer_type(parser_context, temp_left_ast, L"")->type_str;
		Symbol* target_class_symbol = find_symbol(parser_context->class_symbol_table, target_class_name);
		ClassData* target_class = target_class_symbol->data;

		int member_variable_index = get_member_variable_index(parser_context, target_class_name, ((ArrayAccessAST*)last_ast)->target_array->identifier);
		VariableData* member_variable_data = get_member_variable_data(parser_context, target_class_name, ((ArrayAccessAST*)last_ast)->target_array->identifier);

		if (!check_accessibility(parser_context, target_class_name, member_variable_data->access_modifier)) {
			// handle error
			printf("Unable to access variable : %S. access modifier of %S is %S.", member_variable_data->name, member_variable_data->name, member_variable_data->access_modifier);
			exit(1);
		}

		ArrayAccessAST* array_access_last_ast = (ArrayAccessAST*)last_ast;

		new_line(result, indentation);
		wchar_t attr_str_buffer[128];
		swprintf(attr_str_buffer, 128, L"@attr %d", member_variable_index);
		*result = join_string(*result, attr_str_buffer);

		wchar_t store_str_buffer[128];

		int i;
		int access_count = array_access_last_ast->access_count;
		for (i = 0; i < access_count; i++) {
			*result = join_string(*result, create_ir(parser_context, array_access_last_ast->indexes[i], indentation));

			new_line(result, indentation);
			if (i == access_count - 1) {
				*result = join_string(*result, L"@array_store");
			}
			else {
				*result = join_string(*result, L"@array_load");
			}
		}
		break;
	}
	}
}

void check_function_call_condition(ParserContext* parser_context, FunctionData* function_data, const void** parameters, int parameter_count) {

	if (function_data->parameter_types == VARIABLE_ARGUMENTS) {// pass the case for the variable arguments
		return;
	}

	if (function_data->parameter_count != parameter_count) {
		printf("[Temporary error] at ir.c Type not matched\n");
		abort();
	}

	int i;
	for (i = 0; i < parameter_count; i++) {

		Type* infered_type = get_type_of_last_element(parser_context, parameters[i], current_class);

		if (!check_castability(parser_context, infered_type, function_data->parameter_types[i])) {
			printf("[Temporary error] at ir.c Type not matched : %S, %S\n", infered_type->type_str, function_data->parameter_types[i]->type_str);
			abort();
		}
	}

}

wchar_t* create_if_statement_block(ParserContext* parser_context, IfStatementAST* if_statement_ast, int indentation, int end_label_id) {
	wchar_t if_statement_buffer[128];
	wchar_t* result = L"";

	label_id++;
	int block_id = label_id;

	if (if_statement_ast->if_type != StmtElse) {
		new_line(&result, indentation);
		swprintf(if_statement_buffer, 128, L"@jne %d", block_id);
		result = join_string(result, if_statement_buffer);
	}

	open_scope(parser_context);

	int i;
	for (i = 0; i < if_statement_ast->body_count; i++) {
		result = join_string(result, create_ir(parser_context, if_statement_ast->body[i], indentation));
	}

	close_scope(parser_context);

	// as the statement ended. jump for the terminate the statement
	new_line(&result, indentation);
	swprintf(if_statement_buffer, 128, L"@goto %d", end_label_id);
	result = join_string(result, if_statement_buffer);

	if (if_statement_ast->if_type != StmtElse) {
		new_line(&result, indentation);
		swprintf(if_statement_buffer, 128, L"@label %d", block_id);
		result = join_string(result, if_statement_buffer);
	}
}

wchar_t* create_array_declaration_ir(ParserContext* parser_context, ArrayDeclarationAST* array_declaration_ast, int indentation) {
	wchar_t* result = L"";
	int i;
	for (i = 0; i < array_declaration_ast->element_count; i++) {
		result = join_string(result, create_ir(parser_context, array_declaration_ast->elements[i], indentation + 1));
	}

	new_line(&result, indentation + 1);
	wchar_t array_str_buffer[128];
	swprintf(array_str_buffer, 128, L"@array %d", array_declaration_ast->element_count);
	result = join_string(result, array_str_buffer);
	return result;
}

wchar_t* create_return_ir(ParserContext* parser_context, ReturnAST* return_ast, int indentation) {
	wchar_t* result = L"";

	result = join_string(result, create_ir(parser_context, return_ast->expression, indentation));

	new_line(&result, indentation);
	wchar_t ret_str_buffer[128];
	swprintf(ret_str_buffer, 128, L"@ret");
	result = join_string(result, ret_str_buffer);
	return result;
}

wchar_t* create_bin_expr_ir(ParserContext* parser_context, BinExprAST* bin_expr_ast, int indentation) {
	wchar_t* result = L"";

	if (bin_expr_ast->opType == OpASSIGN) {
		create_assign_ir(parser_context, bin_expr_ast->left, bin_expr_ast->right, &result, indentation);
		return result;
	}

	result = join_string(result, create_ir(parser_context, bin_expr_ast->left, indentation));
	result = join_string(result, create_ir(parser_context, bin_expr_ast->right, indentation));

	wchar_t operator_str_buffer[128];
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
	return result;
}

wchar_t* create_identifier_ir(ParserContext* parser_context, IdentifierAST* identifier_ast, int indentation) {
	wchar_t* result = L"";

	new_line(&result, indentation);
	wchar_t identifier_str_buffer[128];

	int is_special_identifier = !wcscmp(identifier_ast->identifier, L"this");

	if (is_special_identifier) {
		swprintf(identifier_str_buffer, 128, L"@this");
		result = join_string(result, identifier_str_buffer);
	}
	else {
		Symbol* local_symbol = find_symbol(parser_context->variable_symbol_table, identifier_ast->identifier);
		VariableData* variable_data = find_variable_data(parser_context, identifier_ast->tok, current_class, identifier_ast->identifier);
		int variable_exist_in_local_area = local_symbol != NULL;

		if (variable_exist_in_local_area) {
			swprintf(identifier_str_buffer, 128, L"@load %d", ((VariableData*)local_symbol->data)->index);
			result = join_string(result, identifier_str_buffer);
		}
		else {
			int member_id = get_member_variable_index(parser_context, current_class, identifier_ast->identifier);
			swprintf(identifier_str_buffer, 128, L"@mload %d", member_id);
			result = join_string(result, identifier_str_buffer);
		}
	}

	if (identifier_ast->attribute != NULL) {
		wchar_t* target_class_name = infer_type(parser_context, identifier_ast, current_class)->type_str;
		create_attribute_ir(parser_context, target_class_name, identifier_ast->attribute, &result, indentation);
	}
	return result;
}

wchar_t* create_string_literal_ir(ParserContext* parser_context, StringLiteralAST* string_literal_ast, int indentation) {
	wchar_t* result = L"";
	wchar_t* string_literal = string_literal_ast->string_literal;
	wchar_t push_str_buffer[128];

	new_line(&result, indentation);

	swprintf(push_str_buffer, 128, L"@push string %s", string_literal);

	result = join_string(result, push_str_buffer);
	return result;
}

wchar_t* create_new_ir(ParserContext* parser_context, NewAST* new_ast, int indentation) {
	wchar_t* result = L"";

	Symbol* class_symbol = find_symbol(parser_context->class_symbol_table, new_ast->class_name);

	if (class_symbol == NULL) {
		handle_error(ER1002, new_ast->tok, parser_context->current_file_name, parser_context->file_str);
	}

	ClassData* class_data = class_symbol->data;

	FunctionData* constructor_data = class_data->constructor_data;

	check_function_call_condition(parser_context, constructor_data, new_ast->parameters, new_ast->parameter_count);

	int i;
	for (i = 0; i < new_ast->parameter_count; i++) {
		result = join_string(result, create_ir(parser_context, new_ast->parameters[i], indentation));
	}

	new_line(&result, indentation);

	wchar_t new_buffer[512];
	swprintf(new_buffer, 128, L"@new %d %d", class_data->index, constructor_data->parameter_count);
	result = join_string(result, new_buffer);

	return result;
}

wchar_t* create_array_access_ir(ParserContext* parser_context, ArrayAccessAST* array_access_ast, int indentation) {
	wchar_t* result = L"";

	result = join_string(result, create_ir(parser_context, array_access_ast->target_array, indentation));

	int i;
	for (i = 0; i < array_access_ast->access_count; i++) {
		result = join_string(result, create_ir(parser_context, array_access_ast->indexes[i], indentation));

		new_line(&result, indentation);
		result = join_string(result, L"@array_load");
	}
	return result;
}

wchar_t* create_function_call_ir(ParserContext* parser_context, FunctionCallAST* function_call_ast, int indentation) {
	wchar_t* result = L"";
	FunctionData* function_data = find_function_data(parser_context, function_call_ast->tok, current_class, function_call_ast->function_name, function_call_ast);

	wchar_t function_call_buffer[512];

	Symbol* local_symbol = find_symbol(parser_context->function_symbol_table, function_call_ast->function_name);

	int member_call = local_symbol == NULL;
	int local_call = (local_symbol != NULL) && !((FunctionData*)local_symbol->data)->is_builtin_function;
	int builtin_call = (local_symbol != NULL) && ((FunctionData*)local_symbol->data)->is_builtin_function;

	if (member_call) {
		swprintf(function_call_buffer, 128, L"@mcall %d %d", get_member_function_index(parser_context, current_class, function_call_ast->function_name), function_call_ast->parameter_count);
	}

	if (local_call) {
		swprintf(function_call_buffer, 128, L"@call %d %d", ((FunctionData*)local_symbol->data)->index, function_call_ast->parameter_count);
	}

	if (builtin_call) {
		swprintf(function_call_buffer, 128, L"@bcall %d %d", ((FunctionData*)local_symbol->data)->index, function_call_ast->parameter_count);
	}

	check_function_call_condition(parser_context, function_data, function_call_ast->parameters, function_call_ast->parameter_count);

	int i;
	for (i = 0; i < function_call_ast->parameter_count; i++) {
		result = join_string(result, create_ir(parser_context, function_call_ast->parameters[i], indentation));
	}

	new_line(&result, indentation);
	result = join_string(result, function_call_buffer);
	return result;
}

wchar_t* create_number_literal_ir(ParserContext* parser_context, NumberLiteralAST* number_literal_ast, int indentation) {
	wchar_t* result = L"";
	wchar_t* number_literal = number_literal_ast->number_literal;
	wchar_t push_str_buffer[128];

	new_line(&result, indentation);
	swprintf(push_str_buffer, 128, L"@push %s %s", number_literal_ast->numeric_type, number_literal);

	result = join_string(result, push_str_buffer);
	return result;
}

wchar_t* create_constructor_ir(ParserContext* parser_context, ConstructorAST* constructor_ast, int indentation) {
	wchar_t* result = L"";
	wchar_t* parameter_buffer = L"";

	if (constructor_ast->parameters != NULL) {
		parameter_buffer = create_parameter_buffer(parser_context, constructor_ast->parameters);
	}

	new_line(&result, indentation + 1);
	wchar_t buffer[256];
	swprintf(buffer, 256, L"$constructor %ls {", parameter_buffer);
	result = join_string(result, buffer);

	int i;
	for (i = 0; i < constructor_ast->body_count; i++) {
		result = join_string(result, create_ir(parser_context, constructor_ast->body[i], indentation + 2));
	}

	new_line(&result, indentation + 1);
	result = join_string(result, L"}");
	return result;
}

wchar_t* create_function_declaration_ir(ParserContext* parser_context, FunctionDeclarationAST* function_declaration_ast, int indentation) {
	wchar_t* result = L"";

	Symbol* function_symbol = find_symbol(parser_context->function_symbol_table, function_declaration_ast->function_name);

	int should_find_member_function = function_symbol == NULL && wcscmp(current_class, L"");
	if (should_find_member_function) {
		ClassData* current_class_data = find_symbol(parser_context->class_symbol_table, current_class)->data;
		function_symbol = find_symbol(current_class_data->member_functions, function_declaration_ast->function_name);
	}

	open_scope(parser_context);

	wchar_t* parameter_buffer = create_parameter_buffer(parser_context, ((VariableDeclarationBundleAST*)function_declaration_ast->parameters));
	FunctionData* function_data = function_symbol->data;

	function_data->access_modifier = function_declaration_ast->access_modifier;

	new_line(&result, indentation);
	wchar_t buffer[256];
	swprintf(buffer, 256, L".%d %ls{", (function_data->index), parameter_buffer);
	result = join_string(result, buffer);

	int i;
	for (i = 0; i < function_declaration_ast->body_count; i++) {
		result = join_string(result, create_ir(parser_context, function_declaration_ast->body[i], indentation + 1));
	}

	new_line(&result, indentation);
	result = join_string(result, L"}");

	close_scope(parser_context);

	return result;
}

wchar_t* create_class_ir(ParserContext* parser_context, ClassAST* ast, int indentation) {
	wchar_t* result = L"";
	ClassAST* class_ast = (ClassAST*)ast;

	ClassData* class_data = find_symbol(parser_context->class_symbol_table, class_ast->class_name)->data;
	Symbol* parent_symbol = find_symbol(parser_context->class_symbol_table, class_ast->parent_class_name);
	ClassData* parent_data = NULL;

	open_scope(parser_context);

	current_class = class_ast->class_name;

	if (parent_symbol != NULL) {
		parent_data = parent_symbol->data;
	}

	wchar_t buffer[128];
	new_line(&result, indentation);
	swprintf(buffer, 128, L"class %d %d {", class_data->index, parent_data ? parent_data->index : 0);
	result = join_string(result, buffer);

	result = join_string(result, create_class_initializer(parser_context, indentation, class_ast));
	result = join_string(result, create_ir(parser_context, class_ast->constructor, indentation));

	int i;
	for (i = 0; i < class_ast->member_function_count; i++) {
		result = join_string(result, create_ir(parser_context, class_ast->member_functions[i], indentation + 1));
	}

	new_line(&result, indentation);
	result = join_string(result, L"}\n");

	current_class = L"";
	close_scope(parser_context);
	return result;
}

wchar_t* create_variable_declaration_ir(ParserContext* parser_context, VariableDeclarationAST* variable_declaration_ast, int indentation) {
	wchar_t* result = L"";
	VariableData* data = NULL;

	if (is_class_initializer) {
		SymbolTable* member_variable_symbol_table = ((ClassData*)find_symbol(parser_context->class_symbol_table, current_class)->data)->member_variables;
		data = create_variable_data(member_variable_symbol_table, variable_declaration_ast->variable_type, variable_declaration_ast->variable_name, variable_declaration_ast->access_modifier);
		insert_variable_symbol(member_variable_symbol_table, variable_declaration_ast->variable_name, data);
	}
	else {
		data = create_variable_data(parser_context->variable_symbol_table, variable_declaration_ast->variable_type, variable_declaration_ast->variable_name, variable_declaration_ast->access_modifier);
		insert_variable_symbol(parser_context->variable_symbol_table, variable_declaration_ast->variable_name, data);
	}

	if (variable_declaration_ast->declaration) {
		result = join_string(result, create_ir(parser_context, variable_declaration_ast->declaration, indentation));

		if (!check_castability(parser_context, get_type_of_last_element(parser_context, variable_declaration_ast, current_class), get_type_of_last_element(parser_context, variable_declaration_ast->declaration, current_class))) {
			return;
		}
	}

	// indexing for additional parent class member variables.
	int parent_variable_count = get_parent_member_variable_count(parser_context, current_class);

	wchar_t store_str_buffer[128];

	new_line(&result, indentation);

	if (is_class_initializer) {
		swprintf(store_str_buffer, 128, L"%s %d", L"@mstore", data->index + parent_variable_count);
	}
	else {
		swprintf(store_str_buffer, 128, L"%s %d", L"@store", data->index);
	}

	result = join_string(result, store_str_buffer);
	return result;
}

wchar_t* create_ident_increase_ir(ParserContext* parser_context, IdentIncreaseAST* ident_increase_ast, int indentation) {
	wchar_t* result = L"";
	VariableData* variable_data = find_variable_data(parser_context, ident_increase_ast->tok, current_class, ident_increase_ast->identifier);

	new_line(&result, indentation);
	wchar_t ident_increase_str_buffer[128];
	swprintf(ident_increase_str_buffer, 128, L"@inc %d", variable_data->index);
	result = join_string(result, ident_increase_str_buffer);
	return result;
}

wchar_t* create_ident_decrease_ir(ParserContext* parser_context, IdentDecreaseAST* ident_decrease_ast, int indentation) {
	wchar_t* result = L"";

	VariableData* variable_data = find_variable_data(parser_context, ident_decrease_ast->tok, current_class, ident_decrease_ast->identifier);

	new_line(&result, indentation);
	wchar_t ident_decrease_str_buffer[128];
	swprintf(ident_decrease_str_buffer, 128, L"@dec %d", variable_data->index);
	result = join_string(result, ident_decrease_str_buffer);

	return result;
}

wchar_t* create_if_statement_ir(ParserContext* parser_context, IfStatementAST* if_statement_ast, int indentation) {
	wchar_t* result = L"";

	if (if_statement_ast->if_type != StmtElse) {
		result = join_string(result, create_ir(parser_context, if_statement_ast->condition, indentation));
	}

	label_id++;
	int end_label_id = label_id;

	create_if_statement_block(parser_context, if_statement_ast, indentation, end_label_id);

	wchar_t if_statement_buffer[128];
	if (if_statement_ast->next_statement != NULL) {
		result = join_string(result, create_ir(parser_context, if_statement_ast->next_statement, indentation));
	}

	new_line(&result, indentation);
	swprintf(if_statement_buffer, 128, L"@label %d", end_label_id);
	result = join_string(result, if_statement_buffer);
	return result;
}

wchar_t* create_for_statement_ir(ParserContext* parser_context, ForStatementAST* for_statement_ast, int indentation) {
	wchar_t* result = L"";
	open_scope(parser_context);

	result = join_string(result, create_ir(parser_context, for_statement_ast->init, indentation));

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

	int i;
	for (i = 0; i < for_statement_ast->body_count; i++) {
		result = join_string(result, create_ir(parser_context, for_statement_ast->body[i], indentation));
	}

	result = join_string(result, create_ir(parser_context, for_statement_ast->step, indentation));

	swprintf(label_str_buffer, 64, L"@label %x", end_label_id);
	new_line(&result, indentation);
	result = join_string(result, label_str_buffer);

	result = join_string(result, create_ir(parser_context, for_statement_ast->condition, indentation));

	swprintf(label_str_buffer, 64, L"@for %x", begin_label_id);
	new_line(&result, indentation);
	result = join_string(result, label_str_buffer);

	close_scope(parser_context);
	return result;
}

wchar_t* create_ir(ParserContext* parser_context, void* ast, int indentation) {

	wchar_t* result = L"";

	switch (*((ASTType*)ast)) {

	case AST_ArrayDeclaration: {
		result = join_string(result, create_array_declaration_ir(parser_context, (ArrayDeclarationAST*)ast, indentation));
		break;
	}

	case AST_Return: {
		result = join_string(result, create_return_ir(parser_context, (ReturnAST*)ast, indentation));
		break;
	}

	case AST_BinExpr: {
		result = join_string(result, create_bin_expr_ir(parser_context, (BinExprAST*)ast, indentation));
		break;
	}

	case AST_Identifier: {
		result = join_string(result, create_identifier_ir(parser_context, (IdentifierAST*)ast, indentation));
		break;
	}

	case AST_StringLiteral: {
		result = join_string(result, create_string_literal_ir(parser_context, (StringLiteralAST*)ast, indentation));
		break;
	}

	case AST_New: {
		result = join_string(result, create_new_ir(parser_context, (NewAST*)ast, indentation));
		break;
	}

	case AST_ArrayAccess: {
		result = join_string(result, create_array_access_ir(parser_context, (ArrayAccessAST*)ast, indentation));
		break;
	}

	case AST_FunctionCall: {
		result = join_string(result, create_function_call_ir(parser_context, (FunctionCallAST*)ast, indentation));
		break;
	}

	case AST_NumberLiteral: {
		result = join_string(result, create_number_literal_ir(parser_context, (NumberLiteralAST*)ast, indentation));
		break;
	}

	case AST_Constructor: {
		result = join_string(result, create_constructor_ir(parser_context, (ConstructorAST*)ast, indentation));
		break;
	}

	case AST_FunctionDeclaration: {
		result = join_string(result, create_function_declaration_ir(parser_context, (FunctionDeclarationAST*)ast, indentation));
		break;
	}

	case AST_Class: {
		result = join_string(result, create_class_ir(parser_context, (ClassAST*)ast, indentation));
		break;
	}

	case AST_VariableDeclarationBundle: {
		VariableDeclarationBundleAST* variable_declaration_bundle_ast = (VariableDeclarationBundleAST*)ast;

		int i;
		for (i = 0; i < variable_declaration_bundle_ast->variable_count; i++) {
			result = join_string(result, create_variable_declaration_ir(parser_context, variable_declaration_bundle_ast->variable_declarations[i], indentation));
		}

		break;
	}

	case AST_VariableDeclaration: {
		result = join_string(result, create_variable_declaration_ir(parser_context, (VariableDeclarationAST*)ast, indentation));
		break;
	}

	case AST_IdentIncrease: {
		result = join_string(result, create_ident_increase_ir(parser_context, (IdentIncreaseAST*)ast, indentation));
		break;
	}

	case AST_IdentDecrease: {
		result = join_string(result, create_ident_decrease_ir(parser_context, (IdentDecreaseAST*)ast, indentation));
		break;
	}

	case AST_IfStatement: {
		result = join_string(result, create_if_statement_ir(parser_context, (IfStatementAST*)ast, indentation));
		break;
	}

	case AST_ForStatement: {
		result = join_string(result, create_for_statement_ir(parser_context, (ForStatementAST*)ast, indentation));
		break;
	}
	}

	return result;
}