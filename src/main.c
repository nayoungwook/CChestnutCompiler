#include "includes/main.h"

#define DEBUG_VIEW_IR
#define IR_BYTE 1

void print_tokens(wchar_t* str) {
	while (1) {
		Token* tok = pull_token(str);

		printf("%S %d %d\n", tok->str, tok->type, tok->line_number);

		if (tok->type == TokEOF) break;
	}
}

void initialize_primitive_types(ParserContext* parser_context) {
	insert_set_symbol(parser_context->primitive_types, L"number");
	insert_set_symbol(parser_context->primitive_types, L"int");
	insert_set_symbol(parser_context->primitive_types, L"float");
	insert_set_symbol(parser_context->primitive_types, L"double");
	insert_set_symbol(parser_context->primitive_types, L"bool");
	insert_set_symbol(parser_context->primitive_types, L"char");
}

void parse_file(IrGenContext* ir_context, ParserContext* parser_context, const wchar_t* file_name) {
	wchar_t* file = read_file(file_name);

	int ast_count = 0;
	int class_ast_count = 0;

	void** asts = NULL;
	void** class_asts = NULL;


	while (peek_token(file)->type != TokEOF) {
		set_file_string(parser_context, file);
		void* ast = parse(parser_context, file);

		if (*((ASTType*)ast) == AST_Class) {
			if (class_ast_count == 0) {
				class_asts = (void**)safe_malloc(sizeof(void*));
				class_asts[class_ast_count] = ast;
				class_ast_count++;
			}
			else {
				class_asts = (void**)realloc(class_asts, sizeof(void*) * (class_ast_count + 1));
				class_asts[class_ast_count] = ast;
				class_ast_count++;
			}
		}

		if (ast_count == 0) {
			asts = (void**)malloc(sizeof(void*));
			asts[ast_count] = ast;
			ast_count++;
		}
		else {
			asts = (void**)realloc(asts, sizeof(void*) * (ast_count + 1));
			asts[ast_count] = ast;
			ast_count++;
		}
	}

	int i = 0;

	for (i = 0; i < class_ast_count; i++) {
		initialize_class_data(ir_context, parser_context, ((ClassAST*)class_asts[i]));
	}

	for (i = 0; i < ast_count; i++) {
		create_ir(ir_context, parser_context, asts[i]);
	}

#ifdef DEBUG_VIEW_IR
	printf(" %S\n", ir_context->string_builder->str);
#endif

	wchar_t* original_name = substr(file_name, 0, wcslen(file_name) - 6);
	wchar_t* new_file_name = join_string(original_name, L".ir");

	write_file(new_file_name, ir_context->string_builder->str);
}

int wmain(int arc, char* args[]) {
	setlocale(LC_ALL, "");

	ParserContext* parser_context = create_parser_context();
	IrGenContext* ir_context = create_ir_context();
	initialize_primitive_types(parser_context);
	initialize_builtin_functions(parser_context);
	initialize_byte_table();

	parse_file(ir_context, parser_context, L"main.cnut");

	return 0;
}