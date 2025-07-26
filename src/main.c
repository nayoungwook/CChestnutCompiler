#include "includes/main.h"

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

void parse_file(ParserContext* parser_context, const wchar_t* file_name) {
	wchar_t* file = read_file(file_name);

	while (peek_token(file)->type != TokEOF) {
		set_file_string(parser_context, file);
		void* ast = parse(parser_context, file);

		printf("%S\n", create_ir(parser_context, ast, 0));
	}
}

int wmain(int arc, char* args[]) {
	setlocale(LC_ALL, "");

	ParserContext* parser_context = create_parser_context();
	initialize_primitive_types(parser_context);

	parse_file(parser_context, "main.cnut");

	return 0;
}