#include "includes/main.h"

void print_tokens(wchar_t* str) {
	while (1) {
		Token* tok = pull_token(str);

		printf("%S %d %d\n", tok->str, tok->type, tok->line_number);

		if (tok->type == TokEOF) break;
	}
}

extern SymbolTable* variable_symbol_table;
extern SymbolTable* function_symbol_table;
extern SymbolTable* class_hierarchy;
extern SymbolTable* class_symbol_table;
extern Set* primitive_types;

void initialize_global_symbol_table() {
	variable_symbol_table = create_symbol_table();
	function_symbol_table = create_symbol_table();
	class_hierarchy = create_symbol_table();
	class_symbol_table = create_symbol_table();
}

void initialize_primitive_types() {
	primitive_types = create_set();

	insert_set_symbol(primitive_types, L"number");
	insert_set_symbol(primitive_types, L"int");
	insert_set_symbol(primitive_types, L"float");
	insert_set_symbol(primitive_types, L"double");
	insert_set_symbol(primitive_types, L"bool");
	insert_set_symbol(primitive_types, L"char");
}

void parse_file(const wchar_t* file_name) {
	wchar_t* file = read_file(file_name);

	while (peek_token(file)->type != TokEOF) {
		set_file_string(file);
		void* ast = parse(file);

		printf("%S\n", generate_ir(ast, 0));
	}
}

int wmain(int arc, char* args[]) {
	setlocale(LC_ALL, "");

	create_parser_context();
	initialize_primitive_types();
	initialize_global_symbol_table();

	parse_file("main.cnut");

	return 0;
}