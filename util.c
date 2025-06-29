#include "util.h"

wchar_t* get_working_directory() {
	wchar_t cwd[1024];
	int result = _wgetcwd(cwd, sizeof(cwd) / sizeof(wchar_t));

	if (!result) {
		wprintf("Failed to find cwd.\n");
	}

	return cwd;
}

wchar_t* read_file(const wchar_t* file_path) {
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

unsigned int hash(const wchar_t* str) {
	unsigned int hash = 0;
	while (*str) {
		hash = (hash << 5) + *str++;
	}
	return hash % TABLE_SIZE;
}

SymbolTable* create_symbol_table() {
	SymbolTable* symbol_table = (SymbolTable*)malloc(sizeof(SymbolTable));
	if (symbol_table) {
		symbol_table->size = 0;
		symbol_table->prev = NULL;
	}
	return symbol_table;
}

Symbol* find_symbol(SymbolTable* cur_symbol_table, const wchar_t* name) {
	unsigned int _hash = hash(name);

	Symbol* result = cur_symbol_table->table[_hash];

	while (result == NULL) {
		cur_symbol_table = cur_symbol_table->prev;
		result = cur_symbol_table->table[_hash];
	}

	while (strcmp(name, result->symbol)) {
		result = result->next;
	}

	if (result == NULL || strcmp(name, result->symbol)) {
		return NULL;
	}

	return result;
}

wchar_t* join_string(const wchar_t* str1, const wchar_t* str2) {
	if (!str1) str1 = L"";
	if (!str2) str2 = L"";

	size_t len1 = wcslen(str1);
	size_t len2 = wcslen(str2);

	wchar_t* result = (wchar_t*)malloc((len1 + len2 + 1) * sizeof(wchar_t));
	if (!result) {
		return NULL;
	}

	wcscpy_s(result, len1 + len2 + 1, str1);
	wcscat_s(result, len1 + len2 + 1, str2);

	return result;
}

int is_decimal(wchar_t* str) {
	while (*str != L'\0') {
		if (*str == L'.' || *str == L'f') return 1;
		str++;
	}
	return 0;
}

const wchar_t* get_generalized_type(const wchar_t* type) {
	return type;
}

const wchar_t* create_mangled_name(const wchar_t* name, VariableDeclarationBundleAST* parameters) {
	wchar_t* result = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
	result = L"";

	int i = 0;
	for (i = 0; i < parameters->variable_count; i++) {
		result = join_string(result, parameters->variable_declarations[i]->variable_type);
		result = join_string(result, L"_");
	}
	result = join_string(result, name);

	return result;
}

const wchar_t* create_generalized_mangled_name(const wchar_t* name, VariableDeclarationBundleAST* parameters) {
	wchar_t* result = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
	result = L"";

	int i = 0;
	for (i = 0; i < parameters->variable_count; i++) {
		result = join_string(result, get_generalized_type(parameters->variable_declarations[i]->variable_type));
		result = join_string(result, L"_");
	}
	result = join_string(result, name);

	return result;
}
