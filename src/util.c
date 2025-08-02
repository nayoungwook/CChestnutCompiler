#include "includes/util.h"

wchar_t* get_working_directory() {
	wchar_t cwd[1024];
	int result = _wgetcwd(cwd, sizeof(cwd) / sizeof(wchar_t));

	if (!result) {
		wprintf("Failed to find cwd.\n");
	}

	return cwd;
}

wchar_t* substr(const wchar_t* str, int s, int e) {
	if (s < 0 || e < s || !str) {
		return NULL;
	}

	int length = e - s + 1;
	wchar_t* result = safe_malloc(sizeof(wchar_t) * (length + 1));;

	wcsncpy_s(result, length + 1, str + s, length);

	result[length] = L'\0';

	return result;
}

void* safe_malloc(size_t size) {
	void* ptr = malloc(size);
	if (!ptr) {
		fprintf(stderr, "malloc failed at %s:%d\n", __FILE__, __LINE__);
	}
	return ptr;
}

void* safe_relloc(void* ptr, size_t size) {
	void* re_ptr = realloc(ptr, size);
	if (!re_ptr) {
		fprintf(stderr, "realloc failed at %s:%d\n", __FILE__, __LINE__);
	}
	return re_ptr;
}

int* line_index_data;
int* line_number_data;

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

	int capacity = 1;
	int size = 0;
	line_index_data = (int*)malloc(sizeof(int));
	line_number_data = (int*)malloc(sizeof(int));
	int line_number = 0;

	size_t i = 0;
	while (fgetws(str + i, (int)(wchar_estimate - i), fp)) {
		i = wcslen(str);

		if (size >= capacity) {
			capacity *= 2;
			line_index_data = (int*)realloc(line_index_data, sizeof(int) * capacity);
			line_number_data = (int*)realloc(line_number_data, sizeof(int) * capacity);
		}

		line_number++;
		line_index_data[size] = i;
		line_number_data[size] = line_number;

		size++;
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
	memset(symbol_table->table, NULL, sizeof(Symbol*) * TABLE_SIZE);

	return symbol_table;
}

Set* create_set() {
	Set* set = (Set*)safe_malloc(sizeof(Set));;

	if (set) {
		set->size = 0;
	}
	memset(set->table, NULL, sizeof(Symbol*) * TABLE_SIZE);

	return set;
}

Symbol* find_symbol_from_set(Set* target_set, const wchar_t* name) {
	unsigned int _hash = hash(name);

	Symbol* result = target_set->table[_hash];

	if (result == NULL) {
		return NULL;
	}

	while (result != NULL && strcmp(name, result->symbol)) {
		result = result->next;
	}

	if (result == NULL || strcmp(name, result->symbol)) {
		return NULL;
	}

	return result;
}

Symbol* find_symbol(SymbolTable* cur_symbol_table, const wchar_t* name) {
	unsigned int _hash = hash(name);

	Symbol* result = cur_symbol_table->table[_hash];

	while (result == NULL) {
		if (cur_symbol_table->prev == NULL) return NULL;
		cur_symbol_table = cur_symbol_table->prev;

		result = cur_symbol_table->table[_hash];
	}

	while (wcscmp(name, result->symbol)) {
		result = result->next;
	}

	if (result == NULL || wcscmp(name, result->symbol)) {
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
