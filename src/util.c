#include "includes/util.h"

Type* clone_type(Type* type) {
	Type* result = (Type*)safe_malloc(sizeof(Type));
	result->array_element_type = type->array_element_type;
	result->is_array = type->is_array;
	result->type_str = _wcsdup(type->type_str);
	return result;
}

wchar_t* get_working_directory() {
	wchar_t* cwd = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
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

void* safe_realloc(void* ptr, size_t size) {
	void* re_ptr = realloc(ptr, size);
	if (!re_ptr) {
		fprintf(stderr, "realloc failed at %s:%d\n", __FILE__, __LINE__);
	}
	return re_ptr;
}

int* line_index_data;
int* line_number_data;

wchar_t* read_file(const wchar_t* file_path) {

	FILE* fp = _wfopen(file_path, "r");
	if (!fp) {
		perror("���� ���� ����");
		return 1;
	}

	fseek(fp, 0, SEEK_END);
	long byte_len = ftell(fp);
	rewind(fp);

	size_t wchar_estimate = byte_len + 1;
	wchar_t* str = (wchar_t*)safe_malloc(sizeof(wchar_t) * wchar_estimate);

	int capacity = 1;
	int size = 0;
	line_index_data = (int*)safe_malloc(sizeof(int));
	line_number_data = (int*)safe_malloc(sizeof(int));
	int line_number = 0;

	size_t i = 0;
	while (fgetws(str + i, (int)(wchar_estimate - i), fp)) {
		i = wcslen(str);

		if (size >= capacity) {
			capacity *= 2;
			line_index_data = (int*)safe_realloc(line_index_data, sizeof(int) * capacity);
			line_number_data = (int*)safe_realloc(line_number_data, sizeof(int) * capacity);
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
	SymbolTable* symbol_table = (SymbolTable*)safe_malloc(sizeof(SymbolTable));
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

void write_file(const wchar_t* path, const wchar_t* str) {
	FILE* file = _wfopen(path, "w");

	if (file == NULL) {
		printf("������ �� �� �����ϴ�.\n");
		return 1;
	}

	fwprintf(file, str);

	fclose(file);
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

wchar_t* join_byte(const wchar_t* str, int data) {
	if (!str) return NULL;

	size_t len = wcslen(str);
	size_t str_byte_size = (len + 1) * sizeof(wchar_t);
	size_t total_size = str_byte_size + 4;

	void* mem = malloc(total_size);
	if (!mem) return NULL;

	memcpy(mem, str, str_byte_size);

	unsigned u = (unsigned)data;
	unsigned char be[4];
	be[0] = (unsigned char)((u >> 24) & 0xFF);
	be[1] = (unsigned char)((u >> 16) & 0xFF);
	be[2] = (unsigned char)((u >> 8) & 0xFF);
	be[3] = (unsigned char)(u & 0xFF);

	memcpy((unsigned char*)mem + str_byte_size, be, 4);

	return (wchar_t*)mem;
}

int is_decimal(wchar_t* str) {
	while (*str != L'\0') {
		if (*str == L'.' || *str == L'f') return 1;
		str++;
	}
	return 0;
}

void format_int_big_endian(int value, size_t size, wchar_t* out) {
	swprintf(out, size, L"%02X %02X %02X %02X ",
		(value >> 24) & 0xFF,
		(value >> 16) & 0xFF,
		(value >> 8) & 0xFF,
		value & 0xFF);
}

void format_wchar_big_endian(const wchar_t* value, size_t out_size, wchar_t* out) {
	if (!value || !out) return;

	size_t len = wcslen(value);
	wchar_t* ptr = out;
	size_t remaining = out_size;
	size_t i = 0;

	for (i = 0; i < len; ++i) {
		unsigned ch = (unsigned)value[i];

		unsigned char high = (ch >> 8) & 0xFF;
		unsigned char low = ch & 0xFF;

		if (remaining < 4) {
			*ptr = L'\0';
			return;
		}

		swprintf(ptr, remaining, L"%02X %02X ", high, low);
		ptr += 5;
		remaining -= 5;

		if (i != len - 1) {
			*(ptr - 1) = L' ';
		}
		else {
			*(ptr - 1) = L'\0';
		}
	}
}