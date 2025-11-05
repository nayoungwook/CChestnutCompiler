#include "includes/util.h"

Type* clone_type(Type* type) {
  Type* result = (Type*)safe_malloc(sizeof(Type));
  result->array_element_type = type->array_element_type;
  result->is_array = type->is_array;
  result->type_str = wcsdup(type->type_str);
  return result;
}

wchar_t* get_working_directory() {
  wchar_t* cwd = (wchar_t*)malloc(sizeof(wchar_t) * 1024);
  int result = _wgetcwd(cwd, sizeof(cwd) / sizeof(wchar_t));

  if (!result) {
    wprintf(L"Failed to find cwd.\n");
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
  FILE* file = _wfopen(path, L"w");

  if (file == NULL) {

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

int is_decimal(wchar_t* str) {
  while (*str != L'\0') {
    if (*str == L'.' || *str == L'f') return 1;
    str++;
  }
  return 0;
}
