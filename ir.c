#include "ir.h"

wchar_t* join_string(const wchar_t* str1, const wchar_t* str2) {
	if (!str1) str1 = L"";
	if (!str2) str2 = L"";

	size_t len1 = wcslen(str1);
	size_t len2 = wcslen(str2);

	// +1 for the null terminator
	wchar_t* result = (wchar_t*)malloc((len1 + len2 + 1) * sizeof(wchar_t));
	if (!result) {
		return NULL; // 메모리 할당 실패
	}

	wcscpy(result, str1);
	wcscat(result, str2);

	return result;
}

wchar_t* generate_ir(void* AST) {

	wchar_t* result = L"";

	switch (*((ASTType*)AST)) {
	case AST_FunctionDeclaration:


		break;
	}

	return result;
}