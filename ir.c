#include "ir.h"

wchar_t* join_string() {

}

wchar_t* generate_ir(void* AST) {

	wchar_t* result = L"";

	switch (*((ASTType*)AST)) {
	case AST_FunctionDeclaration:

		result += L"fun";

		break;
	}

	return result;
}