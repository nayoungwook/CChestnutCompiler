#include "main.h"

int main(int arc, char* args[]) {

	wchar_t* str = L"var i: int = 10;";

	VariableDeclarationBundleAST ast = *((VariableDeclarationBundleAST*)parse(&str));

	return 0;
}