#include "main.h"

int main(int arc, char* args[]) {

	wchar_t* str = L"var a: bool = 3 == 3;";

	while (1) {
		Token* tok = pull_token(str);

		printf("%S %d\n", tok->str, tok->type);

		if (tok->type == TokEOF) break;
	}

	return 0;
}