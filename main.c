#include "main.h"

int main(int arc, char* args[]) {

	wchar_t* str = L"var i: int = 10;";

	while (1) {
		Token* token = pull_token(&str);
		printf("%S %d\n", token->str, token->type);

		if (token->type == EOF) break;
	}

	return 0;
}