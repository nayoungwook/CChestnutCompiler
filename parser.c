#include "parser.h"

void* parse(wchar_t* str) {
	Token* token = pull_token(&str);

	switch (token->type) {
	case TokVar:
		goto VariableDeclarationAST;
		break;
	}

VariableDeclarationAST:
	{
		VariableDeclarationAST* ast = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST*));

		while (1) {
			wchar_t* name = pull_token(&str)->str;
			void* declaration;

			Token* tok = pull_token(&str);

			if (tok->type == TokAssign) {
				declaration = parse(str);
			}
			else if (tok->type == TokSemiColon) {
				break;
			}
			else if (tok->type == TokComma) {

			}

		}

		return ast;
	};



	return 0;
}