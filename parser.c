#include "parser.h"

void* parse(wchar_t** str) {
	Token tok = pull_token(str);

	switch (tok.type) {

	case TokNumberLiteral: {
		NumberLiteralAST literal = { AST_NumberLiteral, tok.str };
		return &literal;
	}

	case TokVar: {
		// var i: int;

		VariableDeclarationBundleAST bundles = { AST_VariableDeclarationBundle, NULL, 0 };

		while (1) {
			Token name_token = pull_token(str);
			wchar_t* name = name_token.str;

			pull_token(str);

			wchar_t* type = pull_token(str).str;

			tok = pull_token(str);

			void* declaration = NULL;

			if (tok.type == TokSemiColon) {
				break;
			}
			else if (tok.type == TokComma) {
			}
			else if (tok.type == TokAssign) {
				declaration = parse(str);
			}

			VariableDeclarationAST variable = { AST_VariableDeclaration, name, declaration };

			if (bundles.variable_count == 0) {
				bundles.variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
			}
			else {
				bundles.variable_declarations
					= (VariableDeclarationAST*)realloc(bundles.variable_declarations, sizeof(VariableDeclarationAST) * (bundles.variable_count + 1));
			}

			bundles.variable_declarations[bundles.variable_count] = variable;
			bundles.variable_count++;

			if (pull_token(str).type == TokSemiColon) {
				break;
			}
		}
		return &bundles;
	}
	}
}