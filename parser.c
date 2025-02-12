#include "parser.h"

void* parse_term(wchar_t* str) {
	void* node = parse(str);  // for factor

	while (peek_token(str)->type == TokMul || peek_token(str)->type == TokDiv) {
		enum TokType op = pull_token(str)->type;
		void* right = parse(str);

		enum OperatorType op_type;

		switch (op) {
		case TokMul:
			op_type = OpMUL;
			break;
		case TokDiv:
			op_type = OpDIV;
			break;
		}

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}

	return node;
}

void* parse_expression(wchar_t* str) {
	void* node = parse_term(str); // for term

	while (peek_token(str)->type == TokAdd || peek_token(str)->type == TokSub) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_term(str);

		enum OperatorType op_type;

		switch (op) {
		case TokAdd:
			op_type = OpADD;
			break;
		case TokSub:
			op_type = OpSUB;
			break;
		}

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}

	return node;
}

void* parse(wchar_t* str) {
	Token* tok = pull_token(str);

	switch (tok->type) {

	case TokLParen: {
		pull_token(str); // consume (

		void* node = parse_expression(str);

		pull_token(str); // consume )

		return node;
	}

	case TokNumberLiteral: {
		NumberLiteralAST* literal = (NumberLiteralAST*)malloc(sizeof(NumberLiteralAST));
		literal->TYPE = AST_NumberLiteral;
		literal->number_literal = tok->str;

		return literal;
	}

	case TokVar: {
		// var i: int;
		VariableDeclarationBundleAST* bundles = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
		bundles->TYPE = AST_VariableDeclarationBundle;
		bundles->variable_count = 0;
		bundles->variable_declarations = NULL;

		while (1) {
			Token* name_token = pull_token(str);
			wchar_t* name = name_token->str;

			pull_token(str);

			wchar_t* type = pull_token(str)->str;

			tok = pull_token(str);

			void* declaration = NULL;

			if (tok->type == TokSemiColon) {
				break;
			}
			else if (tok->type == TokComma) {
			}
			else if (tok->type == TokAssign) {
				declaration = parse_expression(str);
			}

			VariableDeclarationAST* variable = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST));
			variable->TYPE = AST_VariableDeclaration;
			variable->variable_name = name;
			variable->declaration = declaration;

			if (bundles->variable_count == 0) {
				bundles->variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
			}
			else {
				bundles->variable_declarations
					= (VariableDeclarationAST*)realloc(bundles->variable_declarations, sizeof(VariableDeclarationAST) * (bundles->variable_count + 1));
			}

			bundles->variable_declarations[bundles->variable_count] = variable;
			bundles->variable_count++;

			Token* tok = pull_token(str);
			if (tok->type == TokSemiColon) {
				break;
			}
		}
		return bundles;
	}
	}
}