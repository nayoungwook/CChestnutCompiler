#include "parser.h"

void* parse_term(wchar_t* str) {
	void* node = parse(str);

	while (peek_token(str) && (peek_token(str)->type == TokMul || peek_token(str)->type == TokDiv)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse(str);

		enum OperatorType op_type = (op == TokMul) ? OpMUL : OpDIV;

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* parse_simple_expression(wchar_t* str) {
	void* node = parse_term(str);

	while (peek_token(str) && (peek_token(str)->type == TokAdd || peek_token(str)->type == TokSub)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_term(str);

		enum OperatorType op_type = (op == TokAdd) ? OpADD : OpSUB;

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		bin_expr->TYPE = AST_BinExpr;
		bin_expr->left = node;
		bin_expr->right = right;
		bin_expr->opType = op_type;

		node = bin_expr;
	}
	return node;
}

void* parse_unary_expression(wchar_t* str) {
	if (peek_token(str) && peek_token(str)->type == TokNot) {
		pull_token(str); // Consume '!'
		UnaryExprAST* unary_expr = (UnaryExprAST*)malloc(sizeof(UnaryExprAST));
		if (!unary_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
		unary_expr->TYPE = AST_UnaryExpr;
		unary_expr->expr = parse_unary_expression(str);
		return unary_expr;
	}
	return parse_simple_expression(str);
}

void* parse_expression(wchar_t* str) {
	void* node = parse_unary_expression(str);

	while (peek_token(str) && (peek_token(str)->type == TokEqual || peek_token(str)->type == TokNotEqual ||
		peek_token(str)->type == TokGreater || peek_token(str)->type == TokLesser ||
		peek_token(str)->type == TokEqualGreater || peek_token(str)->type == TokEqualLesser)) {
		enum TokType op = pull_token(str)->type;
		void* right = parse_unary_expression(str);

		enum OperatorType op_type;
		switch (op) {
		case TokEqual: op_type = OpEQUAL; break;
		case TokNotEqual: op_type = OpNOTEQUAL; break;
		case TokGreater: op_type = OpGREATER; break;
		case TokLesser: op_type = OpLESSER; break;
		case TokEqualGreater: op_type = OpEQUALGREATER; break;
		case TokEqualLesser: op_type = OpEQUALLESSER; break;
		}

		BinExprAST* bin_expr = (BinExprAST*)malloc(sizeof(BinExprAST));
		if (!bin_expr) {
			fprintf(stderr, "Memory allocation failed\n");
			exit(1);
		}
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

	case TokIf: {
		IfStatementAST* if_statement = (IfStatementAST*)malloc(sizeof(IfStatementAST));

		if_statement->TYPE = AST_IfStatement;
		if_statement->body_count = 0;

		pull_token(str); // consume (

		void* condition = parse(str);

		if_statement->condition = condition;

		pull_token(str); // consume {

		while (peek_token(str)->type != TokRBracket) {
			void* body_element = parse(str);

			if (if_statement->body_count == 0) {
				if_statement->body = (void**)malloc(sizeof(void*));
			}
			else {
				if_statement->body = (void**)realloc(if_statement->body, sizeof(void*) * (if_statement->body_count + 1));
			}

			if_statement->body[if_statement->body_count] = body_element;

			if_statement->body_count++;
		}

		return if_statement;
	}

	case TokIdent: {
		IdentifierAST* identifier = (IdentifierAST*)malloc(sizeof(IdentifierAST));

		identifier->TYPE = AST_Identifier;
		identifier->identifier = tok->str;

		return identifier;
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