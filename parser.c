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
		peek_token(str)->type == TokEqualGreater || peek_token(str)->type == TokEqualLesser || peek_token(str)->type == TokAssign)) {
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
		case TokAssign: op_type = OpASSIGN; break;
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

void* consume(wchar_t* str, TokenType expected_type) {
	Token* tok = pull_token(str);

	if (tok->type != expected_type) {
		// Throw error.
		printf("Unexpected token : %d %d\n", tok->type, expected_type);
	}
}

void* parse(wchar_t* str) {
	Token* tok = pull_token(str);

	switch (tok->type) {

	case TokLParen: {
		void* node = parse_expression(str);

		consume(str, TokRParen); // consume )

		return node;
	}

	case TokStringLiteral: {
		StringLiteralAST* literal = (StringLiteralAST*)malloc(sizeof(StringLiteralAST));
		literal->TYPE = AST_StringLiteral;
		literal->string_literal = tok->str;

		return literal;
	}

	case TokNumberLiteral: {
		NumberLiteralAST* literal = (NumberLiteralAST*)malloc(sizeof(NumberLiteralAST));
		literal->TYPE = AST_NumberLiteral;
		literal->number_literal = tok->str;

		return literal;
	}

	case TokIdent: {
		void* result = NULL;

		Token* next_token = peek_token(str);

		if (next_token->type == TokLParen) { // identifier ( 
			result = (FunctionCallAST*)malloc(sizeof(FunctionCallAST));
			((FunctionCallAST*)result)->TYPE = AST_FunctionCall;
			((FunctionCallAST*)result)->parameter_count = 0;

			consume(str, TokLParen);

			while (peek_token(str)->type != TokRParen) {
				void* parameter = parse(str);

				if (((FunctionCallAST*)result)->parameter_count == 0) {
					((FunctionCallAST*)result)->parameters = (void**)malloc(sizeof(void*));
				}
				else {
					((FunctionCallAST*)result)->parameters = (void**)realloc(((FunctionCallAST*)result)->parameters, sizeof(void*) * (((FunctionCallAST*)result)->parameter_count + 1));
				}

				((FunctionCallAST*)result)->parameters[((FunctionCallAST*)result)->parameter_count] = parameter;
				((FunctionCallAST*)result)->parameter_count++;

				if (peek_token(str)->type == TokComma) {
					consume(str, TokComma);
				}
			}

			consume(str, TokRParen);

			if (peek_token(str)->type == TokSemiColon) {
				consume(str, TokSemiColon);
			}

			((FunctionCallAST*)result)->function_name = tok->str;
		}
		else {
			result = (IdentifierAST*)malloc(sizeof(IdentifierAST));
			((IdentifierAST*)result)->TYPE = AST_Identifier;
			((IdentifierAST*)result)->identifier = tok->str;
		}

		return result;
	}

	case TokFunc: {
		// func add(a: int, b: int): int {}
		FunctionDeclarationAST* function_declaration_ast = (FunctionDeclarationAST*)malloc(sizeof(FunctionDeclarationAST));
		function_declaration_ast->TYPE = AST_FunctionDeclaration;
		function_declaration_ast->body_count = 0;

		wchar_t* function_name = pull_token(str)->str;
		function_declaration_ast->function_name = function_name;

		VariableDeclarationBundleAST* parameters = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
		parameters->variable_count = 0;
		parameters->variable_declarations = NULL;

		consume(str, TokLParen); // consume (

		while (peek_token(str)->type != TokRParen) {
			Token* parameter_name_token = pull_token(str);
			wchar_t* parameter_name = parameter_name_token->str;
			// assert parameter_name_token is type identifier

			consume(str, TokColon); // consume :

			Token* parameter_type_token = pull_token(str);
			wchar_t* parameter_type = parameter_type_token->str;
			// assert parameter_name_token is type string for type.

			if (peek_token(str)->type == TokComma) {
				consume(str, TokComma); // consume ,
			}

			VariableDeclarationAST* variable = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST));
			variable->TYPE = AST_VariableDeclaration;
			variable->variable_name = parameter_name;
			variable->variable_type = parameter_type;
			variable->declaration = NULL;

			if (parameters->variable_count == 0) {
				parameters->variable_declarations = (VariableDeclarationAST*)malloc(sizeof(VariableDeclarationAST) * 1);
			}
			else {
				parameters->variable_declarations
					= (VariableDeclarationAST*)realloc(parameters->variable_declarations, sizeof(VariableDeclarationAST) * (parameters->variable_count + 1));
			}

			parameters->variable_declarations[parameters->variable_count] = variable;
			parameters->variable_count++;
		}

		function_declaration_ast->parameters = parameters;

		consume(str, TokRParen); // consume )
		consume(str, TokColon); // consume :

		wchar_t* return_type = pull_token(str)->str;
		function_declaration_ast->return_type = return_type;

		consume(str, TokLBracket); // consume {

		while (peek_token(str)->type != TokRBracket) {
			void* body_element = parse_expression(str);

			if (function_declaration_ast->body_count == 0) {
				function_declaration_ast->body = (void**)malloc(sizeof(void*));
			}
			else {
				function_declaration_ast->body = (void**)realloc(function_declaration_ast->body, sizeof(void*) * (function_declaration_ast->body_count + 1));
			}

			function_declaration_ast->body[function_declaration_ast->body_count] = body_element;

			function_declaration_ast->body_count++;
		}

		return function_declaration_ast;
	}

	case TokIf: {
		IfStatementAST* if_statement = (IfStatementAST*)malloc(sizeof(IfStatementAST));

		if_statement->TYPE = AST_IfStatement;
		if_statement->body_count = 0;

		consume(str, TokLParen); // consume (

		void* condition = parse(str);

		if_statement->condition = condition;

		consume(str, TokLBracket); // consume {

		while (peek_token(str)->type != TokRBracket) {
			void* body_element = parse_expression(str);

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

	case TokVar: {
		// var i: int;
		VariableDeclarationBundleAST* bundles = (VariableDeclarationBundleAST*)malloc(sizeof(VariableDeclarationBundleAST));
		bundles->TYPE = AST_VariableDeclarationBundle;
		bundles->variable_count = 0;
		bundles->variable_declarations = NULL;

		while (1) {
			Token* name_token = pull_token(str);
			wchar_t* name = name_token->str;

			consume(str, TokColon);

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
			variable->variable_type = type;
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