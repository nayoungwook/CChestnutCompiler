#include <stdio.h>
#include <locale.h>
#include <limits.h>
#include <signal.h>
#include <wchar.h>

#include "util.h"
#include "token.h"
#include "parser.h"
#include "ir.h"
#include "builtin.h"

#define DEBUG_VIEW_IR
#define IR_BYTE 1

void print_tokens(TokenizerContext* tokenizer_context, wchar_t* str) {
  while (1) {
    Token* tok = pull_token(tokenizer_context, str);

    printf("%S %d %d\n", tok->str, tok->type, tok->line_number);

    if (tok->type == TokEOF) break;
  }
}

void initialize_primitive_types(ParserContext* parser_context) {
  insert_set_symbol(parser_context->primitive_types, L"number");
  insert_set_symbol(parser_context->primitive_types, L"int");
  insert_set_symbol(parser_context->primitive_types, L"float");
  insert_set_symbol(parser_context->primitive_types, L"double");
  insert_set_symbol(parser_context->primitive_types, L"bool");
  insert_set_symbol(parser_context->primitive_types, L"char");
}
void print_indent(int depth) {
    for(int i=0;i<depth;i++) wprintf(L"  ");
}

void print_AST(void* ast_ptr, int depth) {
    if (!ast_ptr) return;
    ASTType type = *((ASTType*)ast_ptr);
    print_indent(depth);
    switch(type){
        case AST_NumberLiteral: {
            NumberLiteralAST* n = (NumberLiteralAST*)ast_ptr;
            wprintf(L"NumberLiteral: %ls (type: %ls)\n", n->number_literal_token?n->number_literal_token->str:L"(null)", n->numeric_type?n->numeric_type:L"(null)");
            break;
        }
        case AST_Identifier: {
            IdentifierAST* n = (IdentifierAST*)ast_ptr;
            wprintf(L"Identifier: %ls\n", n->identifier?n->identifier->str:L"(null)");
            if(n->attribute) print_AST(n->attribute, depth+1);
            break;
        }
        case AST_BinExpr: {
            BinExprAST* n = (BinExprAST*)ast_ptr;
            wprintf(L"BinExpr: (op=%d)\n", n->opType);
            print_AST(n->left, depth+1);
            print_AST(n->right, depth+1);
            break;
        }
        case AST_UnaryExpr: {
            UnaryExprAST* n = (UnaryExprAST*)ast_ptr;
            wprintf(L"UnaryExpr\n");
            print_AST(n->expr, depth+1);
            break;
        }
        case AST_VariableDeclaration: {
            VariableDeclarationAST* n = (VariableDeclarationAST*)ast_ptr;
            wprintf(L"VariableDecl: %ls\n", n->variable_name_token?n->variable_name_token->str:L"(null)");
            if(n->declaration) print_AST(n->declaration, depth+1);
            break;
        }
        case AST_VariableDeclarationBundle: {
            VariableDeclarationBundleAST* n = (VariableDeclarationBundleAST*)ast_ptr;
            wprintf(L"VariableDeclBundle\n");
            for(int i=0;i<n->variable_count;i++)
                print_AST(n->variable_declarations[i], depth+1);
            break;
        }
        case AST_StringLiteral: {
            StringLiteralAST* n = (StringLiteralAST*)ast_ptr;
            wprintf(L"StringLiteral: %ls\n", n->string_literal_token?n->string_literal_token->str:L"(null)");
            break;
        }
        case AST_IfStatement: {
            IfStatementAST* n = (IfStatementAST*)ast_ptr;
            wprintf(L"IfStatement (type=%d)\n", n->if_type);
            print_AST(n->condition, depth+1);
            for(int i=0;i<n->body_count;i++)
                print_AST(n->body[i], depth+1);
            if(n->next_statement) print_AST(n->next_statement, depth+1);
            break;
        }
        case AST_FunctionDeclaration: {
            FunctionDeclarationAST* n = (FunctionDeclarationAST*)ast_ptr;
            wprintf(L"FunctionDecl: %ls\n", n->function_name_token?n->function_name_token->str:L"(null)");
            print_AST(n->parameters, depth+1);
            for(int i=0;i<n->body_count;i++)
                print_AST(n->body[i], depth+1);
            break;
        }
        case AST_FunctionCall: {
            FunctionCallAST* n = (FunctionCallAST*)ast_ptr;
            wprintf(L"FunctionCall: %ls\n", n->function_name_token?n->function_name_token->str:L"(null)");
            for(int i=0;i<n->parameter_count;i++)
                print_AST(n->parameters[i], depth+1);
            if(n->attribute) print_AST(n->attribute, depth+1);
            break;
        }
        case AST_ArrayDeclaration: {
            ArrayDeclarationAST* n = (ArrayDeclarationAST*)ast_ptr;
            wprintf(L"ArrayDecl (count=%d)\n", n->element_count);
            for(int i=0;i<n->element_count;i++)
                print_AST(n->elements[i], depth+1);
            break;
        }
        case AST_ArrayAccess: {
            ArrayAccessAST* n = (ArrayAccessAST*)ast_ptr;
            wprintf(L"ArrayAccess\n");
            print_AST(n->target_array, depth+1);
            for(int i=0;i<n->access_count;i++)
                print_AST(n->indexes[i], depth+1);
            if(n->attribute) print_AST(n->attribute, depth+1);
            break;
        }
        case AST_Class: {
            ClassAST* n = (ClassAST*)ast_ptr;
            wprintf(L"Class: %ls\n", n->class_name_token?n->class_name_token->str:L"(null)");
            if(n->parent_class_name_token)
                print_indent(depth+1),wprintf(L"Parent: %ls\n", n->parent_class_name_token->str);
            for(int i=0;i<n->member_variable_bundle_count;i++)
                print_AST(n->member_variables[i], depth+1);
            for(int i=0;i<n->member_function_count;i++)
                print_AST(n->member_functions[i], depth+1);
            if(n->constructor) print_AST(n->constructor, depth+1);
            break;
        }
        case AST_Return: {
            ReturnAST* n = (ReturnAST*)ast_ptr;
            wprintf(L"Return\n");
            print_AST(n->expression, depth+1);
            break;
        }
        // ... 나머지 타입(For, Increase, Decrease 등)은 위 스타일로 추가 ...
        default:
            wprintf(L"Unknown AST Type: %d\n", type);
            break;
    }
}

void parse_file(IrGenContext* ir_context, ParserContext* parser_context, TokenizerContext* tokenizer_context, const wchar_t* file_name) {
  wchar_t* file = read_file(tokenizer_context, file_name);

  int ast_count = 0;
  int class_ast_count = 0;

  void** asts = NULL;
  void** class_asts = NULL;

  setlocale(LC_ALL, "");

  //  print_tokens(tokenizer_context, file);
  
  while (peek_token(tokenizer_context, file)->type != TokEOF) {
    set_file_string(parser_context, file);
    void* ast = parse(tokenizer_context, parser_context, file);

    if (*((ASTType*)ast) == AST_Class) {
      if (class_ast_count == 0) {
	class_asts = (void**)safe_malloc(sizeof(void*));
	class_asts[class_ast_count] = ast;
	class_ast_count++;
      }
      else {
	class_asts = (void**)realloc(class_asts, sizeof(void*) * (class_ast_count + 1));
	class_asts[class_ast_count] = ast;
	class_ast_count++;
      }
    }

    if (ast_count == 0) {
      asts = (void**)malloc(sizeof(void*));
      asts[ast_count] = ast;
      ast_count++;
    }
    else {
      asts = (void**)realloc(asts, sizeof(void*) * (ast_count + 1));
      asts[ast_count] = ast;
      ast_count++;
    }
  }

  printf("parsing done :D\n");
  
  int i = 0;
  
  for (i = 0; i < class_ast_count; i++) {
    initialize_class_data(ir_context, parser_context, ((ClassAST*)class_asts[i]));
  }

  for (i = 0; i < ast_count; i++) {
    create_ir(ir_context, parser_context, asts[i]);
  }

#ifdef DEBUG_VIEW_IR
  wprintf(L" %s\n", ir_context->string_builder->str);
#endif

  wchar_t* original_name = substr(file_name, 0, wcslen(file_name) - 6);
  wchar_t* new_file_name = join_string(original_name, L".ir");

  write_file(new_file_name, ir_context->string_builder->str);
}

int main(int arc, char* args[]) {
  setlocale(LC_ALL, "");

  TokenizerContext* tokenizer_context = create_tokenizer_context();
  ParserContext* parser_context = create_parser_context();
  IrGenContext* ir_context = create_ir_context();

  initialize_primitive_types(parser_context);
  initialize_builtin_functions(parser_context);
  initialize_byte_table();

  parse_file(ir_context, parser_context, tokenizer_context, L"main.cn");

  return 0;
}
