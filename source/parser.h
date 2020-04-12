/*
File name:			parser.h
Compiler:			MS Visual Studio 2015
Author:				Kishan Sondagar 040845747, Ilya Kukhtiy, 040778822
Course:				CST 8152 – Compilers. Lab Section: 014
Assignment:			3
Date:				December 6-7, 2018
Professor:			Svillen Ranev
Purpose:			h-file for implementing a Parser and more...
*/


#include "buffer.h"
#include "token.h"

/*Constants*/
#define NO_ATTR NULL
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

static Token lookahead;
int synerrno = 0;

extern Token malar_next_token();
extern char * kw_table[];
extern Buffer * str_LTBL;
extern int line;
/*Function declarations*/
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void program();
void statements();
void statements_p();
void opt_statements();
void statement();
void assignment_statement();
void assignment_expression();
void selection_statement();
void iteration_statement();
void precondition();
void input_statement();
void output_statement();
void opt_variable_list();
void variable_list();
void variable_list_p();
void variable_identifier();
void arithmetic_expression();
void unary_arithmetic_expression();
void additive_arithmetic_expression();
void additive_arithmetic_expression_p();
void multiplicative_arithmetic_expression();
void multiplicative_arithmetic_expression_p();
void primary_arithmetic_expression();
void string_expression();
void string_expression_p();
void primary_string_expression();
void conditional_expression();
void logical_or_expression();
void logical_or_expression_p();
void logical_and_expression();
void logical_and_expression_p();
void relational_expression();
void primary_a_relational_expression();
void primary_s_relational_expression();
void relational_operator();
void gen_incode(char * code);
void syn_eh(int sync_token_code);
void syn_printe();
