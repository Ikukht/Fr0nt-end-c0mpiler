/*
File name:			parser.c
Compiler:			MS Visual Studio 2015
Author:				Kishan Sondagar 040845747, Ilya Kukhtiy, 040778822
Course:				CST 8152 – Compilers. Lab Section: 014
Assignment:			3
Date:				December 6-7, 2018
Professor:			Svillen Ranev
Purpose:			Implementing a Parser and more...

Function list:
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
*/

#include "parser.h"

/*******************************************************************************
Authors:			Svillen Ranev
*******************************************************************************/
void parser(void) {
	lookahead = malar_next_token();				/* Initialises first token to lookahead*/
	program(); match(SEOF_T, NO_ATTR);			/*Matches Source end-of-file token with no attribute*/
	gen_incode("PLATY: Source file parsed");	
}

/*******************************************************************************
Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {

	if (lookahead.code != pr_token_code) {						  /* Checks if tokens match*/
		syn_eh(pr_token_code);									  /* Print error*/
		return;
	}

	if (lookahead.code == SEOF_T) {								  /* Checks if current input token is End-of-File*/
		return;
	}
	switch (pr_token_code) {

	case KW_T:													  /* Keyword token */
	case LOG_OP_T:												  /* Logical operator token */
	case ART_OP_T:												  /* Arithmetic operator token */
	case REL_OP_T:												  /* Relational operator token */
		if (lookahead.attribute.get_int != pr_token_attribute) {  /* Compares attributes*/
			syn_eh(pr_token_code);
			return;
		}
	default:
		break;
	}

	lookahead = malar_next_token();								  /* Advance to next token*/
	if (lookahead.code == ERR_T) {								  /* Check if new token is an error*/
		syn_printe();											  /* Print out error*/
		lookahead = malar_next_token();							  /* Advance to next token again*/
		synerrno++;												  /* Increment */
		return;
	}

}

/*******************************************************************************
<program>  ->
PLATYPUS {<opt_statements>}

First(program) =
{PLATYPUS}

Author :			Svillen Ranev
*******************************************************************************/
void program() {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();  /* Matches Keyword token with PLATYPUS attribute*/
	match(RBR_T, NO_ATTR);											 /* Matches Right brace token*/
	gen_incode("PLATY: Program parsed");
}

/*******************************************************************************
<statements> ->
<statement> <statements’>

First(statements) =
{First(statment)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void statements() {

	statement();		/* statement non-terminal */
	statements_p();		/* statement prime non-terminal */

}

/*******************************************************************************
<statements’> ->
<statement> <statements’> | epsilon

First(statements’) =
{First(statement)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void statements_p() {

	switch (lookahead.code) {
	case AVID_T:								/* Arithmetic Variable identifier token */
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:									/*Keyword token*/
		switch (lookahead.attribute.kwt_idx) {	
		case IF:
		case WHILE:
		case READ:								
		case WRITE:								
			statement();						/*statement non-terminal is executed if token attribute is one of IF, WHILE, READ, WRITE*/
			statements_p();	
			break;
		}
	default: 
		break;
	}
	return;
}

/*******************************************************************************
<opt_statements> -> <statements> | epsilon

First(opt_statements) =
{First(statements), epsilon}

Author :			Svillen Ranev
*******************************************************************************/
void opt_statements() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*******************************************************************************
<statement> ->
<assignment statement>
| <selection statement>
| <iteration statement>
| <input statement>
| <output statement>

First Set
First(<statement>) =
{First(<assignment statement>),
First(<selection statement >),
First(<iteration statement >),
First(<input statement >),
First(<output statement>),
} = {avid, svid, if, while, read, write}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void statement() {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		/*Checks for Arithmetic and String variable identifier and executes assignment_statement()*/
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		/*Checks for IF, WHILE, READ, WRITE attributes*/
		case IF:
			selection_statement();
			break;
		case WHILE:
			iteration_statement();
			break;
		case READ:
			input_statement();
			break;
		case WRITE:
			output_statement();
			break;
		default:
			/*Prints error*/
			syn_printe();
			break;
		}

		break;
	default:
		syn_printe();
		break;
	}
	return;
}

/*******************************************************************************
<assignment statement> ->
<assignment expression>;

First(<assignment statement>) = {First(<assignment expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void assignment_statement() {

	assignment_expression();							/* Assignment expression non-termial*/
	match(EOS_T, NO_ATTR);								/*Matches End of statement *(semi - colon)*/
	gen_incode("PLATY: Assignment statement parsed");
}

/*******************************************************************************
< assignment expression> ->
AVID = <arithmetic expression>
| SVID = <string expression>

First(<assignment expression >) = {avid, svid}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void assignment_expression() {

	switch (lookahead.code) {
		/*Checks for variable identifier and its attribute*/
	case AVID_T:
		match(lookahead.code, lookahead.attribute.get_int);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(lookahead.code, lookahead.attribute.get_int);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
		
	default:
		/*entering error state */
		syn_printe();
		return;
	}
	return;
}

/*******************************************************************************
<selection statement> ->
IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
ELSE { <opt_statements> } ;

First(<selection statement>) = {if}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void selection_statement() {

	match(KW_T, IF);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);
	conditional_expression();			/*conditional expression non-terminal*/
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();					/*optinal statements non-terminal*/
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();					/*optinal statements non-terminal*/
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");

}

/*******************************************************************************
<iteration statement> ->
WHILE <pre-condition> (<conditional expression>)
REPEAT { <statements>};

First(<iteration statement>) = {while}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void iteration_statement() {

	match(KW_T, WHILE);
	precondition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);					/*Match REPEAT keyword*/
	match(LBR_T, NO_ATTR);					/*Match Left brace token */
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");

}

/*******************************************************************************
<pre-condition> ->
TRUE | FALSE

First(<pre-condition>) =
{TRUE, FALSE}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void precondition() {

	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		/* Checks if current token is a Keyword specificly TRUE or FALSE*/
		case TRUE:
		case FALSE:
			match(lookahead.code, lookahead.attribute.kwt_idx);  /*Match Keyword*/
			break;
		default:
			/*Print error if current token is keyword, but not TRUE or FALSE*/
			syn_printe();
			break;
		}
		return;
	default:
		syn_printe();
		return;
	}

}

/*******************************************************************************
<input statement> ->
READ (<variable list>);

First(<input statement >) =
{read}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void input_statement() {
	match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list();	/*Match keyword READ. Match Left parenthesis token. Execute non-terminal variable_list*/
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);				/*Match Right parenthesis token. Match End of statement *(semi - colon)*/
	gen_incode("PLATY: Input statement parsed");
}

/*******************************************************************************
<output statement> ->
WRITE (<opt_variable list>);
| WRITE (STR_T);

First(<output statement>) =
{write}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************/
void output_statement() {

	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	opt_variable_list();		/*<opt_variable_list> non-terminal*/
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");

}


/********************************************************************************************
< opt_variable list >->
<variable list> | epsilon

First(<opt_variable list >) =
{avid_t, svid_t, str_t, epsilon}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void opt_variable_list() {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		/*If token is variable identifier execute optional variable list non-terminal*/
		variable_list();
		break;
	case STR_T:
		/*Token is string literal with no attribute*/
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		return;
	}
	return;

}

/********************************************************************************************
< variable list > ->
< variable identifier>< variable list ‘>

First(<variable list>) =
{First(<variable identifier>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void variable_list() {

	variable_identifier();					/*Non-terminal variable_identifier*/
	variable_list_p();						/*Non-terminal variable_list_p*/
	gen_incode("PLATY: Variable list parsed");
	return;
}

/********************************************************************************************
< variable list’ > ->
<variable identifier >< variable list ‘> | epsilon

First(<variable list’>) =
{First(<variable identifier>) | epsilon}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void variable_list_p() {

	switch (lookahead.code) {
	case COM_T:
		/*Match comma token with empty attribute to current token lookahead*/
		match(lookahead.code, NO_ATTR);
		variable_identifier();		/*non-terminal variable_identifier*/
		variable_list_p();			/*non-terminal variable_list_p*/
		break;
	default:
		return;
	}

}

/********************************************************************************************
<variable identifier>->
avid_t | svid_t

First(<variable identifier>)  =
{avid_t, svid_t}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void variable_identifier() {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		/*Match variable identifier with no attribute*/
		match(lookahead.code, NO_ATTR);
		break;
	default:
		/*Print error*/
		syn_printe();
		break;
	}
	return;

}

/********************************************************************************************
<arithmetic expression> - >
<unary arithmetic expression>
| <additive arithmetic expression>

First(<arithmetic expression>) =
{First(<unary arithmetic expression>),
First(<additive arithmetic expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void arithmetic_expression() {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			/* Current token is arithmetic operator PLUS or MINUS to define unary expression*/
			unary_arithmetic_expression();						/*<unary_arithmetic_expression> non-terminal*/
			gen_incode("PLATY: Unary arithmetic expression parsed");
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		default:
			/*Print error*/
			syn_printe();
			break;
		}

		return;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		/*Token is one of Arithmetic identifier, Floating point literal, Integer literal or Left parenthesis token*/
		additive_arithmetic_expression();							/*<additive_arithmetic_expression> non-terminal*/
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		syn_printe();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	}
	return;
}

/********************************************************************************************
<unary arithmetic expression> ->
+  <primary arithmetic expression>
| - <primary arithmetic expression>

First(<unary arithmetic expression >) = {+,-}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void unary_arithmetic_expression() {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			/*Arthmetic operators PLUS or MINUS*/
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_arithmetic_expression();					/*<primary_arithmetic_expression> non-terminal*/
			break;
		default:
			/*Error handling*/
			syn_printe();
			break;
		}

		return;
	default:
		syn_printe();
		break;
	}
	return;
}

/********************************************************************************************
< additive arithmetic expression > ->
<multiplicative arithmetic expression><additive arithmetic expression’>

First(<additive arithmetic expression >) =
{First(<multiplicative arithmetic expression >)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void additive_arithmetic_expression() {

	multiplicative_arithmetic_expression();		/*<multiplicative_arithmetic_expression> non-terminal*/
	additive_arithmetic_expression_p();			/*<additive_arithmetic_expression_p> non-terminal*/

}

/********************************************************************************************
< additive arithmetic expression ‘> ->
+<multiplicative arithmetic expression><additive arithmetic expression’>
| -<multiplicative arithmetic expression><additive arithmetic expression’>
| epsilon

First(<additive arithmetic expression ‘>) =
{First(<add op>), epsilon}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void additive_arithmetic_expression_p() {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			/*Arthmetic operators PLUS or MINUS*/
			match(lookahead.code, lookahead.attribute.arr_op);	/*Assign next token to lookahead*/
			multiplicative_arithmetic_expression();				/*<multiplicative_arithmetic_expression> non-terminal*/
			additive_arithmetic_expression_p();					/*<additive_arithmetic_expression_p> non-terminal*/
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			break;
		}

		return;
	default:
		break;
	}
	return;
}

/********************************************************************************************
< multiplicative arithmetic expression > ->
< primary arithmetic expression >< multiplicative arithmetic expression ‘>

First(<multiplicative arithmetic expression >) =
{First(<primary arithmetic expression >)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void multiplicative_arithmetic_expression() {

	primary_arithmetic_expression();			/*<primary_arithmetic_expression> non-terminal*/
	multiplicative_arithmetic_expression_p();	/*<multiplicative_arithmetic_expression_p> non-terminal*/

}

/********************************************************************************************
< multiplicative arithmetic expression ‘> ->
*< primary arithmetic expression >< multiplicative arithmetic expression ‘>
| /< primary arithmetic expression >< multiplicative arithmetic expression ‘>
| epsilon

First(<multiplicative arithmetic expression ‘>) =
{First(<multi op>), epsilon}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void multiplicative_arithmetic_expression_p() {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MULT:
		case DIV:
		/*Current token is an Arithmetic operator Divider or Multiplier*/
			match(lookahead.code, lookahead.attribute.arr_op);			/*Get next token*/
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();				/*<multiplicative_arithmetic_expression_p> non-terminal*/
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			break;
		}

		return;
	default:
		break;
	}
	return;
}

/********************************************************************************************
<primary arithmetic expression> ->
AVID_T
| FPL_T
| INL_T
| (<arithmetic expression>)

First(<primary arithmetic expression>) =
{avid_t, fpl_t, inl_t, (<arithmetic expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void primary_arithmetic_expression() {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		/*Token is Arithmetic identifier, Floating point literal or Integer literal*/
		match(lookahead.code, NO_ATTR);				
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();			/*<arithmetic_expression> non-terminal*/
		match(RPR_T, NO_ATTR);
		break;
	default:
		/*Error printing*/
		syn_printe();
	}

	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/********************************************************************************************
<string expression> ->
<primary string expression>
| <string expression>  #  <primary string expression>

First(<string expression>) =
{First(<primary string expression>),
{First(<string expression'>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void string_expression() {

	primary_string_expression();
	string_expression_p();				/*<string_expression_p> non-terminal*/
	gen_incode("PLATY: String expression parsed");

}

/********************************************************************************************
< string expression ‘> ->
< string expression ‘>< primary string expression > | epsilon

First(<string expression >) =
{ First(<primary string expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void string_expression_p() {

	if (lookahead.code != SCC_OP_T) { /*Function returns if current token is not String concatenation operator token*/
		return;
	}

	match(lookahead.code, NO_ATTR);		/* Matching String concatenation operator with no attribute and gets next token*/
	primary_string_expression();	/*<primary_string_expression> non-terminal*/
	string_expression_p();
}

/********************************************************************************************
<primary string expression> ->
SVID_T
| STR_T

First(<primary string expression>) =
{svid_t, str_t}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void primary_string_expression() {

	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		/* Primary string expression parsing*/
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	default:
		/*Error printing*/
		syn_printe();
		return;
	}

}

/********************************************************************************************
<conditional expression> ->
<logical OR  expression>

First(<conditional expression>) =
{First(<logical  OR expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void conditional_expression() {

	logical_or_expression();					/*<logical_or_expression> non-terminal*/
	gen_incode("PLATY: Conditional expression parsed");
}

/********************************************************************************************
< logical or expression > ->
<logical or expression’><relational expression>

First(<logical  or expression>) =
{First(<relational expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void logical_or_expression() {

	logical_and_expression();		/*<logical_and_expression> non-terminal*/
	logical_or_expression_p();		/*<logical_or_expression_p> non-terminal*/

}

/********************************************************************************************
< logical OR expression‘> ->
<logical OR expression’><relational expression> | epsilon

First(<logical OR expression’>) = {.OR.}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void logical_or_expression_p() {

	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			/*Logical operator token attribute is OR*/
			match(lookahead.code, OR);
			logical_and_expression();				/*<logical_and_expression> non-terminal*/
			logical_or_expression_p();				/*<logical_or_expression_p> non-terminal*/
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		default:
			break;
		}
		return;
	default:
		break;
	}
	return;
}

/********************************************************************************************
< logical AND expression > ->
<logical AND expression’><relational expression>

First(<logical  AND expression>) =
{First(<relational expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void logical_and_expression() {

	relational_expression();			/*<relational_expression> non-terminal*/
	logical_and_expression_p();			/*<logical_and_expression_p> non-terminal*/

}

/********************************************************************************************
< logical AND expression‘> ->
<logical AND expression’><relational expression> | epsilon

First(<logical AND expression’>) = {.AND.}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void logical_and_expression_p() {

	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			/*Logical operator token with attribute AND*/
			match(lookahead.code, AND);
			relational_expression();							/*<relational_expression> non-terminal*/
			logical_and_expression_p();							/*<logical_and_expression_p> non-terminal*/
			gen_incode("PLATY: Logical AND expression parsed");	/*Succesfull parsing of Logical AND expression*/
		default:
			break;
		}

		return;
	default:
		break;
	}
	return;
}

/********************************************************************************************
<relational expression> ->
<primary a_relational expression>  ==  <primary a_relational expression>
| <primary a_relational  expression>  <>  <primary a_relational  expression>
| <primary a_relational  expression>  >   <primary a_relational  expression>
| <primary a_relational expression>  <   <primary a_relational expression>
| <primary s_relational expression>  ==  <primary s_relational expression>
| <primary s_relational  expression>  <>  <primary s_relational  expression>
| <primary s_relational  expression>  >   <primary s_relational  expression>
| <primary s_relational expression>  <   <primary s_relational expression>

First (<relational expression>) =
{First(<primary a_relational expression>),
First(<primary s_relational expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void relational_expression() {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		/* Arithmetic variable identifier, Floating point literal or Integer literal will precide to primary_a_relational_expression non-terminal*/
		primary_a_relational_expression();
		relational_operator();
		primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		/*String Variable Identifier or String Literal will precide to primary_s_relational_expression non-terminal */
		primary_s_relational_expression();
		relational_operator();
		primary_s_relational_expression();
		break;
	default:
		/*Error printing*/
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/********************************************************************************************
<primary a_relational expression> ->
AVID_T
| FPL_T
| INL_T

First(<primary a_relational expression>) =
{avid_t, fpl_t, inl_t}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void primary_a_relational_expression() {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		/*Lookahead token must be Arithmetic Variable Identifier, Floating point literal or Integer literal*/
		match(lookahead.code, NO_ATTR);
		break;
	default:
		/*Error handling*/
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/********************************************************************************************

<primary s_relational expression> ->
<primary string expression>

First(<primary s_relational expression>) =
{<primary string expression>}
First{<primary string expression>} =
{svid_t, str_t}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void primary_s_relational_expression() {

	switch (lookahead.code) {
	case STR_T:
	case SVID_T:
		/*String literal or String Variable Identifier will call primary_string_expression non-terminal*/
		primary_string_expression();
		break;
	default:
		/*Erro handling executed*/
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/********************************************************************************************
<relational ops> -> == | <> | < | >

First (<relational expression>) =
{First(<primary a_relational expression>),
First(<primary s_relational expression>)}

Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void relational_operator() {

	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.rel_op) {
			/*relational_operator must be one of ==, <>, > or <*/
		case EQ:
			match(lookahead.code, EQ);
			break;
		case NE:
			match(lookahead.code, NE);
			break;
		case GT:
			match(lookahead.code, GT);
			break;
		case LT:
			match(lookahead.code, LT);
			break;
		default:
			syn_printe();
			break;
		}
		return;
	}
	/*Error printing function*/
	syn_printe();
}

/********************************************************************************************
Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void gen_incode(char * code) {
	printf("%s\n", code);	/*Print function*/
}

/********************************************************************************************
Author :			Ilya Kukhtiy, Kishan Sondagar
*******************************************************************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;										/* Increments error counter*/

	while (lookahead.code != sync_token_code) {
		lookahead = malar_next_token();				/* Gets next token*/

		if (lookahead.code == SEOF_T) {				/* Checks if end of file is reached*/
			if (lookahead.code == sync_token_code)	/* Token is found and reaches end of file*/
				return;
			else
				exit(synerrno);						/* Token is not found and reaches end of file*/
		}
		if (lookahead.code == sync_token_code) {	/* Any token, but SEOF_T, is found*/
			lookahead = malar_next_token();			/* Advances forward */
			return;
		}
	}

}

/********************************************************************************************
Author :			Svillen Ranev
*******************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/