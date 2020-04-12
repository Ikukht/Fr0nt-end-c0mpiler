/*****************************************************************************************************
*	File name : scanner.c
*	Compiler : MS Visual Studio 2015
* 	Author : Ilya Kukhtiy 040778822 , Kishan Sondagar 040845747
* 	Course : CST 8152 – Compilers, Lab Section : 12 (Kishan), 14 (Ilya)
* 	Assignment : 2
* 	Date : November 8, 2018
* 	Professor : Svillen Ranev
* 	Purpose : Specs provided by Svillen Ranev, the purpose of this file is to implement
*				a Lexical Analyzer (Scanner).
* 	Function list :	scanner_init(|);
*					malar_next_token(),
*					get_next_state(),
*					char_class(),
*					aa_func02(),
*					aa_func03(),
*					aa_func05(),
*					aa_func08(),
*					aa_func10(),
*					aa_func11(),
*					aa_func12(),
*					iskeyword(),
*					findIndexKW()
*****************************************************************************************************/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL;	/*String literal table */
int line;					/* current line number of the source code */
extern int scerrnum;		/* defined in platy_st.c - run-time error number */

							/* Local(file) global objects - variables */
static Buffer *lex_buf;		/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf;		/*pointer to input source buffer*/
							/* No other global variable declarations/definitiond are allowed */

							/* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/*keywords lookup functuion */


												/********************************************************************************************
												Purpose :			Initializes scanner
												Author :			Svillen Ranev
												Version:	        1.0
												Called functions :	b_isempty(), b_rewind(), b_clear()
												parameters :		Buffer: psc_buf
												Return value :		int, 1 for failure - 0 for success
												Algorithm :			Provided by Svillen Ranev
												*******************************************************************************************/
int scanner_init(Buffer * psc_buf)
{
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
												/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/********************************************************************************************
Purpose :			The purpose for part 1 is to catch lexical or illegal characters
and set tokens accordingly.
part two: is to go finite state machine and the transition table
driven scanner and to generate tokens accordingly.
Author :			Ilya Kukhtiy , Kishan Sondagar
Version:	        1.0
Called functions :	b_getc(), b_retract(), b_getcoffset(), b_mark(), b_reset(),
b_limit(), b_addc(), get_next_state(), b_allocate(), b_free()
parameters :		char:c
Return value :		Token t
Algorithm :			Set the state and token structure to zero.
in a infinite while loop, get each character of the buffer
untill it reaches a token and calls its function to check
the corresponding tables. Will set errors accordingly.
*******************************************************************************************/
Token malar_next_token(void)
{
	/*token to return and setting structure to 0*/
	Token t = { 0 };
	/*input*/
	unsigned char c;
	/*set state to 0*/
	int state = 0;
	/*start of the lexeme offset*/
	short lexstart;
	/*end of the lexeme offset*/
	short lexend;
	/*accept in not accepting*/
	int accept = NOAS;

	/*endless only broken when a token is returned*/
	while (1) {
		/*get the next character*/
		c = b_getc(sc_buf);

		/* Part 1:*/

		/*check for end of file*/
		if (c == EOF) {
			t.code = SEOF_T;
			return t;
		}
		/*check for end of file*/
		if (c == 255) {
			t.code = SEOF_T;
			return t;
		}
		/*check for end of file*/
		if (c == '\0') {
			t.code = SEOF_T;
			return t;
		}
		/*white space*/
		if (c == ' ')
			continue;
		/*new line*/
		if (c == '\n') {
			line++;
			continue;
		}
		/*tab*/
		if (c == '\t')
			continue;
		/*verticle space*/
		if (c == '\v')
			continue;
		/*plus*/
		if (c == '+') {
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}
		/*minus*/
		if (c == '-') {
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}
		/*muliply*/
		if (c == '*') {
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}
		/*divide*/
		if (c == '/') {
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}
		/*greater than*/
		if (c == '>') {
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}
		/*less than*/
		if (c == '<') {
			c = b_getc(sc_buf);
			if (c == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
		}
		if (c == '=') {
			c = b_getc(sc_buf);
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;
			}
		}
		if (c == ',') {
			t.code = COM_T;
			return t;
		}
		if (c == ';') {
			t.code = EOS_T;
			return t;
		}
		if (c == '.') {
			lexstart = b_getcoffset(sc_buf);
			b_mark(sc_buf, lexstart);
			c = b_getc(sc_buf);

			if (c == 'A') {
				if (b_getc(sc_buf) == 'N') {
					if (b_getc(sc_buf) == 'D') {
						if (b_getc(sc_buf) == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
				b_reset(sc_buf);
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;
				return t;
			}
			else if (c == 'O') {
				if (b_getc(sc_buf) == 'R') {
					if (b_getc(sc_buf) == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
				b_reset(sc_buf);
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;
				return t;
			}
		}
		if (c == '#') {
			t.code = SCC_OP_T;
			return t;
		}
		if (c == '{') {
			t.code = LBR_T;
			return t;
		}
		if (c == '}') {
			t.code = RBR_T;
			return t;
		}
		if (c == '(') {
			t.code = LPR_T;
			return t;
		}

		if (c == ')') {
			t.code = RPR_T;
			return t;
		}
		if (c == '!') {
			if ((c = b_getc(sc_buf)) == '!') {
				while ((c = b_getc(sc_buf)) != '\n' && c != '\0') {
					continue;
				}
				line++;
				continue;
			}
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
				while ((c = b_getc(sc_buf)) != '\n') {
					if (c == '\0')
						return t;
					continue;
				}
				line++;
				return t;
			}
		}
		if (c == '"') {
			int i = 0;
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));
			while (1) {
				c = b_getc(sc_buf);
				if (c == '\n')
					line++;
				else if (c == '\0') {
					lexend = b_getcoffset(sc_buf);
					b_reset(sc_buf);
					b_retract(sc_buf);
					t.code = ERR_T;
					if ((lexend - lexstart) > ERR_LEN) {
						for (i = 0; i < 17; i++)
							t.attribute.err_lex[i] = b_getc(sc_buf);
						t.attribute.err_lex[ERR_LEN - 3] = '.';
						t.attribute.err_lex[ERR_LEN - 2] = '.';
						t.attribute.err_lex[ERR_LEN - 1] = '.';
						t.attribute.err_lex[ERR_LEN] = '\0';
						while (i < lexend) {
							b_getc(sc_buf);
							i++;
						}
					}
					else {

						for (i = 0; i < (lexend - lexstart); i++) {
							t.attribute.err_lex[i] = b_getc(sc_buf);
						}
						t.attribute.err_lex[i] = '\0';
					}
					return t;
				}
				else if (c == '"') {
					b_retract(sc_buf);
					lexend = b_getcoffset(sc_buf);
					b_reset(sc_buf);
					t.code = STR_T;
					t.attribute.str_offset = b_limit(str_LTBL);

					for (i = lexstart; i < lexend; i++)
						b_addc(str_LTBL, b_getc(sc_buf));

					b_addc(str_LTBL, '\0');
					b_getc(sc_buf);
					return t;
				}
			}
		}

		/* Part 2: DFA */
		if (isdigit(c) || isalpha(c) || c == 0) {

			b_retract(sc_buf);
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));/* Stores begginign of lexeme */
			c = b_getc(sc_buf);/* Next symbole to be checked in transition table */
			state = 0; /* Sets beggining state */
			state = get_next_state(state, c, &accept); /* Gets next state */


			while (accept == NOAS) { /* Keep going while NOT in accepting state */
				c = b_getc(sc_buf);/* Get next char */
				state = get_next_state(state, c, &accept);
			}

			if (accept == ASWR) /* CHeck if Accepting state is returned */
				b_retract(sc_buf);  /* Retract */

			lexend = b_getcoffset(sc_buf); /* Sets lexend  */

			if (!(lex_buf = b_allocate((lexend - lexstart) + 1, 0, FIXED_MODE))) { /* Creates lexeme buffer */
				scerrnum = 1; /* Set Run-time error */

			}

			b_reset(sc_buf); /* Assign mark of the buffer to the offset */


			for (int i = lexstart; i < lexend; i++)/* Stores lexeme in temp buffer */
				b_addc(lex_buf, b_getc(sc_buf)); /* Adds character by character */

			b_addc(lex_buf, '\0'); /* Adds end of string to attribute description. C type string */
			t = aa_table[state](lex_buf->cb_head); /* Calls exception state function by index */
			b_free(lex_buf); /* Frees buffer */

			return t;
		}

		t.code = ERR_T; /* Creates error token */
		t.attribute.err_lex[0] = c; /* Stores character as error attribute */
		t.attribute.err_lex[1] = '\0'; /* Adds end of string to attribute description. C type string */

		return t;

	}/*end while(1)*/
}

/********************************************************************************************
Purpose :			To get the state of the next character being scanned
Author :			Svillen Ranev
Version:	        1.0
Called functions :	char_class(), exit()
parameters :		int state	- the current state
char c		- the character being analyzed
int &accept	- The memory address of the acceptance state
Return value :		exit(), returned if an error detected
int, returns the next state
Algorithm :			Provided by Svillen Ranev
*******************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/********************************************************************************************
Purpose :			Returns the column number in the transition table st_table
for the input character c
Author :			Kishan Sondagar
Version:	        1.0
Called functions :	none
parameters :		char, c
Return value :		int, returns transition table column number
Algorithm :			In an if-else ladder, the char c is checked for the value
it is holding and returns the corresponding column number
*******************************************************************************************/
int char_class(char c)
{
	if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
		return 0;
	else if (c == '0')
		return 1;
	else if (c >= '1' && c <= '9')
		return 2;
	else if (c == '.')
		return 3;
	else if (c == '$')
		return 4;
	else if (c == '"')
		return 6;
	else if (c == '\0' || c == EOF)
		return 7;
	else
		return 5;

}

/********************************************************************************************
Purpose :			Accepting function for the arithmentic variable
identifier AND keywords (VID - AVID/KW)
Author :			Ilya Kukhtiy
Version:	        1.0
Called functions :	findIndexKW(), strlen(), strncpy(), strcpy()
parameters :		char, lexeme[]
Return value :		Token, t
Algorithm :			checks if the lexeme is a keyword.
if it is, then it must return a token with the corresponding
attribute for the keyword.
The attribute code for the keyword is its index in the
keyword table.
if the lexeme is a AVID token VID_LEN characters are stored into the attribute.
add the \0 terminator and return the token

*******************************************************************************************/
Token aa_func02(char lexeme[])
{
	Token t = { 0 };/*Initialize  token*/

	if (iskeyword(lexeme) == 1) {/* Checks if lexeme is one of the keywords */
		int kwIndex = findIndexKW(lexeme); /* Stores index of keyword from  kw_table[] */
		t.code = KW_T; /* Sets keyword token */
		t.attribute.kwt_idx = kwIndex; /* Stores keyword index as token attribute */
	}
	else {/* Lexeme is Arithmetic Variable Identifier */
		char vid[9]; /* Temporary AVID */
		int length = strlen(lexeme); /* Stores length of lexeme */

		if (length > VID_LEN) { /* Check if lexeme's length is out of AVID limit */
			strncpy(vid, lexeme, VID_LEN); /* Copy first character of lexeme up to the limit  */
			vid[8] = '\0'; /* Adds end of string to attribute description. C type string */
		}
		else {/* AVID length is */
			strncpy(vid, lexeme, length); /* Copy entire lexeme */
			vid[length] = '\0'; /* Adds end of string to attribute description. C type string */
		}

		t.code = AVID_T; /* Sets Arithmetic Variable Identifier token code */
		strcpy(t.attribute.vid_lex, vid); /* AVID is assigned to token attribute */
	}

	return t;

}

/********************************************************************************************
Purpose :			stores string variable identifier
Author :			Ilya Kukhtiy
Version:	        1.0
Called functions :	strlen(), strncpy(), strcpy
parameters :		char:c
Return value :		int, returns the next state
Algorithm :			Set a SVID token
If the lexeme is longer than VID_LEN characters, onlye first VID_LEN -1
characters are stored into the cariable attribute array vid_index[].
and then the $ character is appended to the name,
adds \0 to the end to make it a c-type string
*******************************************************************************************/
Token aa_func03(char lexeme[]) {

	Token t = { 0 }; /*Initialize  token*/

	char vid[9]; /* Temporary String Variable Identifier */
	int len = strlen(lexeme); /* Stores length of lexeme */

	if (len > VID_LEN) { /* Checks lexeme length against maximum variable length */
		strncpy(vid, lexeme, VID_LEN - 1); /* Stores first 7 characters of string */
		vid[7] = '$'; /* Adds $ to represent string variable indetifier */
		vid[8] = '\0'; /* Adds end of string to attribute description. C type string */
	}
	else {/* Lexeme length is under the limit */
		strncpy(vid, lexeme, len); /* Copies entire SVID */
		vid[len] = '\0';/* Adds end of string to attribute description. C type string */
	}

	t.code = SVID_T; /* Sets String Variable Identifier token */
	strcpy(t.attribute.vid_lex, vid); /* Copies SVID over to token attribute */

	return t;

}

/********************************************************************************************
Purpose :			converts the lexeme to a decimal integer value
Author :			Kishan Sondagar
Version:	        1.0
Called functions :	atoi(), strlen(), strncpy(),
parameters :		char:c
Return value :		int, returns the next state
Algorithm :			converts the lexeme to a integer using atoi() function.
checks to make sure the the number is within range of not zero,
large than epsilon and smaller than float maximum.
incase of out of range the function must return an error token.
if the error lexeme is longer than the error length characters, only
the first 3 characters are stored in err_lex, three dots,
before returning the function must set the apropriate token code.
*******************************************************************************************/
Token aa_func05(char lexeme[]) {

	Token t = { 0 }; /*Initialize  token*/
	int il = atoi(lexeme); /* Converts string into int */

	if (il > SHRT_MAX || il < SHRT_MIN) { /* Checks if floating number is out of bounds of 2-byte number. If so, enters if-statement */
		t.code = ERR_T; /* Sets error token code */
		int len = strlen(lexeme); /* Stores length of lexeme */

		if (len > ERR_LEN) { /* Stores short form of error attribute */
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3); /* Short form of lexeme */
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
			t.attribute.err_lex[ERR_LEN] = '\0';/* Adds end of string to attribute description. C type string */
		}
		else {
			strcpy(t.attribute.err_lex, lexeme); /* Copy entire string  */
			t.attribute.err_lex[len] = '\0';/* Adds end of string to attribute description. C type string */
		}
	}
	else { /* Integer number is within bounds  */
		t.code = INL_T; /* Sets integer literal token */
		t.attribute.int_value = il; /* Sets value of integer literal */
	}
	return t;

}

/********************************************************************************************
Purpose :			convert string to floating number
Author :			Ilya Kukhtiy
Version:	        1.0
Called functions :	atof(), strlen(), strncpy()
parameters :		char, lexeme[]
Return value :		Token, t
Algorithm :			converts the lexeme to a integer using atoi() function.
checks to make sure the the number is within range of not zero,
large than epsilon and smaller than float maximum.
incase of out of range the function must return an error token.
if the error lexeme is longer than the error length characters, only
the first 3 characters are stored in err_lex, three dots,
before returning the function must set the apropriate token code.
*******************************************************************************************/
Token aa_func08(char lexeme[]) {

	Token t = { 0 }; /*Initialize  token*/
	double fpl = atof(lexeme); /* Converts string into float */

	if (fpl != 0 && (fpl > FLT_MAX || fpl < FLT_EPSILON)) { /* Checks if floating number is out of bounds. If so, enters if-statement */
		t.code = ERR_T; /* Sets token to error */
		int len = strlen(lexeme); /* Gets length of lexeme */

		if (len > ERR_LEN) { /* Stores short form of error attribute */
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
			t.attribute.err_lex[ERR_LEN - 3] = '.'; /* Add triple dot to token attribute */
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
			t.attribute.err_lex[ERR_LEN] = '\0'; /* Adds end of string to attribute description. C type string */
		}
		else { /* Stores entire lexeme to token attribute */
			strcpy(t.attribute.err_lex, lexeme);
			t.attribute.err_lex[len] = '\0'; /* Adds end of string to attribute description. C type string */
		}
	}
	else { /* Floating number is within bounds  */
		t.code = FPL_T; /* Sets token code to Float code  */
		t.attribute.flt_value = (float)fpl; /* Stores floating number as attribute of the token  */
	}

	return t;

}

/********************************************************************************************
Purpose :			stores the lexeme into the string literal table (str_LTBL)
Author :			Kishan Sondagar
Version:	        1.0
Called functions :	b_limit(), b_addc(),
parameters :		char, lexeme[]
Return value :		Token, t
Algorithm :			First the attribute token gets set.
The attribute of the string token is the offset of the beginning of the
string literal character buffer to the locationwhere the first char of the
lexeme will be added.
using add_c() to copy the lexeme content.
this is done in a while loop till the end terminator is hit.
the line number is incremented and the token code get set.
*******************************************************************************************/
Token aa_func10(char lexeme[]) {

	Token t = { 0 }; /*Initialize  token*/
	t.code = STR_T; /* Sets String token*/
	t.attribute.str_offset = b_limit(str_LTBL); /*Save offset from current buffer*/

	while (lexeme != '\0') { /* Loop untill end of line */
		if (lexeme != '"') /* Store string untill it reaches second quote */
			b_addc(str_LTBL, lexeme); /*Add character*/
		lexeme++; /* Increment lexeme */
	}

	if (lexeme == '\0') { /* Add character and increment line count when end of line is reached */
		b_addc(str_LTBL, lexeme); /* Adds characters to buffer */
		line++; /* Increments line */
	}

	return t;

}

/********************************************************************************************
Purpose :			sets the error token
Author :			Ilya Kukhtiy
Version:	        1.0
Called functions :	strlen(), strncpy()
parameters :		char, lexeme[]
Return value :		Token, t
Algorithm :			creates an error token, and if the error atribute is longer than
the limit, it will store the short form of the error lexeme. Else,
the entire error lexeme gets stored.
*******************************************************************************************/
Token aa_func11(char lexeme[]) {

	Token t = { 0 }; /*Initialize  token*/
	t.code = ERR_T; /*Sets token to Error*/
	int len = strlen(lexeme); /*Get length of lexeme*/

	if (len > ERR_LEN) { /*If lexeme is longer than allowed length*/
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3); /* Copy short form of error attribute*/
		t.attribute.err_lex[ERR_LEN - 3] = '.';/* Concatinate ... to the attribute */
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0'; /* Make it C string. Adds end-of-string */
	}
	else { /* if lexeme is under limit length */
		strcpy(t.attribute.err_lex, lexeme); /* Copy entire string */
		t.attribute.err_lex[len] = '\0'; /* Make it C string. Adds end-of-string */
	}

	return t;

}

/********************************************************************************************
Purpose :			empty function, for future use
Author :
Version:			1.0
Called functions :
parameters :		char, lexeme
Return value :		Token, t
Algorithm :
*******************************************************************************************/
Token aa_func12(char *lexeme) {
	/*To be used in future*/
	Token t = { 0 };
	return t;

}

/********************************************************************************************
Purpose :			To check if a keyword is a valid keyword
Author :			Kishan Sondagar
Version:	        1.0
Called functions :	strcmp()
parameters :		char, kw_lexeme
Return value :		int, 1 - if keyword is found
int, 0 - if failed to find keyword
Algorithm :			Loop through the keyword array kw_table until the keyword is found
and returns 1, if the keyword cannot be found then returns 0
*******************************************************************************************/
int iskeyword(char * kw_lexeme) {

	for (int i = 0; i < 10; i++)
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return 1;

	return 0;

}

/********************************************************************************************
Purpose :			Purpose of the function is to find the index of the keyword kw_lexeme
Author :			Ilya Kukhtiy
Version:	        1.0
Called functions :	strcmp()
parameters :		char, kw_lexeme - the keyword lexeme
Return value :		int, i	- the index of the keyword corresponding array
int, RT_FAIL_1 (-1) - if failed to find the keyword in array
Algorithm :			Loop through the length of the kw_table array and if the kw_lexeme
matches with the element of the array and copies successfully then
return the index of the corresponding array value
*******************************************************************************************/
int findIndexKW(char * kw_lexeme) {
	for (int i = 0; i < 10; i++)
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return i;

	/*return -1 on fail*/
	return RT_FAIL_1;

}