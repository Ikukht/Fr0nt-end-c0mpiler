/*****************************************************************************************************
*	File name : table.h
*	Compiler : MS Visual Studio 2013
* 	Author : Ilya Kukhtiy 040778822 , Kishan Sondagar 040845747
* 	Course : CST 8152 – Compilers, Lab Section : 12 (Kishan), 14 (Ilya)
* 	Assignment : 2
* 	Date : November 8, 2018
* 	Professor : Svillen Ranev
* 	Purpose : Provided by Svillen Ranev, The purpose of this file is to provide the Transition
*				Table and function declarations that are neccesary for the scanners implementation
*				as required for assignment 2.
* 	Function list :	aa_func02(),
aa_func03(),
aa_func05(),
aa_func08(),
aa_func10(),
aa_func11(),
aa_func12()
*****************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or one of 255,0xFF,EOF
*
*  Special case tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
*  white space
*  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'illegal symbol',
*/

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1  /* Inavalid state */

/* State transition table definition */

#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */

int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1 ,6 ,4 ,ES,ES,ES,9 ,ES },
	/* State 1 */{ 1 ,1 ,1 ,2 ,3 ,2 ,2 ,ES },
	/* State 2 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 3 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 4 */{ ES,4 ,4 ,7 ,5 ,5 ,5 ,ES },
	/* State 5 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 6 */{ ES,6 ,ES,7 ,5 ,5 ,5 ,ES },
	/* State 7 */{ 8 ,7 ,7 ,8 ,8 ,8 ,8 ,ES },
	/* State 8 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 9 */{ 9 ,9 ,9 ,9 ,9 ,9 ,10,ES },
	/* State 10 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 11 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 12 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 13 */{ IS,IS,IS,IS,IS,IS,IS,IS }
};

/* Accepting state table definition */

#define ASWR      5  /* accepting state with retract */
#define ASNR      4  /* accepting state with no retract */
#define NOAS     -5  /* not accepting state */

int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASWR, ASNR, ASWR };

/* Accepting action function declarations */

Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) returning Token */

typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */

PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11,
	aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
