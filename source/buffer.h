/*
File name:			buffer.h
Compiler:			MS Visual Studio 2015
Author:				Ilya Kukhtiy, 040778822
Course:				CST 8152 – Compilers. Lab Section: 014
Assignment:			1
Date:				September 10, 2018
Professor:			Svillen Ranev
Purpose:			Sets constants. Creates structure for Buffer. Declares methods.
Function list:
b_allocate();
b_addc();
b_clear();
b_free();
b_isfull();
b_limit();
b_capacity();
b_mark();
b_mode();
b_incfactor();
b_load();
b_isempty();
b_getc();
b_eob();
b_print();
b_compact();
b_rflag();
b_retract();
b_reset();
b_getcoffset();
b_rewind();
b_location();
*/

#ifndef BUFFER_H_
#define BUFFER_H_


/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make comments an warning */

							/*#pragma warning(error:4001)*//* to enforce C89 comments - to make  comments an error */

														   /* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

														   /* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail return value */

#define MIN_INIT_CAPACITY 0
#define MAX_INIT_CAPACITY (SHRT_MAX-1)

#define MAX_INC_FACTOR   255
#define MIN_INC_FACTOR   1
#define FIXED_INC_FACTOR 0

#define FIXED_MODE 0
#define ADD_MODE   1
#define MULT_MODE  -1

#define POSITIVE_FACTOR 0x100
														   /* Enter your bit-masks constant definitions here */
#define DEFAULT_FALGSZ  0x0000 
#define DEFAULT_FALGS  0xFFFC       /* 1111 1111 1111 1100 */ /*default flags value */
#define SET_EOB        0x0001       /* 0000 0000 0000 0001 */ /*set eob mask*/
#define RESET_EOB      0xFFFE       /* 1111 1111 1111 1110 */ /*reset eob mask*/
#define CHECK_EOB      0x0001       /* 0000 0000 0000 0001 */ /*check eob mask*/
#define SET_R_FLAG     0x0002       /* 0000 0000 0000 0010 */ /*set r_flag mask*/
#define RESET_R_FLAG   0xFFFD       /* 1111 1111 1111 1101 */ /*reset r_flag mask*/
#define CHECK_R_FLAG   0x0002       /* 0000 0000 0000 0010 */ /*check r_flag mask*/

														   /* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_eob(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif

