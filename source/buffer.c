/*
File name:			buffer.c
Compiler:			MS Visual Studio 2015
Author:				Ilya Kukhtiy, 040778822
Course:				CST 8152 – Compilers. Lab Section: 014
Assignment:			1
Date:				September 10, 2018
Professor:			Svillen Ranev
Purpose:			Create buffer in one of the three modes and allocate it.
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

#include "buffer.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>


Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {

	/*Validates parameters*/
	if (init_capacity > MAX_INIT_CAPACITY || init_capacity < MIN_INIT_CAPACITY) {
		return NULL;
	}


	Buffer * buffer = calloc(1, sizeof(Buffer));

	if (!buffer) {
		return NULL;
	}

	buffer->cb_head = malloc((sizeof(char)) * init_capacity);

	if (!buffer->cb_head) {
		return NULL;
	}


	if ((unsigned char)inc_factor < FIXED_INC_FACTOR || (unsigned char)inc_factor > MAX_INC_FACTOR) {
		return NULL;
	}


	/*Fixed mode buffer*/
	if (o_mode == 'f' || inc_factor == 0) {

		if ((unsigned short)init_capacity == 0)
			return NULL;

		buffer->mode = FIXED_MODE;
		buffer->inc_factor = FIXED_INC_FACTOR;

	}
	/*Additive mode buffer*/
	else if (o_mode == 'a') {

		buffer->mode = ADD_MODE;
		buffer->inc_factor = (unsigned char)inc_factor;

	}
	/*Multiplicative mode buffer*/
	else if (o_mode == 'm') {

		if (inc_factor > 100 || inc_factor < 1) {
			return NULL;
		}

		buffer->mode = MULT_MODE;
		buffer->inc_factor = inc_factor;

	}
	else {

		buffer->mode = FIXED_MODE;
		buffer->inc_factor = FIXED_INC_FACTOR;

	}


	buffer->capacity = init_capacity;

	/* Sets default flags*/
	buffer->flags = buffer->flags & DEFAULT_FALGSZ;
	buffer->flags = buffer->flags | DEFAULT_FALGS;


	return buffer;
}

pBuffer b_addc(pBuffer const pBD, char symbol) {

	if (!pBD) {
		return NULL;
	}

	short avail_space = 0;
	short new_inc = 0;
	short new_capacity = 0;
	char * temp;

	/*
	if ((pBD->flags & 0x0002) == 1) {
	pBD->flags = pBD->flags & RESET_R_FLAG;
	}
	*/

	/*Adds character to buffer and increments index*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;

	}
	else {

		if (pBD->mode == FIXED_MODE) {

			return NULL;

		}

		if (pBD->mode == ADD_MODE) {

			new_capacity = pBD->capacity + ((unsigned char)pBD->inc_factor);
			/*Validates new capacity*/
			if (!(new_capacity > 0) && !(new_capacity <= MAX_INIT_CAPACITY)) {
				return NULL;
			}

			if (pBD->capacity > 0 && pBD->capacity > MAX_INIT_CAPACITY) {
				new_capacity = MAX_INIT_CAPACITY;
			}

		}

		if (pBD->mode == MULT_MODE) {

			if (pBD->capacity == MAX_INIT_CAPACITY) {
				return NULL;
			}

			/*Increases current capacity*/
			avail_space = MAX_INIT_CAPACITY - pBD->capacity;
			new_inc = avail_space *  pBD->inc_factor / 100;
			new_capacity = (pBD->capacity + new_inc);

			/* Incremented unseccessfully */
			if (pBD->capacity == new_capacity) {

				new_capacity = MAX_INIT_CAPACITY;

			}


		}

		temp = (char *)realloc(pBD->cb_head, new_capacity);

		/* Checks if memory is allocated */

		if (!temp) {
			return NULL;
		}

		/*Buffer pointer changed. Sets relocation flag*/

		if (pBD->cb_head != temp) {

			pBD->flags | SET_R_FLAG;
			pBD->cb_head = temp;

		}


		pBD->cb_head[pBD->addc_offset++] = symbol;
		pBD->capacity = new_capacity;

		return pBD;

	}

	/*	if (pBD->capacity > MAX_INIT_CAPACITY) {
	pBD->capacity = MAX_INIT_CAPACITY;
	}
	*/
	return pBD;
}

int b_clear(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}
	/*Clears buffer descriptors so it can be read again*/
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = pBD->flags & RESET_R_FLAG;
	pBD->flags = pBD->flags & RESET_EOB;

	return 0;
}

void b_free(Buffer * const pBD) {

	if (!pBD)
		return;

	free(pBD->cb_head);
	free(pBD);
	return;

}

int b_isfull(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	if (pBD->addc_offset == pBD->capacity) {
		return 1;
	}

	return 0;
}

short b_limit(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->addc_offset;
}

short b_capacity(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->capacity;
}

short b_mark(pBuffer const pBD, short mark) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	if (mark < 0 || mark > pBD->addc_offset) {
		return RT_FAIL_1;
	}

	return pBD->markc_offset = mark;
}

int b_mode(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_2;
	}

	return pBD->mode;
}

size_t b_incfactor(Buffer * const pBD) {
	if (!pBD) {
		return POSITIVE_FACTOR;
	}
	return (unsigned char)pBD->inc_factor;
}

int b_load(FILE * const fi, Buffer * const pBD) {

	char symbol;
	int count = 0;

	if (!pBD || !fi) {
		return RT_FAIL_1;
	}

	while (1) {

		symbol = (char)fgetc(fi);

		if (feof(fi)) {
			break;
		}

		if (!b_addc(pBD, symbol)) {
			ungetc(symbol, fi);
			printf("The last character read from the file is: %c %d \n", symbol, count);
			return LOAD_FAIL;
		}

		count++;
	}

	return count;

}

int b_isempty(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	if (pBD->addc_offset == 0) {
		return 1;
	}

	return 0;
}


char b_getc(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_2;
	}
	/*Reads till the end of buffer and sets flag*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags = pBD->flags | SET_EOB;

		return 0;
	}
	else {
		pBD->flags = pBD->flags & RESET_EOB;
	}

	return pBD->cb_head[pBD->getc_offset++];
}

int b_eob(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->flags & CHECK_EOB;
}

int b_print(Buffer * const pBD) {

	int count = 0;
	char c;

	if (!pBD) {
		return RT_FAIL_1;
	}

	if (pBD->addc_offset == 0) {
		printf("Empty buffer\n");
		return count;
	}

	while (1) {
		c = (char)b_getc(pBD);

		if (b_eob(pBD) == 1) {
			break;
		}

		printf("%c", c);
		count++;
	}

	printf("\n");

	return count;
}

Buffer * b_compact(Buffer * const pBD, char symbol) {

	short new_capacity;
	char * new_char_buffer;

	if (!pBD) {
		return NULL;
	}

	pBD->flags = pBD->flags & RESET_R_FLAG;

	new_capacity = (((unsigned short)pBD->addc_offset + 1) * sizeof(char));

	if ((unsigned short)new_capacity < 0) {
		return NULL;
	}

	new_char_buffer = (char *)realloc(pBD->cb_head, (unsigned short)new_capacity);

	if (!new_char_buffer) {
		return NULL;
	}

	if (pBD->cb_head != new_char_buffer) {

		pBD->flags = pBD->flags | SET_R_FLAG;
		pBD->cb_head = new_char_buffer;

	}


	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = new_capacity;


	return pBD;
}

char b_rflag(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}
	return pBD->flags & CHECK_R_FLAG;

}

short b_retract(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	if (pBD->getc_offset == 0) {
		return RT_FAIL_1;
	}

	return --pBD->getc_offset;
}

short b_reset(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset = pBD->markc_offset;
}

short b_getcoffset(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

int b_rewind(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	return 0;
}


char * b_location(Buffer * const pBD, short loc_offset) {

	if (!pBD) {
		return NULL;
	}

	if (loc_offset >= pBD->addc_offset || loc_offset < 0) {
		return NULL;
	}

	return pBD->cb_head + loc_offset;
}
