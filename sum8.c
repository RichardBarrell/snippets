/**
 * cc -O2 sum8.c -o sum8
 * time ./sum8 simple inputFile [iterations]
 * time ./sum8 punned inputFile [iterations]
 */

#define _POSIX_C_SOURCE 199309L
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

static uint8_t addBytesSimple(uint8_t *bytes, size_t length)
{
	uint8_t total = 0;
	size_t ix;
	for (ix = 0; ix < length; ix++) {
		total += bytes[ix];
	}
	return total;
}

static uint8_t addBytesPunned(uint8_t *bytes, size_t length)
{
	uint8_t total = 0;
	size_t ix;
	uint64_t total8 = 0;
	for (ix = 0; ix < length; ix += 8) {
		uint64_t temp;
		memcpy(&temp, bytes + ix, 8);
		total8 += (temp & 0xff00ff00ff00ff00ULL) >> 8;
		total8 += temp & 0x00ff00ff00ff00ffULL;
		total8 &= 0x00ff00ff00ff00ffULL;
	}
	for (int i = 0; i < 8; i++) {
		uint8_t ii;
		memcpy(&ii, i + ((char*)&total8), 1);
		total += ii;
	}
	for (; ix < length; ix++) {
		total += bytes[ix];
	}
	return total;
}

int print_usage()
{
	fprintf(
	  stderr,
	  "Usage: sum8 simple inputFile [iterations]\n"
	  "   or: sum8 punned inputFile [iterations]\n"
	);
	return 1;
}

typedef enum {
	noImpl,
	simpleImpl,
	punnedImpl,
} impl;

int main(int argc, char **argv)
{
	if (argc < 3 || argc > 4) {
		return print_usage();
	}
	long iterations = 1;	
	if (argc == 4) {
		char *end = NULL;
		iterations = strtol(argv[3], &end, 10);
		if (iterations < 1 || *end != '\0') {
			return print_usage();
		}
	}
	impl useThisOne = noImpl;
	if (strcmp(argv[1], "simple") == 0) {
		useThisOne = simpleImpl;
	} else if (strcmp(argv[1], "punned") == 0) {
		useThisOne = punnedImpl;
	}
	if (useThisOne == noImpl) {
		return print_usage();
	}
	int ret = 1;
	size_t length = 0;
	size_t bytesSz = 8; 
	uint8_t *bytes = malloc(bytesSz);
	if (!bytes) {
		perror(">");
		goto cleanup;
	}
	FILE *inputFile = NULL;
	inputFile = fopen(argv[2], "rb");
	if (inputFile == NULL) {
		perror(">");
		goto cleanup;
	}
	for (;;) {
		size_t bytesRead = fread(
		  bytes + length,
		  1,
		  bytesSz - length,
		  inputFile);
		if (ferror(inputFile)) {
			perror(">");
			goto cleanup;
		}
		if (bytesRead == 0) {
			break;
		}
		length += bytesRead;
		if (length == bytesSz) {
			size_t newBytesSz = bytesSz << 1;
			if (newBytesSz == 0) {
				fprintf(stderr, "Aaaa overflow! Aaaa!\n");
				goto cleanup;
			}
			uint8_t *newBytes = realloc(bytes, newBytesSz);
			if (!newBytes) {
				perror(">");
				goto cleanup;
			}
			bytes = newBytes;
			bytesSz = newBytesSz;
		}
	}
	if (fclose(inputFile)) {
		perror("???");
		goto cleanup;
	}
	inputFile = NULL;
	struct timespec t_start, t_end;
	if (clock_gettime(CLOCK_MONOTONIC, &t_start)) {
		perror(">");
		goto cleanup;
	}
	uint8_t total;
	if (useThisOne == simpleImpl) {
		for (long iter=0; iter<iterations; iter++) {
			total = addBytesSimple(bytes, length);
		}
	} else if (useThisOne == punnedImpl) {
		for (long iter=0; iter<iterations; iter++) {
			total = addBytesPunned(bytes, length);
		}
	}
	if (clock_gettime(CLOCK_MONOTONIC, &t_end)) {
		perror(">");
		goto cleanup;
	}
	int64_t s = t_end.tv_sec - t_start.tv_sec;
	int64_t ns = t_end.tv_nsec - t_start.tv_nsec;
	if (ns < 0) {
		ns += 1000000000LL;
		s -= 1;
	}
	double micros = ((double)s + (double)ns / 1.0e3) / (double)iterations;
	if (0 < printf("Got answer %d in %f us per iter\n", (int)total, micros)) {
		ret = 0;
	}
cleanup:
	if (bytes) {
		free(bytes);
		bytes = NULL;
	}
	if (inputFile) {
		fclose(inputFile);
		inputFile = NULL;
	}
	return ret;
}
