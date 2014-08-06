/* how long does it take to memset() per-byte on different sizes? */
/* cc -O3 -o memwalk memwalk.c -std=c99 -Wall -Wextra */
/* Usage: ./memwalk 10 32 */
/* First parameter is the number of kilobytes to hit. */
/* Second parameter is the base-2 logarithm of how many times to hit them. */
/* "./memwalk 1 10" will walk 1024 times over 1kB. */
/* "./memwalk 1024 20" will walk 1048576 times over 1MB. */
/* "./memwalk 2048 16" will walk 65536 times over 2MB. */

#define _POSIX_C_SOURCE 199309L
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(int argc, char **argv)
{
	struct timespec tbegin, tfinish;
	size_t thismuch;
	long long int thismany;

	if (argc != 3) {
		printf("I want two int parms.\n");
		return 1;
	}

	long me = strtol(argv[1], NULL, 10);
	if (me <= 0) {
		printf("Noooo %ld isn't right.\n", me);
		return 1;
	}
	thismuch = (size_t)me;
	thismuch <<= 10;

	thismany = strtoll(argv[2], NULL, 10);
	if (thismany <= 0) {
		printf("Noooo %lld isn't bigger than 0.\n", thismany);
		return 1;
	}

	size_t longlong_bits = 8 * sizeof(long long);
	if ((size_t)thismany >= longlong_bits - 1) {
		printf("%lld is too many, I can't count to 1 << %zd.\n",
		       thismany, longlong_bits - 1);
		return 1;
	}

	thismany = 1LL << thismany;

	char *thesebytes = malloc(thismuch);
	if (thesebytes == NULL) {
		printf("No, can't allocate that %zu bytes.\n", thismuch);
	}
	printf("allocated %zd bytes at %p.\n", thismuch, (void *)thesebytes);
	printf("will walk %lld times.\n", thismany);
	memset(thesebytes, 0, thismuch);

	clock_gettime(CLOCK_REALTIME, &tbegin);
	for (long long l = 0; l < thismany; l++) {
		memset(thesebytes, l & 0xFF, thismuch);
	}
	clock_gettime(CLOCK_REALTIME, &tfinish);

	long diff_sec = (long)tfinish.tv_sec - (long)tbegin.tv_sec;
	long diff_nsec = (long)tfinish.tv_nsec - (long)tbegin.tv_nsec;
	double nanos = 1.0e9 * (double)diff_sec + (double)diff_nsec;
	double ops = (double)thismuch * (double)thismany;
	printf("%f ns/byte\n", nanos / ops);

	free(thesebytes);
	return 0;
}
