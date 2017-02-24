#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Compile:
   clang implicit_cast_sadness.c -o implicit_cast_sadness \
                                 -pedantic -fsanitize=undefined \
                                 -Wall -Wextra -std=c99

 * Usage:
   ./implicit_cast_sadness 1 2 3 45000 48000 65535

 * Purpose:
   Demonstrates one reason why I don't like implicit numerical conversions.
 */

int main(int argc, char **argv)
{
	for (int i = 1; i < argc; i++) {
		uint16_t x = atoi(argv[i]);
		printf("x is %hu (mod 65536)\n", x);
		uint16_t x_squared = x * x;
		printf("x*x is %hu (mod 65536)\n\n", x_squared);
	}
	return 0;
}
