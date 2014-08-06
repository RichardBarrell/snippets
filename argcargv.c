/* Pretty-prints argc and argv.
 * gcc -o argcargv argcargv.c
 * I wrote this because I can NEVER remember how they go. :)
 * It's almost useful for debugging incorrect word splitting in shell scripts.
 */

#include <stdio.h>

int main(int argc, char **argv)
{
	int i;
	printf("argc=%d\n", argc);
	for (i = 0; i < argc; i++) {
		printf("argv[%d] = %s\n", i, argv[i]);
	}
	return 0;
}
