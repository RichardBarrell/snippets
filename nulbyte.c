#include <stdio.h>

/* For some reason, ages ago, I found myself wanting a tiny program that would
   print a single nul byte on stdout in the middle of a really ugly shell
   script.

   gcc -o nulbyte -Os nulbyte.c
   strip nulbyte

   # to test :P
   ./nulbyte | od -t x1
*/

int main(int argc, const char **argv)
{
	putchar(0);
	return 0;
}
