/* This is a terrible program that uses a tiny part of openssl's API
   that I wrote just for the sake of testing that a working (linkable, etc) openssl 
   happens to be installed.

   To compile:
   gcc ssl_example.c -lssl -lcrypto -o ssl_example

   To use: don't.
   ./ssl_example foo bar baz whatever you like here

   The program uses any command line arguments passed to it to seed an RC4 key
   then uses RC4 as a pseudo-random number generator to spit out some bytes
   on standard output.
*/
#include <openssl/rc4.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
	RC4_KEY key;
	char data[48];
	size_t total_len = 0;
	size_t i;
	for (i=1; i<argc; i++) {
		total_len += strlen(argv[i]);
	}
	char *d = malloc(total_len);
	if (d == NULL) abort();
	char *p = d;
	for (i=1; i<argc; i++) {
		memcpy(p, argv[i], strlen(argv[i]));
		p += strlen(argv[i]);
	}
	printf("phrase = %s\n", d);
	RC4_set_key(&key, total_len, d);
	char plain[32];
	char ciphr[32];
	memset(plain, 0, 32);
	RC4(&key, 32, plain, ciphr);
	for (i=0; i<32; i++) {
		printf("%hhx", ciphr[i]);
	}
	printf("\n");
	return 0;
}
