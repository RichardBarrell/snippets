/* How much can I actually malloc? One argument, number of megabytes.
 * gcc mal.c -o mal
 * ./mal 512
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
	long howmuch = 1 << 20;
	if (argc >= 2) {
		howmuch = atol(argv[1]);
		if (howmuch<1) {
			fprintf(stderr, "At least one byte please.\n");
			return 1;
		}
	}
	unsigned char *tibet = malloc(howmuch);
	if (tibet == NULL) {
		fprintf(stderr, "Oh no that didn't malloc.\n");
	}
	long i;
	for (i=0;i<howmuch;++i) {
		tibet[i] = howmuch & 0xFF;
	}
	free(tibet);
	printf("That worked.\n");
	return 0;
}
