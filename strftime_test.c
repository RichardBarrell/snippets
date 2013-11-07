#include <time.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	size_t buffersz = 4096;
	char* buffer = malloc(buffersz);
	if (buffer == NULL) {
		perror("malloc hates me: ");
		return 1;
	}
	time_t now_seconds = time(NULL);
	struct tm *now = localtime(&now_seconds);
	int i;
	for (i=1; i<argc; i++) {
		size_t outputsz;
		char *fmt = argv[i];
		outputsz = strftime(buffer, buffersz, fmt, now);
		if (outputsz != 0) {
			puts(buffer);
		}
	}
	return 0;
}

