/* compile with: gcc syncbench.c -o syncbench -lrt -Wall -Wextra -Werror
   run as:
 ./syncbench filename 1024 100 'd' # to test fdatasync()
 ./syncbench filename 1024 100 's' # to test fsync()

   to perform 100 repetitions of writing 1024-byte chunks.
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

int mono_time(double *result)
{
	struct timespec now;
	if (clock_gettime(CLOCK_MONOTONIC, &now)) {
		return -1;
	}
	*result = (double)now.tv_sec + ((double)now.tv_nsec * 1e-9);
	return 0;
}

int main(int argc, char **argv)
{
	double t0, t1;
	if (argc != 5) {
		puts("Usage: syncbench file write_size write_qty (d|s)");
		return 1;
	}
	if (strlen(argv[4]) != 1) {
		puts("You're a bad person.");
		return 1;
	}
	int d;
	if (argv[4][0] == 'd') {
		d = 1;
	} else if (argv[4][0] == 's') {
		d = 0;
	} else {
		puts("That last parameter should have been either 'd' or 's'.");
		return 1;
	}
	int write_size = atoi(argv[2]);
	int no_of_writes = atoi(argv[3]);
	if ((write_size <= 0) || (no_of_writes <= 0)) {
		puts("You want me to write no bytes?");
		return 1;
	}
	int fd = open(argv[1], O_CREAT | O_WRONLY | O_EXCL);
	if (fd == -1) {
		if (errno == EEXIST) {
			printf("Refusing to nuke existing file %s.\n", argv[1]);
		} else {
			printf("Can't open %s for write.\n", argv[1]);
		}
		return 1;
	}
	char *bytes = malloc(write_size);
	if (bytes == NULL) {
		puts("malloc() failed, FML.");
		return 1;
	}
	int i;
	for (i = 0; i < write_size; i++) {
		bytes[i] = '0' + (i % 10);
	}
	if (mono_time(&t0)) {
		puts("can't consult a clock.");
		return 1;
	}
	for (i = 0; i < no_of_writes; i++) {
		ssize_t r = write(fd, bytes, write_size);
		if (r != write_size) {
			puts("write() failed.");
			return 1;
		}
		if (d) {
			if (fdatasync(fd)) {
				puts("fdatasync() failed.");
				return 1;
			}
		} else {
			if (fsync(fd)) {
				puts("fsync() failed.");
				return 1;
			}
		}
	}
	if (mono_time(&t1)) {
		puts("really, can't consult the clock?");
		return 1;
	}
	char *c = d ? "fdatasync" : "fsync";
	printf("Performed %d %s()s in %f seconds.\n", no_of_writes, c, t1 - t0);
	double rate = no_of_writes / (t1 - t0);
	printf("That's %f calls/second, or %f seconds/call.\n", rate, 1 / rate);
	return 0;
}
