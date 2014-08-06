/* Does data written to mmap()'d memory survive SIGABRT? */
/* cc -o mmap_kill mmap_kill.c -Wall -Wextra -std=c99 */
/* ./mmap_kill */

#define _POSIX_C_SOURCE 200809L
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static const char message[] = "Hello, spooky world.\n\0";

static int be_child(void)
{
	int fd = open("./mmap_output", O_CREAT | O_TRUNC | O_RDWR, 0666);
	if (fd < 0) {
		abort();
	}
	ftruncate(fd, 1 << 10);
	char *out =
	    mmap(NULL, 1 << 10, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	memset(out, '\r', 1 << 10);
	sprintf(out, "%s", message);
	abort();
	return -1;
}

static int be_parent(pid_t child)
{
	char content[1 << 10];
	pid_t awaited = wait(NULL);
	if (awaited != child) {
		printf("wait() didn't return my child's PID\n");
		return 1;
	}
	int fd = open("./mmap_output", O_RDONLY, 0666);
	if (fd < 0) {
		printf("couldn't open fd\n");
		return 1;
	}
	if (read(fd, content, 1 << 10) != (1 << 10)) {
		printf("expecting child to leave me %d bytes.\n", 1 << 10);
		return 1;
	}

	if (memcmp(content, message, sizeof(message)) != 0) {
		printf("expecting:\n %s\ngot:\n %s\n", content, message);
		return 1;
	}
	printf("Child managed to write the correct message.\n");
	return 0;
}

int main()
{
	pid_t child = fork();
	if (child == 0) {
		return be_child();
	} else {
		return be_parent(child);
	}
}
