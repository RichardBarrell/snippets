/* When does SELinux's execmem protection kick in, anyway? */

#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

int main()
{
	void *m;

	m = mmap(NULL, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	printf("anon page, r-- = %p\n", m);
	if (errno) {
		perror("> ");
	}
	mprotect(m, 4096, PROT_READ | PROT_WRITE);
	if (errno) {
		perror("adding write> ");
	}
	mprotect(m, 4096, PROT_READ);
	if (errno) {
		perror("removing write> ");
	}
	mprotect(m, 4096, PROT_READ | PROT_WRITE | PROT_EXEC);
	if (errno) {
		perror("adding write+exec> ");
	}
	munmap(m, 4096);
	if (errno) {
		perror("munmap> ");
	}

	m = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
	         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	printf("anon page, rw- = %p\n", m);
	if (errno) {
		perror("> ");
	}
	mprotect(m, 4096, PROT_READ | PROT_WRITE | PROT_EXEC);
	if (errno) {
		perror("adding exec> ");
	}
	munmap(m, 4096);
	if (errno) {
		perror("munmap> ");
	}

	m = mmap(NULL, 4096, PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS,
	         -1, 0);
	printf("anon page, r-x = %p\n", m);
	if (errno) {
		perror("> ");
	}
	mprotect(m, 4096, PROT_READ | PROT_WRITE | PROT_EXEC);
	if (errno) {
		perror("adding write> ");
	}
	munmap(m, 4096);
	if (errno) {
		perror("munmap> ");
	}

	m = mmap(NULL, 4096, PROT_READ | PROT_WRITE | PROT_EXEC,
	         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	printf("anon page, rwx = %p\n", m);
	if (errno) {
		perror("> ");
	}
	munmap(m, 4096);
	if (errno) {
		perror("munmap> ");
	}

	return 0;
}
