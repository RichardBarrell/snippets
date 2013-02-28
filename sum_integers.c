#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <sys/mman.h>

double now() {
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	return (double)tp.tv_sec + (double)tp.tv_nsec*1e-9;
}

int main(int argc, char **argv) {
	size_t n = 9*1000*1000;
	size_t to_alloc, i;
	if (argc == 2) {
		int parsed = sscanf(argv[1], "%zd", &n);
		if (parsed != 1) {
			printf("Failed to read %s as an int.\n", argv[1]);
			return 1;
		}
	}
	printf("will sum %zd ints\n", n);
	to_alloc = sizeof(uint32_t) * n;
	if (to_alloc / sizeof(uint32_t) != n) {
		printf("mul overflow\n");
		return 1;
	}
	if (to_alloc % (512*4096)) {
		to_alloc += (512*4096) - (to_alloc % (512*4096));
	}
	printf("to_alloc %zd\n", to_alloc);
	uint32_t *block = mmap(NULL, to_alloc, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_HUGETLB, 0, 0);
	if (block == MAP_FAILED) {
		printf("alloc fail\n");
		return 1;
	}
	for (i = 0; i < n; i++) {
		block[i] = i & 0xFF;
	}
	uint32_t sum = 0;
	double t0 = now();
	for (i = 0; i < n; i++) {
		sum += block[i];
	}
	double t1 = now();
	printf("took %f milliseconds to get answer %lu\n", 1000 * (t1 - t0), (unsigned long)sum);
	return 0;
}
