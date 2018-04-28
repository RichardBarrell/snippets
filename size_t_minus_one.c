#include <stdint.h>
#include <stdio.h>
#include <limits.h>

typedef struct rtunion {
	char whatever[8];
} rtunion;

int main (int argc, char **argv) {
	(void)argc;
	(void)argv;
	int z = 0;
	int n = -1;
	size_t sz = n;
	printf("sizeof(n) = %zu\n", sizeof(n));
	printf("n = %d\n", n);
	printf("(size_t)n = %zu\n", (size_t)n);
	printf("sizeof(sz) = %zu\n", sizeof(sz));
	printf("sz = %zu\n", sz);
	printf("n * sizeof(rtunion) = %zu\n", n * sizeof(rtunion));
	printf("(z - 1) * sizeof(rtunion) = %zu\n", (z - 1) * sizeof(rtunion));
	printf("(size_t)(uint32_t)n = %zu\n", (size_t)(uint32_t)n);
	return 0;
}
