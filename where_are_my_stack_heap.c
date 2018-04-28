#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
	(void)argc;
	(void)argv;

	void *stack = sbrk(0);
	int *small = calloc(sizeof(int), 1);
	int i = 1234;
	*small = i;
	int *big = malloc(1<<28);

	printf("my stack is around:               %016p\n", stack);
	printf("the address of a local variable:  %016p\n", &i);
	printf("address of a small allocation:    %016p\n", small);
	printf("address of a big allocation:      %016p\n", big);

	free(small); small=NULL;
	free(big); big=NULL;
	return 0;
}
