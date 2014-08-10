#include <stdio.h>

int main(int argc, char **argv)
{
	printf("La la la...\nCrash time now.\n");
	((int*)0)[0] = 1;
	printf("Surprisingly, still alive.\n");
	return 0;
}
