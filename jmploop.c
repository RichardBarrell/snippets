#include <setjmp.h>
#include <stdio.h>

int global_j;

int main(int argc, char **argv)
{
	(void)argc;
	(void)argv;
	jmp_buf env;
	int i;
	global_j = 0;
	printf("Hi. Top.\n");
	switch (_setjmp(env)) {
	case 0:
		printf("Hi from 0.\n");
	case 1:
		printf("Enter a number?\n");
		scanf("%d", &i);
		_longjmp(env, i);
	case 2:
		printf("Welcome to 2!\n");
		break;
	case 3:
		if (global_j < 40) {
			global_j++;
			_longjmp(env, 3);
		}
		break;
	default:
		printf("This is default.\n");
		break;
	}
	return 0;
}
