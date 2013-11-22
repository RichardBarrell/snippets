/* cc -o prctl_test prctl_test.c */
/* ./prctl_test */
/* only works on linux, playing with the prctl() call */
/* because I wonder what the defaults are */

#include <stdio.h>
#include <sys/prctl.h>
#include <errno.h>


int main(int argc, char **argv) {
	char my_name[17];
	int pdeathsig;
	int r;

	r = prctl(PR_GET_PDEATHSIG, (unsigned long)&pdeathsig, 0, 0, 0);
	if (r != 0) { perror("Can't get PDEATHSIG: "); }
	else { printf("PDEATHSIG = %d.\n", pdeathsig); }

	my_name[16] = 0;
	r = prctl(PR_GET_NAME, (unsigned long)&my_name[0], 0, 0, 0);
	if (r != 0) { perror("Can't get NAME: "); }
	else { printf("NAME = %s.\n", my_name); }

	return 0;
}
