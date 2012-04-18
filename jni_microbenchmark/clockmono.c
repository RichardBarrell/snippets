#include <dlfcn.h>
#include <stdio.h>

int main(int argc, char **argv) {
	void *cm = dlopen("./libcclockmono.so", RTLD_LAZY);
	if (cm == NULL) { printf("damn, %s\n", dlerror()); return 1; }
	double (*clock_mono)(void) = dlsym(cm, "clock_mono");
	double t0 = clock_mono();
	int i;
	for (i=0; i<5000000; i++) {
		clock_mono();
	}
	double t1 = clock_mono();
	printf("c/dlsym(): %f\n", t1 - t0);
	return 0;
}
