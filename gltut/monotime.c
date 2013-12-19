#include <time.h>

double float_mono_time() {
	struct timespec tp;
	if (clock_gettime(CLOCK_MONOTONIC, &tp)) {
		return (0.0 / 0.0);
	}
	return (double)tp.tv_sec + (double)tp.tv_nsec*1e-9;
}
