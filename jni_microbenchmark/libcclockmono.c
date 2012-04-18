#include <time.h>

double clock_mono() {
	struct timespec ts;
	if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
		// TODO EXCEPTION
		return -1;
	}
	double rv = ts.tv_sec + (ts.tv_nsec * 1e-9);
	return rv;
}
