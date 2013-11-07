/* world's crummiest fsync() benchmark */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

double mono_time() {
 struct timespec now;
 clock_gettime(CLOCK_MONOTONIC,&now);
 if (errno) { abort(); }
 return (double)now.tv_sec +
        ((double)now.tv_nsec * 1e-9);
}
#define TRIES 100
int main(int argc, char **argv) {
 double t0, t1, ns;
 int i;
 t0 = mono_time();
 for (i=0; i<(TRIES); i++) {
  write(1, &i, sizeof(i));
  fsync(1);
 }
 t1 = mono_time();
 ns = (t1 - t0) / (TRIES);
 fprintf(stderr, "%ens per fsync();", ns);
 return 0;
}
