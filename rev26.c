/* To compile, run one of: */
/* gcc -o rev26 -O3 rev26.c -lpthread -Wall -DUSE_A_POSIX_BARRIER */
/* or: */
/* gcc -o rev26 -O3 rev26.c -lpthread -Wall -DUSE_A_SPIN_BARRIER */
/* or maybe even: */
/* gcc -o rev26 -O3 rev26.c -lpthread -Wall -DUSE_A_SPIN_BARRIER -DGO_FASTER_DAMMIT */

/* To run, run as: */
/* ./rev26 */

/* Wait for a bit; it'll try to count violations of sequential consistency; */
/* if Paul Loewenstein is right then it will print nonzero numbers. ☺. */
/* With posix barriers, I get ~one violation every few million iterations. */
/* With spin barriers, anything up to 90% of iterations show violations. */

/* This program is an experiment for x86 processors that shows off the */
/* 2nd example discrepancy on page 3 of the x86-TSO paper, found here: */
/* http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.153.6657 */
/* This is called "rev26" because it shows a flaw in rev 26 of Intel's SDM. */

#ifndef USE_A_POSIX_BARRIER
#ifndef USE_A_SPIN_BARRIER
#error Please give either -DUSE_A_POSIX_BARRIER or -DUSE_A_SPIN_BARRIER
#endif
#endif
#ifdef USE_A_POSIX_BARRIER
#ifdef USE_A_SPIN_BARRIER
#error Please do not give both -DUSE_A_POSIX_BARRIER and -DUSE_A_SPIN_BARRIER
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

volatile int xv, yv;
volatile int *x = &xv, *y = &yv;

#ifdef USE_A_POSIX_BARRIER
pthread_barrier_t barr;
#else /* USE_A_SPIN_BARRIER  */
int pretend_i_am_a_barrier;
#endif

/* If I take the __attribute__((noinline)) away, this program runs faster */
/* and the violation rate goes up to >90%. However, the assembly code that */
/* comes out of gcc with -O3 becomes twisty and I find it too hard to read */
/* to verify that what the CPUs are being asked to do actually matches my */
/* expectations. */
#ifndef GO_FASTER_DAMMIT
__attribute__((noinline))
#endif
void paws() {
    #ifdef USE_A_POSIX_BARRIER
    pthread_barrier_wait(&barr);
    #else /* USE_A_SPIN_BARRIER  */
    int *vi = &pretend_i_am_a_barrier;
    if (__sync_bool_compare_and_swap(vi, 0, 1)) {
        while (!__sync_bool_compare_and_swap(vi, 2, 0)) {
            ;
        }
    } else {
        while (!__sync_bool_compare_and_swap(vi, 1, 2)) {
            ;
        }
    }
    #endif
}

#define NO_OF_ITERATIONS (100*1000*1000)

static void *links(void *context) {
    int i;
	int a, b;
    int nots = 0;
    for (i=0; i<NO_OF_ITERATIONS; i++) {
		*x = 0;
		*y = 0;
        paws();
        *x = 1;
        a = *x;
        b = *y;
        paws();
		if ((a == 1) && (b == 0) && (*x == 1)) {
            nots++;
		}
        if ((i>0) && (i%(1000000)==0)) {
            double pc = (double)nots * 1.0e-4;
            printf("Saw %d violations per million. (%.4f%%)\n", nots, pc);
            fflush(stdout);
            nots = 0;
        }
    }
    return NULL;
}

static void *recht(void *context) {
    int i;
    for (i=0; i<NO_OF_ITERATIONS; i++) {
        paws();
        *y = 2;
        *x = 2;
        paws();
    }
    return NULL;
}

int main(int arc, char **argv) {
    int i;
    int n = 1;
    #ifdef USE_A_POSIX_BARRIER
    if (pthread_barrier_init(&barr, NULL, 2)) { abort(); }
    #else /* USE_A_SPIN_BARRIER  */
    pretend_i_am_a_barrier = 0;
    #endif
    pthread_t pair[2];
    for (i=0; i<n; i++) {
        if (pthread_create(pair+0, NULL, links, NULL)) { abort(); }
        if (pthread_create(pair+1, NULL, recht, NULL)) { abort(); }
    }
    for (i=0; i<n; i++) {
        if (pthread_join(pair[0], NULL)) { abort(); }
        if (pthread_join(pair[1], NULL)) { abort(); }
    }
    #ifdef USE_A_POSIX_BARRIER
    pthread_barrier_destroy(&barr);
    #else /* USE_A_SPIN_BARRIER  */
    #endif
    return 0;
}
