/* gcc -o rev26 -O3 rev26.c -lpthread -Wall */
/* Usage: ./rev26 */
/* Wait for a bit; if Paul Loewenstein is right then it'll print a message */
/* after a few minutes ☺. It manifests once every few million iterations. */
/* This program is an experiment for x86 processors that shows off the */
/* 2nd example discrepancy on page 3 of the x86-TSO paper, found here: */
/* http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.153.6657 */
/* This is called "rev26" because it shows a flaw in rev 26 of Intel's SDM. */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

volatile int xv, yv;
volatile int *x = &xv, *y = &yv;

pthread_barrier_t barr;

static void *links(void *context) {
    int i;
	int a, b;
    for (i=0; i<(1024*1024*1024); i++) {
        pthread_barrier_wait(&barr);
		*x = 0;
		*y = 0;
        pthread_barrier_wait(&barr);
        *x = 1;
        a = *x;
        b = *y;
        pthread_barrier_wait(&barr);
		if ((a == 1) && (b == 0) && (*x == 1)) {
			printf("My CPUs aren't sequentially consistent!\n");
		}

    }
    return NULL;
}

static void *recht(void *context) {
    int i;
    for (i=0; i<(1024*1024*1024); i++) {
        pthread_barrier_wait(&barr);
        pthread_barrier_wait(&barr);
        *y = 2;
        *x = 2;
        pthread_barrier_wait(&barr);
    }
    return NULL;
}

int main(int arc, char **argv) {
    int i;
    int n = 1;
    if (pthread_barrier_init(&barr, NULL, 2)) { abort(); }
    pthread_t *pairs = calloc(n*2, sizeof(pthread_t));
    for (i=0; i<n; i++) {
        pthread_t *pair = pairs + (i*2);
        if (pthread_create(pair+0, NULL, links, NULL)) { abort(); }
        if (pthread_create(pair+1, NULL, recht, NULL)) { abort(); }
    }
    for (i=0; i<n; i++) {
        pthread_t *pair = pairs + (i*2);
        if (pthread_join(pair[0], NULL)) { abort(); }
        if (pthread_join(pair[1], NULL)) { abort(); }
    }
    pthread_barrier_destroy(&barr);
    free(pairs);
    return 0;
}
