/* compile: gcc -fopenmp -Wall -std=c99 openmp_frequency.c -lrt -lm -O3 -o openmp_frequency */
/* usage ./openmp_frequency [y|n|p] [no of tests] [work factor per test] */
/* performs busywork many times, prints how long iterations take */
/* with y, uses OpenMP */
/* with p, uses pthreads + pthread_barrier_wait() */
/* with n, runs a single thread */

/* Typical test session looks like: */
/* gcc -fopenmp -Wall -std=c99 openmp_frequency.c -lrt -lm -O3 -o openmp_frequency */
/* ./openmp_frequency n 1000000 100 */
/* ./openmp_frequency y 1000000 100 */
/* ./openmp_frequency p 1000000 100 */

/* Busywork done here isn't enough to stress CPU, so OpenMP is expected */
/* to be slower than non-OpenMP. What I'm trying to measure here is how */
/* much overhead it actually has per parallel-for start-and-stop. */

/* Amusingly, OpenMP with gcc 4.6.3 on my machine seems to go faster than */
/* the manual pthread_barrier_wait() version. I should go find out why. */

#define THREADS 8

#define _GNU_SOURCE
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <pthread.h>

double clock_mono_us() {
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return 1.0e6 * (ts.tv_sec + (ts.tv_nsec * 1e-9));
}

void run_openmp_test(long runs, long iters, double *times, uint64_t *gbg) {
    printf("OpenMP test: %ld runs of %ld iterations.\n", runs, iters);
    double t0 = clock_mono_us();
    for (long run=0; run<runs; run++) {
#pragma omp parallel for
        for (long iter=0; iter<iters; iter++) {
            gbg[iter]++;
        }
        double t1 = clock_mono_us();
        times[run] = t1 - t0;
        t0 = t1;
    }
}

void run_single_test(long runs, long iters, double *times, uint64_t *gbg) {
    printf("Single test: %ld runs of %ld iterations.\n", runs, iters);
    double t0 = clock_mono_us();
    for (long run=0; run<runs; run++) {
        for (long iter=0; iter<iters; iter++) {
            gbg[iter]++;
        }
        double t1 = clock_mono_us();
        times[run] = t1 - t0;
        t0 = t1;
    }
}

struct context {
    long runs;
    long iters;
    uint64_t *gbg;
    double *times;
    int id;
    pthread_barrier_t *barr_p;
};

struct context contexts[THREADS];

void *run_pthread_inner(void *context_v) {
    struct context *context = context_v;
    long runs = context->runs;
    long iters = context->iters;
    long iter_share = iters / THREADS;
    int id = context->id;
    long iter_from = iter_share * id;
    long iter_to = iter_from + iter_share;
    double *times = context->times;
    if (context->id == (THREADS-1)) {
        iter_to = iters;
    }
    uint64_t *gbg = context->gbg;
    pthread_barrier_t *barr_p = context->barr_p;
    pthread_barrier_wait(barr_p);
    double t0 = 0;
    if (id == 0) {
        t0 = clock_mono_us();
    }
    for (long run=0; run<runs; run++) {
        for (long iter=iter_from; iter<iter_to; iter++) {
            gbg[iter]++;
        }
        if (id == 0) {
            double t1 = clock_mono_us();
            times[run] = t1 - t0;
            t0 = t1;
        }
        pthread_barrier_wait(barr_p);
    }
    return NULL;
}

void run_pthread_test(long runs, long iters, double *times, uint64_t *gbg) {
    printf("Thread test: %ld runs of %ld iterations.\n", runs, iters);
    pthread_barrier_t barr;
    pthread_barrier_init(&barr, NULL, THREADS);
    pthread_t threads[THREADS];
    for (int i=0; i<THREADS; i++) {
        contexts[i].runs = runs;
        contexts[i].iters = iters;
        contexts[i].times = times;
        contexts[i].gbg = gbg;
        contexts[i].id = i;
        contexts[i].barr_p = &barr;
        pthread_create(threads+i, NULL, run_pthread_inner, contexts+i);
        if (errno) { abort(); } /* meh, error handling */
    }
    for (int i=0; i<THREADS; i++) {
        pthread_join(threads[i], NULL);
        if (errno) { abort(); } /* meh, error handling */
    }
}

#define ALLOC_WITHOUT_OVERFLOW(p, n) do {               \
    size_t to_alloc = sizeof(*(p)) * (n);               \
    if ((to_alloc / (n)) != sizeof(*(p))) { p = NULL; } \
    else { p = malloc(to_alloc); } } while(0)

int run_test(long runs, long iters, int with_loop) {
    double *run_times;
    ALLOC_WITHOUT_OVERFLOW(run_times, runs);
    uint64_t *iter_bits;
    ALLOC_WITHOUT_OVERFLOW(iter_bits, iters);
    if (run_times == NULL) {
        printf("Couldn't alloc %ld doubles.\n", runs);
        return 1;
    }
    if (iter_bits == NULL) {
        printf("Couldn't alloc %ld uint64_ts.\n", iters);
        return 1;
    }
    for (long run=0; run<runs; run++) {
        run_times[run] = 0.0;
    }
    for (long iter=0; iter<iters; iter++) {
        iter_bits[iter] = iter & 0xFFFF;
    }

    switch (with_loop) {
    case 0:
        run_single_test(runs, iters, run_times, iter_bits);
        break;
    case 1:
        run_openmp_test(runs, iters, run_times, iter_bits);
        break;
    case 2:
        run_pthread_test(runs, iters, run_times, iter_bits);
        break;
    }

    uint64_t ac = 0;
    for (long iter=0; iter<iters; iter++) {
        ac += iter_bits[iter];
    }

    double sum = 0;
    for (long run=0; run<runs; run++) {
        sum += run_times[run];
    }
    double mean = sum / (double)runs;
    double sum_squared_diff = 0;
    for (long run=0; run<runs; run++) {
        double diff = run_times[run] - mean;
        sum_squared_diff += (diff * diff);
    }
    double variance = sum_squared_diff / (double)runs;
    double std_dev = sqrt(variance);

    double top = 0;
    for (long run=0; run<runs; run++) {
        if (run_times[run] > top) {
            top = run_times[run];
        }
    }
    double bot = top;
    for (long run=0; run<runs; run++) {
        if (run_times[run] < bot) {
            bot = run_times[run];
        }
    }

    printf("Garbage sum: %lu.\n", (unsigned long)ac);
    printf("(garbage should be equal between different loop types)\n");
    printf("times: mean %fus, std_dev %fus.\n", mean, std_dev);
    printf("times: min %fus, max %fus.\n", bot, top);

    long howmany, prev_howmany = 0;
    double threshold = 0, each_diff = top/25.0;
    do {
        howmany = 0;
        for (long run=0; run<runs; run++) {
            if (run_times[run] >= threshold) {
                howmany++;
            }
        }
        if ((howmany > 1) && (prev_howmany != howmany)) {
            printf("runs: %9ld took >=%fus\n", howmany, threshold);
        }
        threshold += each_diff;
        prev_howmany = howmany;
    } while(howmany > 1);
    printf("runs: %9ld took ==%fus\n", 1L, top);

    free(run_times);
    run_times = NULL;
    free(iter_bits);
    iter_bits = NULL;

    return 0;
}

int main(int argc, char **argv) {
    long runs = 5;
    long iters = 1024;
    int with_loop = 0;
    int fail = 0;
    if (argc > 4) {
        printf("%d is too many arguments.\n", argc-1);
        fail = 1;
    }
    if (argc > 3) {
        iters = strtol(argv[3], NULL, 10);
        if ((errno) || (iters <= 0)) {
            printf("%s is not an in-range integer.\n", argv[3]);
            fail = 1;
        }
    }
    if (argc > 2) {
        runs = strtol(argv[2], NULL, 10);
        if ((errno) || (runs <= 0)) {
            printf("%s is not an in-range integer.\n", argv[2]);
            fail = 1;
        }

    }
    if (argc > 1) {
        if (strcmp(argv[1], "p") == 0) {
            with_loop = 2;
        } else if (strcmp(argv[1], "y") == 0) {
            with_loop = 1;
        } else if (strcmp(argv[1], "n") == 0) {
            with_loop = 0;
        } else {
            printf("%s is not in 'y', 'n', 'p'.\n", argv[1]);
            fail = 1;
        }
    }
    if (fail) {
        fprintf(stderr, "Usage: openmp_frequency [y|n] [5] [1024]\n");
        return 1;
    }
    return run_test(runs, iters, with_loop);
}
