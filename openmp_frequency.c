/* compile: gcc -fopenmp -Wall -std=c99 openmp_frequency.c -lrt -lm -O3 -o openmp_frequency */
/* usage ./openmp_frequency [y|n] [no of tests] [work factor per test] */
/* performs busywork many times, prints how long iterations take */
/* with y, uses OpenMP */
/* with n, skips OpenMP */

/* Typical test session looks like: */
/* gcc -fopenmp -Wall -std=c99 openmp_frequency.c -lrt -lm -O3 -o openmp_frequency */
/* ./openmp_frequency n 1000000 100 */
/* ./openmp_frequency y 1000000 100 */

/* Busywork done here isn't enough to stress CPU, so OpenMP is expected */
/* to be slower than non-OpenMP. What I'm trying to measure here is how */
/* much overhead it actually has per parallel-for start-and-stop. */

#define _GNU_SOURCE
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

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

#define ALLOC_WITHOUT_OVERFLOW(p, n) do {               \
    size_t to_alloc = sizeof(*(p)) * (n);               \
    if ((to_alloc / (n)) != sizeof(*(p))) { p = NULL; } \
    else { p = malloc(to_alloc); } } while(0)

int run_test(long runs, long iters, int with_openmp) {
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

    if (with_openmp) {
        run_openmp_test(runs, iters, run_times, iter_bits);
    } else {
        run_single_test(runs, iters, run_times, iter_bits);
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
    printf("(garbage should be equal between OpenMP and single-thread runs)\n");
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
    int with_openmp = 0;
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
        if (strcmp(argv[1], "y") == 0) {
            with_openmp = 1;
        } else if (strcmp(argv[1], "n") == 0) {
            with_openmp = 0;
        } else {
            printf("%s is neither 'y' nor 'n'.\n", argv[1]);
            fail = 1;
        }
    }
    if (fail) {
        fprintf(stderr, "Usage: openmp_frequency [y|n] [5] [1024]\n");
        return 1;
    }
    return run_test(runs, iters, with_openmp);
}
