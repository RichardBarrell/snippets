/* gcc has a thing called __sync_fetch_and_add(&i, n), which
   does (i += n) atomically and returns the result.
   This file plays around with that. Mess with the parameters below
   (particularly RING_SIZE and SEGMENTS) to see how timings change
   with contention rates.
*/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <stdint.h>

#define DIE(...)                                                               \
	do {                                                                   \
		fprintf(stderr, __VA_ARGS__);                                  \
		perror("");                                                    \
		exit(1);                                                       \
	} while (0)

double nanotime()
{
	struct timespec now;
	if (clock_gettime(CLOCK_MONOTONIC, &now)) {
		DIE("Can't read clock.\n");
	}
	return (double)now.tv_sec + 1e-9 * (double)now.tv_nsec;
}

#define RING_SIZE 1
#define SEGMENTS 2
#define SEGMENT_SIZE (RING_SIZE / SEGMENTS)
#define CASINT int32_t

struct incrementer_input
{
	CASINT *ints;
	int iterations;
	int offset;
	pthread_mutex_t *go_lock;
};

void *incrementer(void *void_input)
{
	struct incrementer_input *input =
	    (struct incrementer_input *)void_input;
	pthread_mutex_lock(input->go_lock);
	int i, j, iterations = input->iterations;
	CASINT *ints = input->ints;
	int offset = input->offset;
	for (i = 0; i < RING_SIZE * iterations; ++i) {
		j = (i + offset) % RING_SIZE;
		__sync_fetch_and_add(&ints[j], 1);
	}
	return NULL;
}

int main(int argc, char **argv)
{
	long int iterations = 1024;
	if (argc == 2) {
		iterations = 1024 * strtol(argv[1], NULL, 10);
		if (errno) {
			DIE("Can't parse %s as a number.\n", argv[1]);
		}
		if (iterations < 0) {
			DIE("I'd like a *positive* number, please.\n");
		}
	}
	CASINT *ints = malloc(RING_SIZE * sizeof(CASINT));
	int i;
	for (i = 0; i < RING_SIZE; ++i) {
		ints[i] = 0;
	}
	pthread_t *threads = malloc(sizeof(pthread_t) * SEGMENTS);
	pthread_mutex_t *locks = malloc(sizeof(pthread_mutex_t) * SEGMENTS);
	struct incrementer_input *inputs =
	    malloc(sizeof(struct incrementer_input) * SEGMENTS);
	for (i = 0; i < SEGMENTS; ++i) {
		if (pthread_mutex_init(locks + i, NULL)) {
			DIE("Can't init lock.\n");
		}
		inputs[i].go_lock = &locks[i];
		inputs[i].offset = i * SEGMENT_SIZE;
		inputs[i].ints = ints;
		inputs[i].iterations = iterations;
		pthread_mutex_lock(&locks[i]);
		pthread_create(&threads[i], NULL, &incrementer, &inputs[i]);
	}

	double time_begin = nanotime();
	for (i = 0; i < SEGMENTS; ++i) {
		pthread_mutex_unlock(&locks[i]);
	}
	for (i = 0; i < SEGMENTS; ++i) {
		if (pthread_join(threads[i], NULL)) {
			DIE("pthread_join failed.\n");
		}
	}
	double time_end = nanotime();
	double time_taken = time_end - time_begin;
	printf("Ran %ld iterations in %f.\n", iterations, time_taken);
	printf("Nanoseconds/lock = %f.\n",
	       1e9 * time_taken / RING_SIZE / (double)iterations);
	return 0;
}
