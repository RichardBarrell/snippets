/* single producer, multiple consumer queue which throws away
   enqueued data instead of blocking producer indefinitely if
   consumers fall behind.

   Compile:

    clang -o spmc_lossy spmc_lossy.c -Os -g -Wall -Wextra -std=c99 -lpthread

   Run:

    ./spmc_lossy create

   and then run, possibly simultaneously:

    ./spmc_lossy produce # (send newline-terminated bytes into stdin)

   and:

    ./spmc_lossy consume # (and watch it receive some of the bytes in produce)

   You should be able to pause the consumer / make the producer run ahead of it
   and see that the producer doesn't stop for anything, and the consumer will
   always be able to get the last slightly-less-than-1kB of lines that the
   producer last ingested.

   Improvements this could brook:
     * wants a hazard pointer so that consumers don't need to hold the lock
       on the queue while processing an item (e.g. to memcpy its data elsewhere)
     * signal handlers so stuff could respond to SIGINT properly
*/

#define _POSIX_C_SOURCE 200112L
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <semaphore.h>

#define QUEUE_FILENAME "./spmc_queue"
#define QUEUE_SIZE (1<<10)
#define MAX_ITEM_SZ (QUEUE_SIZE - sizeof(q_head) - sizeof(q_item))

// #define DEBUG(...) do { fprintf(stderr, __VA_ARGS__); } while(0)
#define DEBUG(...) do { } while(0)

typedef unsigned int uint;

typedef struct q_head {
	sem_t lock;
	sem_t consumer_sem;
	uint first_item_location;
	uint last_item_location;
	uint bytes_available;
} q_head;

typedef struct q_item {
	uint length;
	uint next;
	char data[0];
} q_item;

typedef struct queue {
	q_head *head;
	void *region;
} queue;

static size_t align_up_to(size_t x, size_t n)
{
	assert(SIZE_MAX - x > n);
	size_t a = x + (n-1);
	return (a - (a % n));
}

static int print_usage(const char *name) {
	const char *m = "Usage: %s COMMAND where COMMAND is one of:\n"
		        " create - create a new shared memory queue\n"
		        " produce - take lines from stdin and enqueue them\n"
		        " consume - dequeue items and print them\n";
	fprintf(stderr, m, name);
	return 125;
}

static int open_queue(queue *q) {
	int fd = open(QUEUE_FILENAME, O_RDWR);
	if (fd < 0) {
		perror("Couldn't open " QUEUE_FILENAME ": ");
		return 1;
	}
	void *region = mmap(NULL, QUEUE_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
	if (region == MAP_FAILED) {
		perror("Couldn't mmap " QUEUE_FILENAME ": ");
		return 3;
	}
	close(fd);
	q->region = region;
	q->head = (q_head *)region;
	return 0;
}

static int close_queue(queue *q) {
	if (munmap(q->region, QUEUE_SIZE)) {
		perror("Couldn't munmap queue: ");
		return 6;
	}
	q->region = NULL;
	q->head = NULL;
	return 0;
}

static int be_consumer(void) {
	printf("Am consumer.\n");
	int failing = 0;
	queue q;
	{
		int open_fail = open_queue(&q);
		if (open_fail) return open_fail;
	}
	q_head *h = q.head;
	void *region = q.region;
	for (;;) {
		if (sem_wait(&h->consumer_sem)) {
			if (errno == EINTR) break;
			perror("sem_wait consumer_sem: ");
			abort();
		}
		DEBUG("c+sem\n");
		if (sem_wait(&h->lock)) {
			perror("sem_wait failed: ");
			abort();
		}
		DEBUG("c+lock\n");
		if (h->first_item_location) {
			q_item *item = region + h->first_item_location;
			printf("I saw: %u bytes: ", item->length);;
			fwrite(item->data, 1, item->length, stdout);
			printf("!\n");
			h->first_item_location = item->next;
			if (!h->first_item_location) {
				h->last_item_location = 0;
			}
		}
		if (sem_post(&h->lock)) {
			perror("sem_post failed: ");
			abort();
		}
		DEBUG("c-lock\n");
	}
	{
		int close_fail = close_queue(&q);
		if (close_fail) return close_fail;
	}
	// consumer_done:
	return failing;
}

static void enqueue_line(queue *q, const char *begin, const char *end)
{
	q_head *h = q->head;
	void *region = q->region;
	for (;;) {
		int r = sem_wait(&h->lock);
		if (r == 0) break;
		if (errno == EINTR) continue;
		perror("sem_wait failed: ");
		abort();
	}
	uint line_bytes = end - begin;
	uint bytes_needed = line_bytes + sizeof(q_item);
	uint max_line_len = h->bytes_available - sizeof(q_item);
	uint min_loc = sizeof(q_head);
	uint max_loc = sizeof(q_head) + h->bytes_available;
	DEBUG("enq %u bytes (%u), fst=%u, last=%u\n",
		line_bytes, bytes_needed,
		h->first_item_location,
		h->last_item_location);
	if (bytes_needed > h->bytes_available) {
		fprintf(stderr,
			"Can't enqueue line with %u bytes (max=%u)!\n",
			line_bytes,
			max_line_len);
		goto enqueue_exit;
	}
	uint new_loc;
	if (!h->last_item_location) {
		assert (!h->first_item_location);
		new_loc = min_loc;
		goto enqueue_build;
	}
	{
		q_item *last = q->region + h->last_item_location;
		assert(last->next == 0);
		uint after_last = h->last_item_location + last->length + sizeof(q_item);
		after_last = align_up_to(after_last, sizeof(q_item));
		uint new_end;
		new_loc = after_last;
		new_end = new_loc + bytes_needed;
		DEBUG("new_loc=%u, new_end=%u\n", new_loc, new_end);
		if (new_end > max_loc) {
			new_loc = min_loc;
			new_end = new_loc + bytes_needed;
			DEBUG("wrap! new_loc=%u, new_end=%u\n", new_loc, new_end);
		}
		last->next = new_loc;
		while ((new_loc <= h->first_item_location) && (new_end > h->first_item_location) && h->first_item_location) {
			q_item *old_first = q->region + h->first_item_location;
			DEBUG("strip %u to %u\n", h->first_item_location, old_first->next);
			h->first_item_location = old_first->next;
			assert(h->first_item_location < QUEUE_SIZE);
		}
	}
 enqueue_build:
	{
		if (!h->first_item_location) {
			h->first_item_location = new_loc;
			DEBUG("set fil <- %u\n", new_loc);
			assert(new_loc < QUEUE_SIZE);
		}
		h->last_item_location = new_loc;
		DEBUG("lil <- %u\n", new_loc);
		q_item *new_item = region + new_loc;
		new_item->length = line_bytes;
		new_item->next = 0;
		memcpy(new_item->data, begin, line_bytes);
		if (sem_post(&h->consumer_sem)) {
			perror("sem_post consumer sem: ");
			abort();
		}
		DEBUG("p-sem\n");
	}
 enqueue_exit:
	if (sem_post(&h->lock)) {
		perror("sem_post failed: ");
		abort();
	};
}

static int be_producer(void) {
	printf("Am producer.\n");
	int failing = 0;
	char *lbuf = NULL;
	queue q;
	{
		int open_fail = open_queue(&q);
		if (open_fail) return open_fail;
	}
	size_t lbuf_sz = 2;
	size_t lbuf_used = 0;
	lbuf = malloc(lbuf_sz);
	if (lbuf == NULL) {
		perror("malloc hates you: ");
		failing = 1; goto producer_exit;
	}
	for (;;) {
		ssize_t got = read(0, lbuf + lbuf_used, lbuf_sz - lbuf_used);
		if (got < 0) {
			perror("failed to read stdin: ");
			failing = 1; goto producer_exit;
		}
		if (got == 0) {
			if (lbuf_used) {
				enqueue_line(&q, lbuf, lbuf + lbuf_used);
			}
			goto producer_exit;
		}
		lbuf_used += got;
		char *lbuf_end = lbuf + lbuf_used;
		char *line_start, *line_end;
		line_start = lbuf;
		while ((line_end = memchr(line_start, '\n', lbuf_end - line_start))) {
			enqueue_line(&q, line_start, line_end);
			line_start = line_end + 1;
		}
		size_t lbuf_left = lbuf_end - line_start;
		if ((lbuf_left > 0) && (line_start != lbuf)) {
			memmove(lbuf, line_start, lbuf_left);
		}
		lbuf_used = lbuf_left;
		if (lbuf_used == lbuf_sz) {
			size_t new_sz = lbuf_sz << 1;
			if (new_sz < lbuf_sz) {
				fprintf(stderr, "lbuf_sz overflow!\n");
				abort();
			}
			char *new_lbuf = realloc(lbuf, new_sz);
			if (!new_lbuf) {
				perror("Can't enlarge lbuf: ");
				failing = 1; goto producer_exit;
			}
			lbuf = new_lbuf;
			lbuf_sz = new_sz;
		}
	}
 producer_exit:
	free(lbuf);
	{
		int close_fail = close_queue(&q);
		if (close_fail) return close_fail;
	}
	return failing;
}

static int create_queue(void) {
	printf("Sizes: sem_t %zu, queue_head %zu, queue_item %zu\n",
	       sizeof(sem_t), sizeof(q_head), sizeof(q_item));
	void *q_region;
	struct q_head *head;
	int fd = open(QUEUE_FILENAME, O_RDWR|O_CREAT|O_TRUNC|O_EXCL, 0600);
	if (fd < 0) {
		perror("Couldn't create " QUEUE_FILENAME ": ");
		return 1;
	}
	if (ftruncate(fd, QUEUE_SIZE)) {
		perror("Couldn't set size of " QUEUE_FILENAME ": ");
		return 2;
	}
	q_region = mmap(NULL, QUEUE_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
	if (q_region == MAP_FAILED) {
		perror("Couldn't mmap " QUEUE_FILENAME ": ");
		return 3;
	}
	close(fd);
	head = (struct q_head *)q_region;
	if (sem_init(&head->lock, 1, 0)) {
		perror("Couldn't init head->lock: ");
		return 4;
	}
	if (sem_init(&head->consumer_sem, 1, 0)) {
		perror("Couldn't init head->consumer_sem: ");
		return 4;
	}
	head->first_item_location = 0;
	head->last_item_location = 0;
	head->bytes_available = QUEUE_SIZE - sizeof(struct q_head);
	if (sem_post(&head->lock)) {
		perror("sem_post on head failed during init! ");
		return 5;
	}
	munmap(q_region, QUEUE_SIZE);
	return 0;
}

int main(int argc, char **argv) {
	if (argc < 2) {
		return print_usage(argv[0]);
	}
	if (strcmp(argv[1], "create") == 0) {
		return create_queue();
	}
	if (strcmp(argv[1], "consume") == 0) {
		return be_consumer();
	}
	if (strcmp(argv[1], "produce") == 0) {
		return be_producer();
	}
	return print_usage(argv[0]);
}
