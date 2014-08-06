/* How slow is bubblesort on singly-linked lists, anyway? */
/* For comparison, I've implemented a heapsort too. */
/* And a bottom-up mergesort which amusingly takes O(1) space. */
/* gcc -O3 bubble.c -o bubble -Wall -Wextra -ansi -pedantic -std=c99 */
/* for size in 32 64 128 256 257 258 259 260; do */
/*     ./bubble $size h | openssl md5 */
/*     ./bubble $size m | openssl md5 */
/*     ./bubble $size b | openssl md5 */
/*     echo "++" */
/* done */
/* time ./bubble 65536 h | openssl md5 */
/* time ./bubble 65536 m | openssl md5 */
/* time ./bubble 65536 b | openssl md5 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define DIE_STRIFY(x) #x
#define DIE(...)                                                               \
	do {                                                                   \
		fprintf(stderr, __VA_ARGS__);                                  \
		perror(DIE_STRIFY(__LINE__));                                  \
		abort();                                                       \
	} while (0)

#ifdef I_WANT_TO_DEBUG
#define DEBUG(...)                                                             \
	do {                                                                   \
		printf(__VA_ARGS__);                                           \
	} while (0)
#else
#define DEBUG(...)                                                             \
	do {                                                                   \
	} while (0)
#endif

typedef struct lle
{
	struct lle *next;
	int data;
} lle;

static void print_ll(lle *start)
{
	size_t i;
	lle *elem;
	for (elem = start, i = 0; elem != NULL; elem = elem->next, i++) {
		printf("ll %.6zu %d\n", i, elem->data);
	}
}

static size_t llen(lle *list)
{
	size_t len = 0;
	for (; list; list = list->next) {
		len++;
	}
	return len;
}

static size_t heap_lix(size_t i)
{
	return (i * 2) + 1;
}
static size_t heap_rix(size_t i)
{
	return (i * 2) + 2;
}
static size_t heap_uix(size_t i)
{
	return (i - 1) / 2;
}
static void heap_bubble_up(size_t i, lle **ptrs)
{
	size_t u;
	lle *up;

	for (;;) {
		if (i == 0) {
			return;
		}
		u = heap_uix(i);
		if (ptrs[u]->data > ptrs[i]->data) {
			/* We're already the right way around. */
			return;
		}

		/* No? Swap with parent. */
		up = ptrs[u];
		ptrs[u] = ptrs[i];
		ptrs[i] = up;

		/* ...and keep bubbling up. */
		i = u;
	}
}

static void heap_sift_down(size_t u, size_t len, lle **ptrs)
{
	size_t l, r, swapme;
	lle *swap;

	for (;;) {
		l = heap_lix(u);
		r = heap_rix(u);

		if ((l < len) && (ptrs[l]->data > ptrs[u]->data)) {
			if ((r < len) && (ptrs[r]->data > ptrs[l]->data)) {
				swapme = r;
			} else {
				swapme = l;
			}
		} else if ((r < len) && (ptrs[r]->data > ptrs[u]->data)) {
			swapme = r;
		} else {
			return;
		}
		swap = ptrs[u];
		ptrs[u] = ptrs[swapme];
		ptrs[swapme] = swap;
		u = swapme;
	}
}

static int heapsort(lle **startp)
{
	lle **ptrs, *swap, *here;
	size_t len, alloc, ptr_index, top;

	if (*startp == NULL) {
		return 0;
	}

	len = llen(*startp);
	alloc = sizeof(lle *) * len;
	if ((alloc / sizeof(lle *)) != len) {
		return -1;
	}
	ptrs = malloc(alloc);
	if (ptrs == NULL) {
		return -2;
	}

	for (ptr_index = 0, here = *startp; here;
	     ptr_index++, here = here->next) {
		ptrs[ptr_index] = here;
	}

	/* Establish heap property here. */
	for (ptr_index = 0; ptr_index < len; ptr_index++) {
		heap_bubble_up(ptr_index, ptrs);
	}

	for (ptr_index = len; ptr_index > 0; ptr_index--) {
		top = ptr_index - 1;
		swap = ptrs[0];
		ptrs[0] = ptrs[top];
		ptrs[top] = swap;

		heap_sift_down(0, top, ptrs);
	}

	for (ptr_index = 0; ptr_index < len; ptr_index++) {
		ptrs[ptr_index]->next = ptrs[ptr_index + 1];
	}
	ptrs[len - 1]->next = NULL;

	*startp = ptrs[0];
	free(ptrs);

	return 0;
}

static void bubblesort(lle **startp)
{
	lle *here, **before, *next;
	size_t len, gofor, gonefor;

	if (*startp == NULL) {
		return;
	}

	len = llen(*startp);

	for (gofor = len; gofor > 0; gofor--) {
		gonefor = 0;
		before = startp;
		here = *before;
		for (gonefor = 1; gonefor < gofor; gonefor++) {
			next = here->next;
			if (here->data > next->data) {
				here->next = next->next;
				next->next = here;
				*before = next;
				here = next;
				next = here->next;
			}
			before = &here->next;
			here = here->next;
		}
	}
}

#ifndef BU_MERGE_INITIAL_RUN_SIZE
#define BU_MERGE_INITIAL_RUN_SIZE 1
#endif

static void bu_merge_groups(lle **startp)
{
	lle *here;
	lle *here_n_was;
	lle **before = startp;
	int i, still_going;

	do {
		still_going = 0;
		here = *before;
		for (i = 0; i < (BU_MERGE_INITIAL_RUN_SIZE - 1); i++) {
			if (here == NULL) {
				break;
			}
			here = here->next;
		}
		if (here) {
			still_going = 1;
			here_n_was = here->next;
			here->next = NULL;
		}
		bubblesort(before);
		here = *before;
		while (here && here->next) {
			here = here->next;
		}
		if (still_going) {
			if (here->next != NULL)
				DIE("assert fail");
			here->next = here_n_was;
		}
		before = &here->next;
	} while (*before);
}

static void bottomupmergesort(lle **startp)
{
	lle **before_l, **before_r;
	lle *here_l, *here_r, *next_here_r;
	size_t run_size = BU_MERGE_INITIAL_RUN_SIZE, new_run_size;
	size_t pos_l, pos_r, pos_m, offset;
	size_t len;

	len = llen(*startp);
	if (len == 0) {
		return;
	}

	/* This function contains no variably-sized allocations, only a fixed
	 * set
	 * of automatic variables. Hence its data space usage is O(1). */
	/* This function doesn't recurse, so its stack usage is O(1). */

	/* Perhaps we can make this faster by starting with small runs sorted?
	 */
	if (BU_MERGE_INITIAL_RUN_SIZE > 1) {
		bu_merge_groups(startp);
	}

	/* This loop repeats ceil(log2(len)) times.
	 * It repeats until run_size >= len. Every iteration doubles run_size.
	 */
	for (;;) {
		/* here_l == *before_l, at the start and end of every basic
		 * block */
		here_l = *startp;
		before_l = startp;
		offset = 0;

		/* If the left-hand sorted-run covers the entire list, then the
		 * entire list must be sorted and we're done. */
		if (run_size >= len) {
			return;
		}

		DEBUG("run_size = %zd\n", run_size);

		/* This loop repeats len/run_size times. */
		/* Each of the loops in it takes O(run_size) time. */
		/* Hence its run-time is O(run_size * len/run_size) = O(len). */
		/* Hence the whole function's run-time is O(len * log2(len)). */
		for (;;) {
			DEBUG("run\n");

			/* The right-hand run will definitely be empty, so
			 * immediately
			 * skip up to the next merge size. */
			if (len - offset <= run_size) {
				goto done_merging;
			}

			/* Advance right pointer run_size spaces up from left
			 * pointer. */
			/* Afterwards, here_r will be at the start of the
			 * right-hand run
			 * and before_r will point at the list pointer to
			 * *here_r. */
			here_r = here_l;
			before_r = before_l;
			for (pos_m = 0; pos_m < run_size; pos_m++) {
				/* Tried this, made no difference that I could
				 * measure: */
				/* __builtin_prefetch(here_r->next); */
				before_r = &here_r->next;
				here_r = here_r->next;
			}

			/* run_size - pos_l = number of elements in left-hand
			 * run */
			/* (this invariant is maintained from here on) */
			/* run_size - pos_r = number of elements in right-hand
			 * run */
			/* (this invariant is true after the next paragraph) */
			pos_l = 0;
			pos_r = 0;

			/* If we're in the final segment, the right-hand run may
			 * be
			 * shorter than run_size, so increment pos_r by the
			 * number of
			 * elements that it's short by. This makes the later
			 * loops
			 * that compare pos_r to run_size drop out at the actual
			 * size
			 * of the right-hand run, rather than running past the
			 * end. */
			/* (pos_r's invariant is now maintained from here on) */
			offset += run_size;
			if (len - offset < run_size) {
				pos_r = run_size - (len - offset);
				offset += run_size - pos_r;
			} else {
				offset += run_size;
			}

			/* Now advance down both the left and right lists, emit
			 * the
			 * smaller of the left-min and the right-min each time.
			 * If
			 * If either list reaches its endpoint, continue
			 * advancing
			 *  down the other one. */
			while ((pos_l < run_size) && (pos_r < run_size)) {
				if (here_l->data <= here_r->data) {
					/* A left-emit can just skip here_l one
					 * element. */
					DEBUG("merge l [%d] %d\n", here_l->data,
					      here_r->data);
					before_l = &here_l->next;
					here_l = here_l->next;
					pos_l++;
				} else {
					DEBUG("merge r %d [%d]\n", here_l->data,
					      here_r->data);
					/* A right-emit moves the *here_r node
					 * so that it now
					 * comes before the *here_l node. This
					 * necessitates
					 * setting *before_l to slot it in
					 * front, and advancing
					 * *before_r to take it out of the
					 * right-hand run. */
					next_here_r = here_r->next;
					*before_l = here_r;
					*before_r = here_r->next;
					before_l = &here_r->next;
					here_r->next = here_l;
					here_r = next_here_r;
					pos_r++;
				}
			}

			/* Regardless of which run finished first, all of the
			 * remaining
			 * elements of the other run should be emitted, and
			 * here_l is
			 * pointing to something that's 2*run_size-pos_r-pos_l
			 * elements
			 * behind the end of the right-hand run.
			 * We can emit them all just by advancing here_l past
			 * them. */
			for (; pos_r < run_size; pos_r++) {
				DEBUG("append r [%d]\n", here_l->data);
				before_l = &here_l->next;
				here_l = here_l->next;
			}
			for (; pos_l < run_size; pos_l++) {
				DEBUG("append l [%d]\n", here_l->data);
				before_l = &here_l->next;
				here_l = here_l->next;
			}
		}

	done_merging:

		new_run_size = run_size << 1;
		if (new_run_size < run_size) {
			DIE("run_size overflowed");
		}
		run_size = new_run_size;
	}
}

size_t sort_me_cheat;

int main(int argc, char **argv)
{
	size_t sort_me, alloc, index;
	lle *start, *allob;
	int sort_rv;

	if (argc != 3) {
		DIE("Usage: ./bubble [0-9]+ [bhm]\n");
	}
	if (sscanf(argv[1], "%zu", &sort_me) != 1) {
		DIE("Can't read arg as size_t.\n");
	}
	if (strlen(argv[2]) != 1) {
		DIE("Too many letters in arg 2.\n");
	}

	srand(sort_me & 65535);
	sort_me_cheat = sort_me;

	if ((argv[2][0] != 'b') && (argv[2][0] != 'h') && (argv[2][0] != 'm')) {
		DIE("Wrong letter in arg 2.\n");
	}
	alloc = sizeof(lle) * sort_me;
	if ((alloc / sizeof(lle)) != sort_me) {
		DIE("Overflow, won't alloc %zu / %zu.\n", sort_me, alloc);
	}
	printf("allocating %zu\n", sort_me);
	allob = malloc(alloc);
	if (allob == NULL) {
		DIE("Couldn't allocate %zu anyway.\n", alloc);
	}

	for (index = 0; index < sort_me; index++) {
		allob[index].next = &allob[index + 1];
		allob[index].data = rand(); /* (sort_me - index); */
	}
	allob[sort_me - 1].next = NULL;

	start = allob;

	printf("pre-sort:\n");
	print_ll(start);

	switch (argv[2][0]) {
	case 'b':
		bubblesort(&start);
		break;
	case 'h':
		sort_rv = heapsort(&start);
		if (sort_rv == -1) {
			DIE("Too many pointers to think about allocating.\n");
		}
		if (sort_rv == -2) {
			DIE("Too many pointers to actually allocate.\n");
		}
		break;
	case 'm':
		bottomupmergesort(&start);
		break;
	default:
		DIE("Fell off the end of a switch.\n");
	}

	printf("post-sort:\n");
	print_ll(start);

	free(allob);
	return 0;
}
