/* How slow is bubblesort on singly-linked lists, anyway? */
/* For comparison, I've implemented a heapsort too. */
/* gcc -O3 bubble.c -o bubble -Wall -Wextra -ansi -pedantic -std=c99 */
/* for size in 32 64 128 256 257 258 259 260; do */
/*     ./bubble $size h | openssl md5 */
/*     ./bubble $size b | openssl md5 */
/*     echo "++" */
/* done */
/* time ./bubble 65536 h | openssl md5 */
/* time ./bubble 65536 b | openssl md5 */


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define DIE_STRIFY(x) #x
#define DIE(...) do { fprintf(stderr, __VA_ARGS__); perror(DIE_STRIFY(__LINE__)); abort(); } while(0)

typedef struct lle {
	struct lle *next;
	int data;
} lle;

static void print_ll(lle* start) {
	size_t i;
	lle *elem;
	for (elem = start, i = 0; elem != NULL; elem = elem->next, i++) {
		printf("ll %.6zu %d\n", i, elem->data);
	}
}

static size_t llen(lle* list) {
	size_t len = 0;
	for (; list; list=list->next) {
		len++;
	}
	return len;
}

static size_t heap_lix(size_t i) {
	return (i*2) + 1;
}
static size_t heap_rix(size_t i) {
	return (i*2) + 2;
}
static size_t heap_uix(size_t i) {
	return (i-1) / 2;
}
static void heap_bubble_up(size_t i, lle **ptrs) {
	size_t u;
	lle *up;

	for (;;) {
		if (i == 0) { return; }
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

static void heap_sift_down(size_t u, size_t len, lle **ptrs) {
	size_t l, r;
	lle* swap;

	l = heap_lix(u);
	if ((l < len) && (ptrs[l]->data > ptrs[u]->data)) {
		swap = ptrs[u];
		ptrs[u] = ptrs[l];
		ptrs[l] = swap;
		heap_sift_down(l, len, ptrs);
	}
	r = heap_rix(u);
	if ((r < len) && (ptrs[r]->data > ptrs[u]->data)) {
		swap = ptrs[u];
		ptrs[u] = ptrs[r];
		ptrs[r] = swap;
		heap_sift_down(r, len, ptrs);
	}
}

static void heapsort(lle **startp) {
	lle *start = *startp;
	lle **ptrs, *swap, *here;
	size_t len, alloc, ptr_index, top;

	if (start == NULL) {
		return;
	}

	len = llen(start), alloc, ptr_index;
	alloc = sizeof(lle*) * len;
	if ((alloc / sizeof(lle*)) != len) {
		DIE("%zu pointers is too many to think about allocating.\n", len);
	}
	ptrs = malloc(alloc);
	if (ptrs == NULL) {
		DIE("%zu pointers is too many to actually allocate.\n", len);
	}

	for (ptr_index=0, here=start; here; ptr_index++, here=here->next) {
		ptrs[ptr_index] = here;
	}

	/* printf("---\n"); */

	/* size_t i; */
	/* for (i=0; i < len; i++) { */
	/* 	printf("ptrs[%.6zu]->data = %d\n", i, ptrs[i]->data); */
	/* } */
	/* printf("===\n"); */

	/* Establish heap property here. */
	for (ptr_index=0; ptr_index < len; ptr_index++) {
		heap_bubble_up(ptr_index, ptrs);
	}

	/* for (i=0; i < len; i++) { */
	/* 	printf("ptrs[%.6zu]->data = %d\n", i, ptrs[i]->data); */
	/* } */
	/* printf("zzz\n\n"); */

	for (ptr_index=len; ptr_index > 0; ptr_index--) {
		top = ptr_index - 1;
		swap = ptrs[0];
		ptrs[0] = ptrs[top];
		ptrs[top] = swap;
		
		heap_sift_down(0, top, ptrs);
	}

	for (ptr_index=0; ptr_index < len; ptr_index++) {
		ptrs[ptr_index]->next = ptrs[ptr_index+1];
	}
	ptrs[len-1]->next = NULL;

	start = ptrs[0];

	*startp = start;
}

static void bubblesort(lle **startp) {
	lle *start = *startp;
	lle *here, *next;
	int swapped;

	lle **before;

	if (start == NULL) {
		return;
	}

	here = start;
	next = here->next;

	do {
		swapped = 0;
		for (here=start, before=&start; here && here->next; before=&here->next, here=here->next) {
			next = here->next;
			if (here->data > next->data) {
				here->next = next->next;
				next->next = here;
				*before = next;
				here = next;
				next = here->next;
				swapped = 1;
			}
		}
	} while(swapped);

	*startp = start;
}

int main(int argc, char **argv) {
	size_t sort_me, alloc, index;
	lle* start, *allob;

	if (argc != 3) {
		DIE("Usage: ./bubble [0-9]+ [bh]\n");
	}
	if (sscanf(argv[1], "%zu", &sort_me) != 1) {
		DIE("Can't read arg as size_t.\n");
	}
	if (strlen(argv[2]) != 1) {
		DIE("Too many letters in arg 2.\n");
	}

	srand(sort_me & 65535);

	if ((argv[2][0] != 'b') && (argv[2][0] != 'h')) {
		DIE("Wrong letter in arg 2.\n");
	}
	alloc = sizeof(lle) * sort_me;
	if ( (alloc / sizeof(lle)) != sort_me ) {
		DIE("Overflow, won't alloc %zu / %zu.\n", sort_me, alloc);
	}
	printf("allocating %zu\n", sort_me);
	allob = malloc(alloc);
	if (allob == NULL) {
		DIE("Couldn't allocate %zu anyway.\n", alloc);
	}

	for (index = 0; index < sort_me; index++) {
		/* allob[index].prev = &allob[index-1]; */
		allob[index].next = &allob[index+1];
		allob[index].data = rand();
	}
	allob[sort_me-1].next = NULL;
	/* allob[0].prev = NULL; */

	start = allob;

	printf("pre-sort:\n");
	print_ll(start);

	switch (argv[2][0]) {
	case 'b':
		bubblesort(&start);
		break;
	case 'h':
		heapsort(&start);
		break;
	default:
		DIE("Fell off the end of a switch.\n");
	}

	printf("post-sort:\n");
	print_ll(start);

	free(allob);
	return 0;
}
