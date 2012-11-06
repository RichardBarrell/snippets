/* How slow is bubblesort on singly-linked lists, anyway? */
/* gcc -O3 bubble.c -o bubble */
/* time ./bubble 4096 >/dev/null */
/* time ./bubble 8192 >/dev/null */
/* time ./bubble 16384 >/dev/null */
/* time ./bubble 32768 >/dev/null */
/* time ./bubble 65536 >/dev/null */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#define DIE_STRIFY(x) #x
#define DIE(...) do { fprintf(stderr, __VA_ARGS__); perror(DIE_STRIFY(__LINE__)); abort(); } while(0)

typedef struct lle {
	struct lle *next;
	int data;
} lle;

void print_ll(lle* start) {
	size_t i;
	lle *elem;
	for (elem = start, i = 0; elem != NULL; elem = elem->next, i++) {
		printf("ll %zu %d\n", i, elem->data);
	}
}

void bubblesort(lle **startp) {
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
	if (argc == 1) {
		DIE("Usage: ./bubble [0-9]+\n");
	}
	if (sscanf(argv[1], "%zu", &sort_me) != 1) {
		DIE("Can't read arg as size_t.\n");
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
		allob[index].data = (sort_me - index) & 65535;
	}
	allob[sort_me-1].next = NULL;
	/* allob[0].prev = NULL; */

	start = allob;

	printf("pre-sort:\n");
	print_ll(start);
	bubblesort(&start);
	printf("post-sort:\n");
	print_ll(start);

	free(allob);
	return 0;
}
