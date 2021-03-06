/* Compile with:
   cc string_ln.c -o string_ln -Wall -Wextra

   Usage:
   string_ln from_dir unto_dir

   Roughly equivalent to:
   find from_dir -type f -exec ln {} unto_dir \;

   except a bit faster, and doesn't whine if a file already exists.
   Aborts at the first sign of anything even vaguely error-shaped.

   The point? I have a big, heavily-nested directory tree containing
   files with globally-unique names, and I want to hardlink it down
   into a single flat directory.
 */

#define _BSD_SOURCE
/* #define I_WANT_TO_DEBUG */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

#define DIE(...)                                                               \
	do {                                                                   \
		fprintf(stderr, "Die on line %d: ", __LINE__);                 \
		fprintf(stderr, __VA_ARGS__);                                  \
		perror(__FILE__);                                              \
		abort();                                                       \
	} while (0)

#ifdef I_WANT_TO_DEBUG
#define DEBUG(...)                                                             \
	do {                                                                   \
		fprintf(stderr, __VA_ARGS__);                                  \
	} while (0)
#else
#define DEBUG(...)                                                             \
	do {                                                                   \
	} while (0)
#endif

/* Not bothering to shrink these buffers. Size is bounded by max path length
 * anyway. */
#define UPSIZE_OR_DIE(BUFFER)                                                  \
	do {                                                                   \
		size_t new_len = BUFFER##_len << 1, esz = sizeof(BUFFER[0]),   \
		       old_sz = esz * BUFFER##_len, new_sz = esz * new_len;    \
		if ((new_sz <= old_sz) || (new_sz < esz)) {                    \
			DIE("overflow upsizing.\n");                           \
		}                                                              \
		BUFFER = realloc(BUFFER, new_sz);                              \
		if (BUFFER == NULL) {                                          \
			DIE("realloc failed.\n");                              \
		}                                                              \
		BUFFER##_len = new_len;                                        \
		DEBUG("bigger %d, oldsz %zd, newsz %zd\n", __LINE__, old_sz,   \
		      new_sz);                                                 \
	} while (0)
#define ALLOC_OR_DIE(BUFFER)                                                   \
	do {                                                                   \
		size_t esz = sizeof(BUFFER[0]), new_sz = esz * BUFFER##_len;   \
		if (new_sz < esz) {                                            \
			DIE("overflow alloc.\n");                              \
		}                                                              \
		BUFFER = malloc(new_sz);                                       \
		if (BUFFER == NULL) {                                          \
			DIE("Can't malloc()\n");                               \
		}                                                              \
	} while (0)
/* A sensible person would use an abstract data type. :P */
#define S_PUSH(STACK, VALUE)                                                   \
	do {                                                                   \
		if (STACK##_n == STACK##_len) {                                \
			UPSIZE_OR_DIE(STACK);                                  \
		}                                                              \
		STACK[STACK##_n] = VALUE;                                      \
		STACK##_n++;                                                   \
	} while (0)
#define S_POP(STACK)                                                           \
	do {                                                                   \
		if (STACK##_n == 0) {                                          \
			DIE("underflow stack %s.\n", #STACK);                  \
		}                                                              \
		STACK##_n--;                                                   \
	} while (0)
#define S_PEEK(STACK) (STACK[STACK##_n - 1])

/* This program does two things: recursive directory traversal (ignoring
 * symlinks), and
 * invocations of link(from_name, unto_name); */
int main(int argc, char **argv)
{
	char *from_dir, *unto_dir;
	size_t from_dir_len;
	size_t unto_dir_len;

	/* from_name here is a null-terminated string that gets mutated to
	 * contain
	 * every single file-name in the directory tree under from_dir. */
	char *from_name;
	size_t from_name_len = 1;
	size_t from_name_n = 0;

	/* unto_name is a null-terminated string that gets mutated to contain
	 * unto_name/<foo> for every file <foo> under from_dir. */
	char *unto_name;
	size_t unto_name_len = 1;
	size_t unto_name_n = 0;

	/* dir_stack's a stack of DIR* objects. Push onto it when finding a new
	 * subdirectory, pop from it every time we exit one. */
	DIR **dir_stack;
	size_t dir_stack_len = 1;
	size_t dir_stack_n = 0;

	/* Each of the elements of from_off_stack is a previous value of
	 * from_name_n. from_off_stack records the list of places where
	 * from_name
	 * contains a '/' or '\0', so that we can pop components off when
	 * leaving
	 * directories. */
	size_t *from_off_stack;
	size_t from_off_stack_len = 1;
	size_t from_off_stack_n = 0;

	/* Individual file-name piece. */
	char *fn;
	size_t fn_len;

	/* Returns from simple syscalls. */
	struct dirent *lookit;
	struct stat checkit;

	/* Set to 0 if file_name points to a file, 1 if it points to a
	 * directory.
	 * Otherwise, is_dir is undefined. */
	int is_dir;

	if (argc != 3) {
		fprintf(stderr, "Usage: string_ln <from_dir> <unto_dir>\n");
		return 1;
	}
	from_dir = argv[1];
	unto_dir = argv[2];

	from_dir_len = strlen(from_dir);
	unto_dir_len = strlen(unto_dir);

	/* alloc for string bytes plus terminator */
	while ((from_dir_len + 1) >= from_name_len) {
		from_name_len <<= 1;
	}
	while ((unto_dir_len + 1) >= unto_name_len) {
		unto_name_len <<= 1;
	}

	ALLOC_OR_DIE(from_name);
	ALLOC_OR_DIE(unto_name);
	ALLOC_OR_DIE(dir_stack);
	ALLOC_OR_DIE(from_off_stack);

	memcpy(from_name, from_dir, from_dir_len);
	from_name_n = from_dir_len + 1;
	from_name[from_name_n - 1] = '\0';

	/* Push the starting search directory and from_off_stack value. */
	memcpy(unto_name, unto_dir, unto_dir_len);
	unto_name_n = unto_dir_len + 1;
	unto_name[unto_name_n - 1] = '\0';

	S_PUSH(dir_stack, opendir(from_name));
	if (S_PEEK(dir_stack) == NULL) {
		DIE("Couldn't open %s (%s).\n", from_name, from_dir);
	}

	S_PUSH(from_off_stack, from_name_n);

	while (dir_stack_n) {
		/* Top of from_off_stack goes up and down as we ascend and
		 * descend
		 * the directory tree. */
		from_name_n = S_PEEK(from_off_stack);

		/* readdir() and check errno. */
		errno = 0;
		lookit = readdir(dir_stack[dir_stack_n - 1]);
		if (errno) {
			DIE("readdir() failed.\n");
		}

		/* NULL means "done with this directory" - go up and carry on.
		 */
		if (lookit == NULL) {
			DEBUG("leaving\n");
			S_POP(from_off_stack);
			if (closedir(S_PEEK(dir_stack))) {
				DIE("closedir() failed\n");
			}
			S_POP(dir_stack);
			continue;
		}

		/* Some filesystems can fill out d_type immediately, which lets
		 * us
		 * skip boring things right now. */
		switch (lookit->d_type) {
		case DT_REG:
		case DT_DIR:
		case DT_UNKNOWN:
			is_dir = lookit->d_type == DT_DIR;
			break;
		default:
			continue;
		}
		fn = lookit->d_name;
		fn_len = strlen(fn);

		/* Don't check this and you get infinite recursion. ;) */
		if (strcmp(fn, ".") == 0) {
			continue;
		}
		if (strcmp(fn, "..") == 0) {
			continue;
		}

		/* Now we stick "fn" on the end of from_name, may undo below. */
		from_name_n += fn_len + 1;
		while (from_name_n > from_name_len) {
			UPSIZE_OR_DIE(from_name);
		}
		from_name[S_PEEK(from_off_stack) - 1] = '/';
		memcpy(from_name + S_PEEK(from_off_stack), fn, fn_len);
		from_name[from_name_n - 1] = '\0';
		S_PUSH(from_off_stack, from_name_n);

		DEBUG("visit %s\n", from_name);

		/* If d_type wasn't filled out, find out with lstat(). */
		if (lookit->d_type == DT_UNKNOWN) {
			if (lstat(from_name, &checkit)) {
				DIE("Can't stat %s (%s).\n", from_name, fn);
			}
			if (S_ISREG(checkit.st_mode)) {
				is_dir = 0;
			} else if (S_ISDIR(checkit.st_mode)) {
				is_dir = 1;
			} else {
				/* If it wasn't interesting then undo putting
				 * "fn" onto the end
				 * of from_name and go have a shot at the next
				 * entry. */
				S_POP(from_off_stack);
				continue;
			}
		}

		/* If we get here, file_name must be staring at either a file
		 * or a directory, because both the d_type and the lstat()
		 * checks continue on not-a-file-or-directory. */

		/* Handle directories by pushing them and descending
		 * immediately.
		 * Keeping the fn path segment on from_name makes opendir() and
		 * link() calls go into the decended-into directory. */
		if (is_dir) {
			S_PUSH(dir_stack, opendir(from_name));
			if (S_PEEK(dir_stack) == NULL) {
				DIE("Couldn't open %s (%s).\n", from_name, fn);
			}
			continue;
		}

		/* Directories handled above; must be looking at a file now. */

		/* Now tack fn onto the end of unto_name. Because unto_name has
		 * exactly
		 * one path component under it, we work out where the replacable
		 * slash
		 * should be from the length of the original dir name. No need
		 * to store
		 * that on a stack. */

		unto_name[unto_dir_len] = '/';
		unto_name_n = unto_dir_len + 1 + fn_len + 1;
		while (unto_name_n > unto_name_len) {
			UPSIZE_OR_DIE(unto_name);
		}
		memcpy(unto_name + unto_dir_len + 1, fn, fn_len);
		unto_name[unto_name_n - 1] = '\0';

		DEBUG("go go go %s\n", unto_name);

		/* EEXIST is expected - this program's idempotent. */
		/* Any other error signals a real problem. */

		errno = 0;
		link(from_name, unto_name);
		if ((errno != 0) && (errno != EEXIST)) {
			DIE("link() failed for wrong reason.\n");
		}

		/* Once we've used a file, take its name of the end of from_name
		 * to go
		 * back up into the directory prior to descending to the next
		 * file. */

		S_POP(from_off_stack);
	}

	return 0;
}
