#include <sys/types.h>
#include <sys/xattr.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

const char usage_message[] =
	"Usage: selcopy out_dir/ [filename]*\n"
	"Copies SELinux attributes from listed files\n"
	"to files with the same name in out_dir.\n"
	"\n"
	"This is meant for copying SELinux attributes over after copying\n"
	"filesystems with cpio. e.g.:\n"
	"\n"
	"cd /old_system\n"
	"find . -exec selcopy /new_filesystem/ {} +\n"
	"";

typedef struct {
	char *c;
	size_t len;
	size_t size;
} string;

static size_t
next_power_of_2(size_t x) {
	/* TODO: look up best way on bit-fiddling wiki. */
	if (x == 0) return 1;
	if (x == (x & (x >> 1))) return x;
	size_t y = x;
	size_t z = x >> 1;
	while (z) {
		y |= z;
		z >>= 1;
	}
	return y + 1;
}

static void
string_init(string *s)
{
	s->c = malloc(1);
	s->len = 0;
	s->size = 1;
	if (s->c == NULL) abort();
}

static void
string_expand(string *s, size_t request_size)
{
	size_t new_size = next_power_of_2(request_size);
	char *new_c = realloc(s->c, new_size);
	if (new_c == NULL)
		abort();
	s->c = new_c;
	s->size = new_size;
}

static void
string_append(string *s, char *source, size_t length) {
	ssize_t spare = s->size - s->len;
	if (length > spare)
		string_expand(s, s->size + length);
	memcpy(s->c + s->len, source, length);
	s->len += length;
}

static void
string_free(string *s) {
	free(s->c);
	s->c = NULL;
	s->len = 0;
	s->size = 0;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		fputs(usage_message, stderr);
		return 1;
	}

	string attr;
	string out_path;

	string_init(&attr);
	string_init(&out_path);

	size_t out_prefixlen = strlen(argv[1]);
	string_append(&out_path, argv[1], out_prefixlen);

	if (out_path.c[out_path.len - 1] != '/') {
		out_prefixlen++;
		string_append(&out_path, "/", 1);
	}

	int argv_pos;
	for (argv_pos=2; argv_pos < argc; argv_pos++) {
		out_path.len = out_prefixlen;

		char *suffix = argv[argv_pos];
		size_t suffixlen = strlen(suffix);

		string_append(&out_path, suffix, suffixlen + 1);

		ssize_t new_len;
		do {
			new_len = lgetxattr(
				suffix,
				"security.selinux",
				attr.c,
				attr.size);
			if (errno == ERANGE) {
				string_expand(&attr, attr.size << 1);
			} else {
				fprintf(stderr, "lgetxattr (%d)\n", errno);
				perror(suffix);
			}
		} while (new_len == -1);

		int setxattr_rv = lsetxattr(
			out_path.c,
			"security.selinux",
			attr.c,
			strlen(attr.c),
			0);
		if (setxattr_rv != 0) {
			fprintf(stderr, "lsetxattr (%d)\n", errno);
			perror(suffix);
			return 1;
		}
	}

	string_free(&out_path);
	string_free(&attr);
	return 0;
}
