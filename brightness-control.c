/* I know, it sucks. I was in a hurry. */

#include <stdio.h>
#include <stdlib.h>

static void print_usage()
{
	puts("brightness-control # to print brightness");
	puts("brightness-control [+-]?(0-100) # to set brightness");
}

static int read_int_file(char *name, int *out)
{
	int rv = 0;
	FILE *f;
	f = fopen(name, "rb");
	if (f == NULL) {
		return -1;
	}
	if (fscanf(f, "%d", out) != 1) {
		rv = -1;
	}
	fclose(f);
	return rv;
}

static int write_int_file(char *name, int val)
{
	int rv = 0;
	FILE *f;
	f = fopen(name, "wb");
	if (f == NULL) {
		return -1;
	}
	if (fprintf(f, "%d", val) == 0) {
		rv = -1;
	}
	fclose(f);
	return rv;
}

enum direction {
	GOING_DOWN,
	GOING_UP,
	SETTING
};

int main(int argc, char **argv)
{
	int max, cur, pc, newpc, level;
	enum direction direction;
	if (read_int_file("/sys/class/backlight/gmux_backlight/max_brightness",
	                  &max)) {
		fprintf(stderr, "Can't read max brightness.\n");
		return 1;
	}
	if (read_int_file("/sys/class/backlight/gmux_backlight/brightness",
	                  &cur)) {
		fprintf(stderr, "Can't read current brightness.\n");
		return 1;
	}
	pc = (int)(100.0 * (double)cur / (double)max);
	if (argc == 1) {
		printf("Brightness at %d%%.\n", pc);
		return 0;
	}
	if (argc > 2) {
		print_usage();
		return 1;
	}

	char *read_int_from = argv[1];
	if (argv[1][0] == '+') {
		read_int_from++;
		direction = GOING_UP;
	} else if (argv[1][0] == '-') {
		read_int_from++;
		direction = GOING_DOWN;
	} else {
		direction = SETTING;
	}

	if (sscanf(read_int_from, "%d", &newpc) != 1) {
		print_usage();
		return 1;
	}

	switch (direction) {
	case GOING_UP:
		newpc = pc + newpc;
		break;
	case GOING_DOWN:
		newpc = pc - newpc;
		break;
	case SETTING:
		if ((newpc < 0) || (newpc > 100)) {
			fprintf(stderr,
			        "Brightness should be between 0 and 100.\n");
			return 1;
		}
		break;
	}

	if (newpc > 100) {
		newpc = 100;
	}
	if (newpc < 0) {
		newpc = 0;
	}

	level = (int)((double)newpc / 100.0 * (double)max);
	if (write_int_file("/sys/class/backlight/gmux_backlight/brightness",
	                   level)) {
		fprintf(stderr, "Can't write brightness.\n");
		return 1;
	}

	printf("Set brightness to %d%%.\n", newpc);
	return 0;
}
