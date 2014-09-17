/*
To compile:

  gcc -o allfg allfg.c -std=c99 -Wall -Wextra \
      -DDEFAULT_CONFIG_FILE_NAME='"superisor.conf"'

To run:

  ./allfg

or

  ./allfg /path/to/supervisor.conf

This program reads a supervisor.conf file, finds all of the programs
in it and starts all of them right away. It leaves them all right in
the foreground so that hitting a pdb in any one of them will work.

Disclaimer: I've only ever tested this against the supervisord.conf
files that collective.recipe.supervisor outputs.

If you send SIGTERM (e.g. by pressing ctrl-C) to this program, this program
sends SIGTERM to all the processes. If one of the processes dies on its own,
this program sends SIGTERM to all the rest.

The end result is that this makes a supervisor's worth of processes
feel like one process, except for heating up your laptop more.
*/

#define _XOPEN_SOURCE 500

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#ifndef DEFAULT_CONFIG_FILE_NAME
#define DEFAULT_CONFIG_FILE_NAME "parts/supervisor/supervisord.conf"
#endif

#define CRY(...)                                                               \
	do {                                                                   \
		fprintf(stderr, "%s:%d (%s): ", __FILE__, __LINE__,            \
		        strerror(errno));                                      \
		fprintf(stderr, __VA_ARGS__);                                  \
	} while (0)
#define DOOMED()                                                               \
	do {                                                                   \
		CRY("allocation failure");                                     \
		abort();                                                       \
	} while (0)

sig_atomic_t i_am_stopping = 0;

void signal_stop_me_handler(int signo)
{
	if (signo == 0)
		signo = 1;
	i_am_stopping = signo;
}

static int open_config_file(char *config_file_name, int *out_fd,
                            char **out_text, size_t *out_sz)
{
	int fd = -1;
	struct stat stat;
	char *text = NULL;
	size_t sz = 0;
	int saved_errno;

	fd = open(config_file_name, O_RDONLY);
	if (fd < 0)
		goto open_config_file_error;
	if (fstat(fd, &stat))
		goto open_config_file_error;
	sz = stat.st_size;
	text = mmap(0, sz, PROT_READ, MAP_PRIVATE, fd, 0);
	if (text == NULL)
		goto open_config_file_error;

	*out_fd = fd;
	*out_text = text;
	*out_sz = sz;
	return 0;

open_config_file_error:
	saved_errno = errno;
	if (text != NULL)
		munmap(text, sz);
	if (fd > 0)
		close(fd);
	errno = saved_errno;
	return -1;
}

typedef void (*parse_line_callback)(char *, char *, void *);
static void parse_lines(char *text_start, char *text_end,
                        parse_line_callback do_line, void *do_line_state)
{
	char *line_start, *line_end;
	char *cursor;
	line_start = text_start;
	for (cursor = text_start; cursor < text_end; cursor++) {
		if (*cursor == '\n') {
			line_end = cursor;
			do_line(line_start, line_end, do_line_state);
			line_start = cursor + 1;
		}
	}
	if (line_start < text_end) {
		do_line(line_start, text_end, do_line_state);
	}
}

struct program_spec
{
	char *cmd, *cmd_end;
	char *name, *name_end;
	char *dir, *dir_end;
};

struct program_spec_state
{
	struct program_spec *specs;
	size_t specs_used;
	size_t specs_alloc;

	int in_program_section;
};

static int same_word(char *word, char *haystack_start, char *haystack_end)
{
	size_t word_len = strlen(word);
	size_t haystack_len = haystack_end - haystack_start;
	if (word_len != haystack_len)
		return 0;
	return 0 == memcmp(word, haystack_start, word_len);
}

static void alloc_more_specs(struct program_spec_state *s)
{
	if (s->specs_alloc == 0)
		s->specs_alloc = 1;
	else
		s->specs_alloc <<= 1;
	size_t new_sz = s->specs_alloc * sizeof s->specs[0];
	s->specs = realloc(s->specs, new_sz);
	if (s->specs == NULL)
		DOOMED();
}

static void parse_supervisor(char *line_start, char *line_end, void *v)
{
	struct program_spec_state *s = v;

	char *c = line_start;
	for (; c < line_end && isspace(*c); c++)
		;
	if (c == line_end)
		return;
	if (*c == '[') {
		char *kind_start = c + 1;
		for (; (c < line_end) && (*c != ':'); c++)
			;
		if (c == line_end)
			return;
		if (same_word("program:", kind_start, c + 1)) {
			s->in_program_section = 1;
			s->specs_used++;
			if (s->specs_used > s->specs_alloc) {
				alloc_more_specs(s);
			}
			struct program_spec *p = &s->specs[s->specs_used - 1];
			p->cmd = p->cmd_end = NULL;
			p->name = p->name_end = NULL;
			p->dir = p->dir_end = NULL;
		} else {
			s->in_program_section = 0;
		}
		return;
	}

	if (!s->in_program_section)
		return;

	struct program_spec *program = &s->specs[s->specs_used - 1];

	char *key_start = c;
	for (; (c < line_end) && !isspace(*c) && (*c != '='); c++)
		;
	if (c == line_end)
		return;
	char *key_end = c;
	for (; (c < line_end) && (*c != '='); c++)
		;
	c++;
	for (; (c < line_end) && isspace(*c); c++)
		;
	char *value_start = c;
	if (c == line_end)
		return;
	char *value_end = line_end;

	if (same_word("command", key_start, key_end)) {
		program->cmd = value_start;
		program->cmd_end = value_end;
	}
	if (same_word("process_name", key_start, key_end)) {
		program->name = value_start;
		program->name_end = value_end;
	}
	if (same_word("directory", key_start, key_end)) {
		program->dir = value_start;
		program->dir_end = value_end;
	}
}

int main(int argc, char **argv)
{
	if (argc > 2 || (argc == 2 && 0 == strcmp("--help", argv[1]))) {
		printf("Usage: %s [/path/to/supervisor.conf]\n"
		       "Runs every process in a supervisor.conf file.\n"
		       "Defaults to reading %s.\n",
		       argv[0], DEFAULT_CONFIG_FILE_NAME);
		return 1;
	}
	char *config_file_name = DEFAULT_CONFIG_FILE_NAME;
	if (argc == 2)
		config_file_name = argv[1];

	pid_t *children = NULL;
	pid_t terminated_pid;
	int terminated_status;

	int config_fd;
	char *config_text;
	char *config_text_end;
	size_t config_text_sz;

	if (open_config_file(config_file_name, &config_fd, &config_text,
	                     &config_text_sz)) {
		CRY("Can't open config file %s.", config_file_name);
		return 1;
	}
	config_text_end = config_text + config_text_sz;

	struct program_spec_state specs_state;
	memset(&specs_state, 0, sizeof specs_state);
	parse_lines(config_text, config_text_end, parse_supervisor,
	            &specs_state);

	struct program_spec *specs = specs_state.specs;
	size_t nr_programs = specs_state.specs_used;

#define PSLEN(p, NAME) ((int)(p->NAME##_end - p->NAME))
	size_t i;
	struct program_spec *p;

	for (i = 0; i < nr_programs; i++) {
		p = &specs[i];
		if (p->cmd == NULL) {
			CRY("Won't be able to run program with missing "
			    "command.\n");
			return 1;
		}
	}

	for (i = 0; i < nr_programs; i++) {
		p = &specs[i];
		printf("program: %.*s\n", PSLEN(p, name), p->name);
		printf("command: %.*s\n", PSLEN(p, cmd), p->cmd);
		printf("directory: %.*s\n\n", PSLEN(p, dir), p->dir);
	}

	struct sigaction stop_me_action;
	memset(&stop_me_action, 0, sizeof stop_me_action);
	stop_me_action.sa_handler = signal_stop_me_handler;
	stop_me_action.sa_flags = 0;
	sigemptyset(&stop_me_action.sa_mask);

	sigaction(SIGTERM, &stop_me_action, NULL);
	sigaction(SIGINT, &stop_me_action, NULL);
	sigaction(SIGHUP, &stop_me_action, NULL);

	children = calloc(nr_programs, sizeof *children);
	if (children == NULL)
		DOOMED();

	for (i = 0; i < nr_programs; i++) {
		p = &specs[i];
		pid_t child = fork();
		if (child < 0) {
			CRY("Can't fork for %.*s.\n", PSLEN(p, name), p->name);
			nr_programs = i;
			i_am_stopping = 1;
			break;
		}
		if (child == 0) {
			if (p->dir != NULL) {
				char *dir_nulled = malloc(1 + PSLEN(p, dir));
				if (dir_nulled == NULL)
					DOOMED();
				memcpy(dir_nulled, p->dir, PSLEN(p, dir));
				dir_nulled[PSLEN(p, dir)] = '\0';
				if (chdir(dir_nulled)) {
					CRY("Can't chdir to %s.\n", dir_nulled);
					return 1;
				}
			}

			char *cmd_nulled = malloc(1 + PSLEN(p, cmd));
			if (cmd_nulled == NULL)
				DOOMED();
			memcpy(cmd_nulled, p->cmd, PSLEN(p, cmd));
			cmd_nulled[PSLEN(p, cmd)] = '\0';

			if (execlp("sh", "sh", "-c", cmd_nulled, NULL)) {
				CRY("Can't exec sh -c %s\n", cmd_nulled);
				return 1;
			}
			DOOMED(); // can't get here
		} else {
			children[i] = child;
		}
	}

	free(specs);
	munmap(config_text, config_text_sz);
	close(config_fd);

	for (;;) {
		if (i_am_stopping) {
			for (i = 0; i < nr_programs; i++) {
				kill(children[i], SIGTERM);
			}
			usleep(1000 * 10);
		}

		int wopt = 0;
		for (;;) {
			terminated_pid = waitpid(-1, &terminated_status, wopt);
			if (errno == ECHILD)
				goto all_done_now_thanks;

			if (terminated_pid == -1)
				break;
			if (terminated_pid == 0)
				break;

			wopt = WNOHANG;
			i_am_stopping = 1;
			for (i = 0; i < nr_programs; i++) {
				if (terminated_pid == children[i]) {
					size_t last = nr_programs - 1;
					children[i] = children[last];
					nr_programs--;
					break;
				}
			}
		}
	}

all_done_now_thanks:
	free(children);
	printf("Taken down by %d.\n", i_am_stopping);
	return 0;
}
