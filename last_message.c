/* This is a really sily C program that I'm using as a TCP-interfaced
   DB backend, for a really terrible program which I wrote in a language
   with sockets but no storage or FFI. */

/* to compile on Linux: */
/* cc last_message.c -o last_message -Os -Wall -Wextra -std=c99 -lsqlite3 */

#define _POSIX_C_SOURCE 199309L
#define _BSD_SOURCE

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>

#include <sqlite3.h>

#define FAIL(...) do { fprintf(stderr, __VA_ARGS__); perror(" "); } while(0)

int print_usage(void) {
	fprintf(stderr, "Usage: last_message [foo.db]\n");
	return 1;
}

struct remove_ll {
	int fd;
	struct remove_ll *next;
};

struct poll_context {
	struct pollfd **p_fds;
	size_t *p_nfds;
	size_t have_fds;
	struct remove_ll *to_remove;
};

void remove_client_fd(int fd, struct poll_context *c)
{
	struct remove_ll *remove = malloc(sizeof(struct remove_ll));
	remove->fd = fd;
	remove->next = c->to_remove;
	c->to_remove = remove;
}

void add_client_fd(int fd, struct poll_context *c)
{
	if (*c->p_nfds == c->have_fds) {
		size_t new_sz = sizeof(struct pollfd) * c->have_fds * 2;
		struct pollfd *new_fds = realloc(*c->p_fds, new_sz);
		if (new_fds == NULL) {
			FAIL("realloc()");
			close(fd);
			return;
		}
		c->have_fds *= 2;
		*c->p_fds = new_fds;
	}
	struct pollfd *pollfd = *c->p_fds + *c->p_nfds;
	(*c->p_nfds)++;
	pollfd->fd = fd;
	pollfd->events = POLLIN | POLLOUT | POLLERR | POLLHUP;
	pollfd->revents = 0;
}

void cleanup_client_fds(struct poll_context *c)
{
	struct pollfd *fds, *this_pollfd, *last_pollfd;
	fds = *c->p_fds;

	while (c->to_remove) {
		last_pollfd = fds + (*c->p_nfds - 1);
		struct remove_ll *removed = c->to_remove;
		this_pollfd = NULL;
		for (size_t i = 0; i < *c->p_nfds; i++) {
			if (fds[i].fd == removed->fd) {
				this_pollfd = &fds[i];
				break;
			}
		}
		if (this_pollfd == NULL) {
			FAIL("tried to remove missing fd %d.\n", removed->fd);
			abort();
		}

		if (this_pollfd != last_pollfd) {
			memcpy(this_pollfd, last_pollfd, sizeof fds[0]);
		}

		c->to_remove = removed->next;
		(*c->p_nfds)--;
		free(removed);
	}
}

void handle_client_fd(struct pollfd *pollfd, struct poll_context *c)
{
	int fd = pollfd->fd;
	send(fd, "hello\n", 7, 0);
	close(fd);
	remove_client_fd(fd, c);
}

int main(int argc, char **argv)
{
	int dying = 0;

	const char *db_filename = "last_message.db";
	if (argc > 2) { return print_usage(); }
	if (argc == 2) {
		if (strcmp(argv[1], "--help") == 0) { return print_usage(); }
		db_filename = argv[1];
	}
	sqlite3 *sql = NULL;
	int err;
	if ((err = sqlite3_open(db_filename, &sql)) != SQLITE_OK) {
		fprintf(stderr, "Can't open DB (%s): %s.\n",
			db_filename, sqlite3_errstr(err));
		return 1;
	}

	int accept_sock;
	accept_sock = socket(AF_INET, SOCK_STREAM, 0);
	if (accept_sock == -1) { FAIL("socket()"); goto die; }

	const int one = 1;
	/* Don't care if this fails. */
	setsockopt(accept_sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);

	struct sockaddr_in bind_to;
	memset(&bind_to, 0, sizeof bind_to);
	bind_to.sin_family = AF_INET;
	bind_to.sin_port = htons(1066);
	bind_to.sin_addr.s_addr = htonl(0); /* 0.0.0.0, please */
	if (bind(accept_sock, (struct sockaddr *)&bind_to, sizeof bind_to)) {
		FAIL("bind()"); goto die;
	}
	if (listen(accept_sock, 8)) {
		FAIL("listen()"); goto die;
	}

	struct pollfd *fds = malloc(sizeof(struct pollfd));
	size_t nfds = 1;

	fds[0].fd = accept_sock;
	fds[0].events = POLLIN;
	fds[0].revents = 0;

	struct poll_context ctx;
	ctx.p_fds = &fds;
	ctx.p_nfds = &nfds;
	ctx.have_fds = 1;
	ctx.to_remove = NULL;

	for (;;) {
		printf("%zd\n", nfds);
		poll(fds, nfds, -1);
		if (errno == EINTR) { continue; }
		else if (errno) {
			FAIL("poll()"); goto die;
		}
		for (size_t i = 0; i < nfds; i++) {
			if ((fds[i].fd == accept_sock) &&
			    (fds[i].revents & POLLIN)) {
				int client = accept(accept_sock, NULL, NULL);
				add_client_fd(client, &ctx);
			} else if (fds[i].revents) {
				handle_client_fd(&fds[i], &ctx);
			}
		}
		cleanup_client_fds(&ctx);
	}

	free(fds);

	if (0) {
	die:
		dying = 1;
	}

	if (sqlite3_close(sql) != SQLITE_OK) {
		fprintf(stderr, "Error closing DB (%s): %s.\n",
			db_filename, sqlite3_errmsg(sql));
		return 1;
	}
	return dying;
}
