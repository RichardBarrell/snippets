/* This is a really sily C program that I'm using as a TCP-interfaced
   DB backend, for a really terrible program which I wrote in a language
   with sockets but no storage or FFI. */

/* to compile on Linux: */
/* cc last_message.c -o last_message -Os     */
/*                   -Wall -Wextra -std=c99  */
/*                   -lsqlite3 -lapr-1       */

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

#include <apr-1/apr.h>
#include <apr-1/apr_errno.h>
#include <apr-1/apr_pools.h>
#include <apr-1/apr_poll.h>

#include <sqlite3.h>

#define FAIL(...) do { fprintf(stderr, __VA_ARGS__); perror(" "); } while(0)

typedef struct per_client {
	int dummy;
} per_client;

static int print_usage(void)
{
	fprintf(stderr, "Usage: last_message [foo.db]\n");
	return 1;
}

static void apr_fail(apr_status_t apr_err)
{
	char error_str[256];
	memset(&error_str[0], 0, sizeof error_str);
	apr_strerror(apr_err, &error_str[0], sizeof error_str);
	error_str[255] = 0;
	fputs(error_str, stderr);
	fputc('\n', stderr);
}

#define APR_DO_OR_DIE(call) do { \
	apr_status_t err; \
	err = (call); \
	if (err) { \
		FAIL("%s\n", #call); \
		apr_fail(err); \
		goto die; \
	} \
	} while(0)

void do_client_state_machine(
    const apr_pollfd_t *s,
    apr_pollset_t *pollset)
{
	apr_pollset_remove(pollset, s);
	apr_pollfd_t s1;
	apr_pollset_add(pollset, &s1);
}

int main(int argc, const char * const *argv, const char * const *env)
{
	const char *db_filename = "last_message.db";
	int dying = 0;
	sqlite3 *sql = NULL;

	apr_socket_t *acc = NULL;
	apr_pollset_t *pollset = NULL;

	APR_DO_OR_DIE(apr_app_initialize(&argc, &argv, &env));
	atexit(&apr_terminate);

	apr_pool_t *pool;
	APR_DO_OR_DIE(apr_pool_create(&pool, NULL));

	if (argc > 2) { return print_usage(); }
	if (argc == 2) {
		if (strcmp(argv[1], "--help") == 0) { return print_usage(); }
		db_filename = argv[1];
	}
	int err;
	if ((err = sqlite3_open(db_filename, &sql)) != SQLITE_OK) {
		fprintf(stderr, "Can't open DB (%s): %s.\n",
			db_filename, sqlite3_errstr(err));
		return 1;
	}

	APR_DO_OR_DIE(apr_socket_create(&acc, APR_INET, SOCK_STREAM, 0, pool));
	/* APR_DO_OR_DIE(apr_socket_opt_set(acc, APR_SO_REUSEADDR, 1)); */
	apr_sockaddr_t *l_addr;
	APR_DO_OR_DIE(apr_sockaddr_info_get(&l_addr, NULL, APR_INET, 1066, 0, pool));
	APR_DO_OR_DIE(apr_socket_bind(acc, l_addr));
	APR_DO_OR_DIE(apr_socket_listen(acc, 8));

	apr_pollfd_t apr_accept_descr;
	memset(&apr_accept_descr, 0, sizeof apr_accept_descr);
	apr_accept_descr.p = pool;
	apr_accept_descr.desc_type = APR_POLL_SOCKET;
	apr_accept_descr.desc.s = acc;
	apr_accept_descr.reqevents = APR_POLLIN;
	apr_accept_descr.client_data = NULL;

	APR_DO_OR_DIE(apr_pollset_create(&pollset, 256, pool, 0));
	APR_DO_OR_DIE(apr_pollset_add(pollset, &apr_accept_descr));

	for (;;) {
		apr_int32_t signalled_len;
		const apr_pollfd_t *signalled;
		apr_status_t apr_poll_err = apr_pollset_poll(pollset, 0,
							     &signalled_len,
							     &signalled);
		if (apr_poll_err == APR_EINTR) { continue; }
		APR_DO_OR_DIE(apr_poll_err);

		for (apr_int32_t i = 0; i < signalled_len; i++) {
			const apr_pollfd_t *s = signalled + i;
			/* this is the acc socket */
			if (s->desc.s == acc) {
				apr_socket_t *client = NULL;
				apr_status_t apr_accept_err =
					apr_socket_accept(&client, acc, pool);
				if (apr_accept_err) {
				}
				struct per_client *ctx = malloc(sizeof ctx[0]);
				if (ctx == NULL) {
					apr_socket_close(client);
					fprintf(stderr, "malloc fail!\n");
					continue;
				}
				memset(ctx, 0, sizeof *ctx);
				apr_pollfd_t client_d;
				memset(&client_d, 0, sizeof client_d);
				client_d.p = pool;
				client_d.desc_type = APR_POLL_SOCKET;
				client_d.desc.s = client;
				client_d.reqevents = APR_POLLIN | APR_POLLOUT | APR_POLLERR | APR_POLLHUP;
				client_d.client_data = ctx;

				apr_status_t err;
				err = apr_pollset_add(pollset, &client_d);
				if (err != 0) {
					apr_fail(err);
					apr_socket_close(client);
				}
			} else {
				do_client_state_machine(s, pollset);
			}
		}
	}
	/* 	for (size_t i = 0; i < nfds; i++) { */
	/* 		if ((fds[i].fd == accept_sock) && */
	/* 		    (fds[i].revents & POLLIN)) { */
	/* 			int client = accept(accept_sock, NULL, NULL); */
	/* 			add_client_fd(client, &ctx); */
	/* 		} else if (fds[i].revents) { */
	/* 			/\* handle_client_fd(&fds[i], &ctx); *\/ */
	/* 		} */
	/* 	} */
	/* 	cleanup_client_fds(&ctx); */
	/* } */

	if (0) {
	die:
		dying = 1;
	}

	if (sqlite3_close(sql) != SQLITE_OK) {
		fprintf(stderr, "Error closing DB (%s): %s.\n",
			db_filename, sqlite3_errmsg(sql));
		return 1;
	}

	apr_pollset_destroy(pollset);
	apr_socket_close(acc);

	return dying;
}
