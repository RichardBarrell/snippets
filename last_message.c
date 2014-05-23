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

const char LM_SERVER_HI[4] = "L\x00\x00\n";

#define FAIL(...) do { fprintf(stderr, __VA_ARGS__); perror(" "); } while(0)

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

static void apr_maybe_fail(apr_status_t apr_err) {
	if (apr_err != 0)
		apr_fail(apr_err);
}

#define APR_DO_OR_DIE(call) do { \
	apr_status_t err; \
	err = (call); \
	if (err) { \
		FAIL("apr: %s\n", #call); \
		apr_fail(err); \
		goto die; \
	} \
	} while(0)

size_t next_power_of_two(size_t v)
{
	size_t shift;

	v--;
	for (shift=1; shift<(8/2 * sizeof v); shift <<= 1) {
		v |= v >> shift;
	}
	v++;

	return v;
}

typedef struct {
	char *buf;
	size_t size, used;
} byte_buffer;

void byte_buffer_init(byte_buffer *b)
{
	b->buf = NULL;
	b->size = 0;
	b->used = 0;
}

int byte_buffer_grow_to(byte_buffer *b, size_t desired_size)
{
	if (desired_size <= b->size) {
		return 0;
	}
	size_t new_size = next_power_of_two(desired_size);

	if ((desired_size > 0) && (new_size == 0))
		return -1;

	char *new_buf = realloc(b->buf, new_size);
	if (new_buf == NULL)
		return -1;

	b->buf = new_buf;
	b->size = new_size;
	return 0;
}

int byte_buffer_grow(byte_buffer *b, size_t more)
{
	return byte_buffer_grow_to(b, b->used + more);
}

void byte_buffer_free(byte_buffer *b)
{
	free(b->buf);
	b->buf = NULL;
	b->size = 0;
	b->used = 0;
}

typedef struct lmSQL {
	sqlite3 *sql;
	sqlite3_stmt *put;
	sqlite3_stmt *get;
	sqlite3_stmt *del;
} lmSQL;

typedef enum per_client_state {
	LM_S_INIT_CLIENT,
	LM_S_SEND_HI,
	LM_S_GET_QUERY,
	LM_S_SEND_REPLY,
	LM_S_CLOSING,
} per_client_state;

typedef struct per_client {
	per_client_state state;
	apr_size_t hi_bytes_sent;
	byte_buffer query;
	byte_buffer reply;
	lmSQL *lmdb;
} per_client;

typedef struct bytes {
	char *start;
	char *end;
} bytes;

int bytes_start_with(const char *needle, bytes haystack)
{
	size_t len = haystack.end - haystack.start;
	if (len < strlen(needle)) { return 0; }
	return (0 == strncmp(needle, haystack.start, strlen(needle)));
}

char *bytes_find_delimiter(char delimiter, bytes haystack)
{
	size_t len = haystack.end - haystack.start;
	return memchr(haystack.start, delimiter, len);
}

static void stamp_INVALID(byte_buffer *reply)
{
	memcpy(reply->buf, "INVALID", strlen("INVALID"));
}

static int do_client_query(lmSQL *lmdb, bytes query, byte_buffer *reply)
{
	reply->used = 0;
	if (byte_buffer_grow_to(reply, strlen("INVALID"))) {
		FAIL("can't grow reply buffer");
		return -1;
	}

	if (bytes_start_with("PUT ", query)) {
		bytes name;
		name.start = query.start + strlen("PUT ");
		name.end = query.end;
		name.end = bytes_find_delimiter(' ', name);
		if ((name.end == NULL) || (name.end == name.start)) {
			stamp_INVALID(reply);
			return 0;
		}
		bytes message;
		message.start = name.end + 1;
		message.end = query.end;
		if (message.end < message.start) {
			FAIL("parsed past end of query!?\n");
			abort();
		}

		abort(); // TODO
	} else if (bytes_start_with("GET ", query)) {
		abort(); // TODO
	} else if (bytes_start_with("TAKE ", query)) {
		abort(); // TODO
	} else {
		memcpy(reply->buf, "INVALID", strlen("INVALID"));
		return 0;
	}
}

void do_client_state_machine(const apr_pollfd_t *s, apr_pollset_t *pollset)
{
	struct per_client *c = s->client_data;
	apr_socket_t *client = s->desc.s;
	per_client_state old_state = c->state, new_state;
	apr_int16_t old_reqevents = s->reqevents, new_reqevents;

	switch (old_state) {
	case LM_S_INIT_CLIENT: {
		new_state = LM_S_SEND_HI;
		new_reqevents = APR_POLLOUT | APR_POLLHUP | APR_POLLERR;
		c->hi_bytes_sent = 0;
		byte_buffer_init(&c->query);
		byte_buffer_init(&c->reply);
		break;
	}
	case LM_S_SEND_HI: {
		apr_size_t send_sz = (sizeof LM_SERVER_HI) - c->hi_bytes_sent;
		apr_status_t send_err;
		send_err = apr_socket_send(client, LM_SERVER_HI, &send_sz);
		if (send_err && !(APR_STATUS_IS_EAGAIN(send_err))) {
			apr_fail(send_err);
			new_state = LM_S_CLOSING;
			break;
		}
		c->hi_bytes_sent += send_sz;
		if (c->hi_bytes_sent == (sizeof LM_SERVER_HI)) {
			new_state = LM_S_GET_QUERY;
			new_reqevents = APR_POLLIN | APR_POLLHUP | APR_POLLERR;
		} else {
			new_state = LM_S_GET_QUERY;
		}
		break;
	}
	case LM_S_GET_QUERY: {
		byte_buffer *q = &c->query;
		apr_status_t recv_err;
		size_t bigger = q->size + 64;
		if (q->used > bigger) {
			if (byte_buffer_grow_to(&c->query, bigger)) {
				FAIL("can't grow receive buffer\n");
				new_state = LM_S_CLOSING;
				break;
			}
		}
		char *put_bytes_here = q->buf + q->used;
		apr_size_t bytes_read = q->size - q->used;
		recv_err = apr_socket_recv(client, put_bytes_here, &bytes_read);
		if (recv_err) {
			abort(); // TODO
		}
		if (bytes_read == 0) {
			new_state = LM_S_CLOSING;
			break;
		}
		q->used += bytes_read;

		char *null_here = memchr(q->buf, '\x00', q->used);
		if (null_here) {
			new_state = LM_S_SEND_REPLY;
			q->used = null_here - q->buf;
			bytes query_bytes;
			query_bytes.start = c->query.buf;
			query_bytes.end = null_here;
			do_client_query(c->lmdb, query_bytes, &c->reply);
		} else {
			new_state = LM_S_GET_QUERY;
		}
		break;
	}
	case LM_S_SEND_REPLY: {
		abort(); // TODO
		break;
	}
	default: {
		FAIL("Invalid client state.\n");
		abort();
		break;
	}
	}

	if (new_state == LM_S_CLOSING) {
		apr_pollset_remove(pollset, s);
		apr_socket_close(s->desc.s);
		byte_buffer_free(&c->query);
		byte_buffer_free(&c->reply);
	} else if (old_reqevents != new_reqevents) {
		apr_pollfd_t s1;
		memset(&s1, 0, sizeof s1);
		s1.p = s->p;
		s1.client_data = s->client_data;
		s1.desc_type = s->desc_type;
		s1.desc.s = s->desc.s;
		s1.reqevents = new_reqevents;

		apr_pollset_remove(pollset, s);
		apr_pollset_add(pollset, &s1);
	}
}

void do_client_accept(
 apr_socket_t *acc,
 apr_pollset_t *pollset,
 apr_pool_t *pool,
 lmSQL *lmdb)
{
	apr_socket_t *client = NULL;
	apr_status_t acc_err = apr_socket_accept(&client, acc, pool);
	if (acc_err) {
		apr_fail(acc_err);
		return;
	}

	apr_status_t opt_err = apr_socket_opt_set(client, APR_SO_NONBLOCK, 1);
	if (opt_err) {
		apr_fail(opt_err);
		apr_maybe_fail(apr_socket_close(client));
		return;
	}

	struct per_client *ctx = malloc(sizeof *ctx);
	if (ctx == NULL) {
		apr_maybe_fail(apr_socket_close(client));
		FAIL("can't malloc ctx!\n");
		return;
	}

	ctx->state = LM_S_INIT_CLIENT;
	ctx->lmdb = lmdb;

	apr_pollfd_t fake_s;
	memset(&fake_s, 0, sizeof fake_s);
	fake_s.p = pool;
	fake_s.desc_type = APR_POLL_SOCKET;
	fake_s.desc.s = client;
	fake_s.reqevents = 0;
	fake_s.client_data = ctx;

	do_client_state_machine(&fake_s, pollset);
}

int main(int argc, const char * const *argv, const char * const *env)
{
	const char *db_filename = "last_message.db";
	int dying = 0;
	sqlite3 *sql = NULL;
	lmSQL lmdb = { 0, 0, 0, 0 };

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

	char *rc_msg;
	const char *CREATE_MESSAGE_TABLES =
		"CREATE TABLE IF NOT EXISTS messages ("
		"  msgid INTEGER PRIMARY KEY NOT NULL,"
		"  name BLOB NOT NULL,"
		"  left INTEGER NOT NULL,"
		"  delivered INTEGER,"
		"  message BLOB NOT NULL"
		");";
	int rc = sqlite3_exec(sql, CREATE_MESSAGE_TABLES, NULL, NULL, &rc_msg);
	if (rc != SQLITE_OK) {
		FAIL("Can't create messages table: %s.\n", rc_msg);
		sqlite3_close(sql);
		return 1;
	}

	lmdb.sql = sql;

#define LM_SQLITE_PREP(thing, statement) do { int prep = sqlite3_prepare_v2(sql, (statement), -1, (thing), NULL); if (prep != SQLITE_OK) { FAIL("SQL compilation error: (%s) while compiling (%s).\n", sqlite3_errmsg(sql), statement); goto die; } } while(0)

	LM_SQLITE_PREP(&lmdb.put,
		"INSERT INTO messages (name, left, message) "
		"VALUES (?, ?, ?);"
	);
	LM_SQLITE_PREP(&lmdb.get,
		"SELECT name, left, message "
		"FROM messages "
		"WHERE name = ? "
		"ORDER BY msgid ASC;"
	);
	LM_SQLITE_PREP(&lmdb.del,
		"DELETE FROM messages WHERE name = ?;"
	);

	APR_DO_OR_DIE(apr_socket_create(&acc, APR_INET, SOCK_STREAM, 0, pool));
	/* APR_DO_OR_DIE(apr_socket_opt_set(acc, APR_SO_REUSEADDR, 1)); */
	apr_sockaddr_t *l_addr;
	APR_DO_OR_DIE(apr_sockaddr_info_get(&l_addr, NULL, APR_INET, 1066, 0, pool));
	APR_DO_OR_DIE(apr_socket_bind(acc, l_addr));
	APR_DO_OR_DIE(apr_socket_listen(acc, 8));

	apr_pollfd_t apr_accept_desc;
	memset(&apr_accept_desc, 0, sizeof apr_accept_desc);
	apr_accept_desc.p = pool;
	apr_accept_desc.desc_type = APR_POLL_SOCKET;
	apr_accept_desc.desc.s = acc;
	apr_accept_desc.reqevents = APR_POLLIN;
	apr_accept_desc.client_data = NULL;

	APR_DO_OR_DIE(apr_pollset_create(&pollset, 256, pool, 0));
	APR_DO_OR_DIE(apr_pollset_add(pollset, &apr_accept_desc));

	for (;;) {
		apr_int32_t signalled_len;
		const apr_pollfd_t *signalled;
		apr_status_t poll_err = apr_pollset_poll(pollset, -1,
							 &signalled_len,
							 &signalled);
		if (poll_err == APR_EINTR) { continue; }
		APR_DO_OR_DIE(poll_err);

		for (apr_int32_t i = 0; i < signalled_len; i++) {
			const apr_pollfd_t *s = signalled + i;
			/* this is the acc socket */
			if (s->desc.s == acc) {
				do_client_accept(acc, pollset, pool, &lmdb);
			} else {
				do_client_state_machine(s, pollset);
			}
		}
	}

	if (0) {
	die:
		dying = 1;
	}

	sqlite3_finalize(lmdb.put); lmdb.put = 0;
	sqlite3_finalize(lmdb.get); lmdb.get = 0;
	sqlite3_finalize(lmdb.del); lmdb.del = 0;

	if (sqlite3_close(sql) != SQLITE_OK) {
		fprintf(stderr, "Error closing DB (%s): %s.\n",
			db_filename, sqlite3_errmsg(sql));
		return 1;
	}

	apr_pollset_destroy(pollset);
	apr_socket_close(acc);

	return dying;
}
