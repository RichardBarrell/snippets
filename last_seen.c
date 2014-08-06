/* This is a really silly C program that I'm using as a TCP interface       */
/* to gdbm for a really terrible program which I wrote, which happens to    */
/* have sockets but no DB libraries, nor any real FFI.                      */

/* to compile on Linux: */
/* cc -lpthread -lgdbm -O2 -Wall -Wextra -std=c99 last_seen.c -o last_seen  */
/* on FreeBSD, install databases/gdbm and add:                              */
/*   -I/usr/local/include                                                   */
/*   -L/usr/local/lib                                                       */

/* connect to it on TCP port 1070                                           */
/* send a message 'jfoo', it'll store NOW as the 'j' time for the key 'foo' */
/* it'll send back ":)              "                                       */

/* send a message 'wfoo', it'll store NOW as the 'w' time for 'foo'         */
/*                    and it'll store NOW as the 'j' time for 'foo'         */
/* it'll send back ":)              "                                       */

/* send a message 'qfoo', it'll send back a 16-byte message.                */
/*  first 8 bytes are the big-endian 'j' time for 'foo'                     */
/*  second 8 bytes are the big-endian 'w' time for 'foo'                    */
/* Timestamps are Unix time_t values. Assume UTC.                           */

#define _POSIX_C_SOURCE 199309L
#define _BSD_SOURCE

#include <gdbm.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>

#ifdef __FreeBSD__
#include <sys/endian.h>
#else
#include <endian.h>
#endif

#define FAIL(...)                                                              \
	do {                                                                   \
		fprintf(stderr, __VA_ARGS__);                                  \
		perror(" ");                                                   \
	} while (0)

time_t get_now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	return ts.tv_sec;
}

struct joincontext
{
	sem_t read;
	sem_t write;
	pthread_t joinme;
};
void *thread_joiner(void *arg)
{
	struct joincontext *j = (struct joincontext *)arg;
	for (;;) {
		if (sem_wait(&j->read)) {
			FAIL("sem_wait");
			abort();
		}
		pthread_t joinme = j->joinme;
		if (sem_post(&j->write)) {
			FAIL("sem_post");
			abort();
		}
		pthread_join(joinme, NULL);
	}
	return NULL;
}

struct context
{
	struct joincontext *j;
	struct sockaddr_storage addr;
	socklen_t addrlen;
	int fd;
	pthread_mutex_t *dblock;
	GDBM_FILE db;
	char buf[4096];
};
void *per_client(void *arg)
{
	struct context *ctx = (struct context *)arg;
	char *buf = ctx->buf;
	datum key, value;
	char response[16];

	ssize_t bytes = recv(ctx->fd, buf, 4096, 0);
	if (bytes <= 1) {
		FAIL("recv");
		goto die;
	}

	key.dptr = buf;
	key.dsize = bytes;

	if (buf[0] == 'q') {
		pthread_mutex_lock(ctx->dblock);
		buf[0] = 'j';
		value = gdbm_fetch(ctx->db, key);
		if (value.dptr == NULL) {
			memset(response + 0, 0, 8);
		} else {
			if (value.dsize != 8) {
				FAIL("gdbm_fetch j bad len %d", value.dsize);
				free(value.dptr);
				goto die;
			}
			memcpy(response + 0, value.dptr, 8);
			free(value.dptr);
		}

		buf[0] = 'w';
		value = gdbm_fetch(ctx->db, key);
		if (value.dptr == NULL) {
			memset(response + 8, 0, 8);
		} else {
			if (value.dsize != 8) {
				FAIL("gdbm_fetch w bad len %d", value.dsize);
				free(value.dptr);
				goto die;
			}
			memcpy(response + 8, value.dptr, 8);
			free(value.dptr);
		}
		pthread_mutex_unlock(ctx->dblock);
	} else if ((buf[0] == 'w') || (buf[0] == 'j')) {
		uint64_t now = htobe64((uint64_t)get_now());
		value.dptr = (char *)&now;
		value.dsize = sizeof(now);

		pthread_mutex_lock(ctx->dblock);
		if (buf[0] == 'w') {
			if (gdbm_store(ctx->db, key, value, GDBM_REPLACE)) {
				FAIL("gdbm_store w");
			}
		}
		buf[0] = 'j';
		if (gdbm_store(ctx->db, key, value, GDBM_REPLACE)) {
			FAIL("gdbm_store j");
		}
		pthread_mutex_unlock(ctx->dblock);
		memcpy(response, ":)              ", 16);
	}

	size_t sent = 0;
	ssize_t last_sent = 0;
	while (sent < 16) {
		last_sent = send(ctx->fd, response + sent, 16 - sent, 0);
		if (last_sent < 0) {
			FAIL("send to client");
			goto die;
		}
		sent += last_sent;
	}

die:
	close(ctx->fd);

	struct joincontext *j = ctx->j;
	if (sem_wait(&j->write)) {
		FAIL("sem_wait");
		abort();
	}
	j->joinme = pthread_self();
	if (sem_post(&j->read)) {
		FAIL("sem_post");
		abort();
	}
	free(ctx);
	return NULL;
}

int main(void)
{
	pthread_mutex_t dblock;
	if (pthread_mutex_init(&dblock, NULL)) {
		FAIL("mutex_init");
		return 1;
	}
	GDBM_FILE db = gdbm_open("last_seen.db", 4096, GDBM_WRCREAT, 0644, 0);

	int acc = socket(AF_INET, SOCK_STREAM, 0);
	if (acc < 0) {
		FAIL("socket");
		return 1;
	}
	int one = 1;
	if (setsockopt(acc, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one))) {
		FAIL("setsockopt(SO_REUSEADDR");
		return 1;
	}
	struct sockaddr_in bind_addr;
	memset(&bind_addr, 0, sizeof(bind_addr));
	bind_addr.sin_family = AF_INET;
	bind_addr.sin_port = htons(1070);
	bind_addr.sin_addr.s_addr = htonl(0); /* 0.0.0.0, please */
	if (bind(acc, (struct sockaddr *)&bind_addr, sizeof(bind_addr))) {
		FAIL("bind");
		return 1;
	}
	if (listen(acc, 8)) {
		FAIL("listen");
		return 1;
	}

	pthread_t joiner;
	struct joincontext j;
	if (sem_init(&j.read, 0, 0)) {
		FAIL("sem_init");
		return 1;
	}
	if (sem_init(&j.write, 0, 1)) {
		FAIL("sem_init");
		return 1;
	}
	if (pthread_create(&joiner, NULL, thread_joiner, &j)) {
		FAIL("pthread_create joiner");
		return 1;
	}

	for (;;) {
		struct sockaddr_storage addr;
		socklen_t addrlen = sizeof(addr);
		int client = accept(acc, (struct sockaddr *)&addr, &addrlen);
		if (client < 0) {
			FAIL("accept");
			continue;
		}
		struct context *ctx = malloc(sizeof(struct context));
		if (ctx == NULL) {
			FAIL("malloc");
			continue;
		}
		ctx->j = &j;
		memcpy(&ctx->addr, &addr, (size_t)addrlen);
		ctx->addrlen = addrlen;
		ctx->fd = client;
		ctx->db = db;
		ctx->dblock = &dblock;

		pthread_t t;
		if (pthread_create(&t, NULL, per_client, ctx)) {
			FAIL("pthread_create per_client");
			free(ctx);
			continue;
		}
	}
	pthread_mutex_lock(&dblock);
	gdbm_close(db);
	pthread_mutex_unlock(&dblock);
	pthread_mutex_destroy(&dblock);
	sem_destroy(&j.read);
	sem_destroy(&j.write);
	pthread_join(joiner, NULL);
	return 0;
}
