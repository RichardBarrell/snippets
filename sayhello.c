/* cc -o sayhello.c '-DHELLO_ADDRESS="foo.example.com"' -DHELLO_PORT=4567
   sends a UDP packet containing this machine's hostname (and nothing else)
   to HELLO_ADDRESS:HELLO_PORT every five seconds.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#ifndef HELLO_ADDRESS
#define HELLO_ADDRESS "hello.example.com"
#endif
#ifndef HELLO_PORT
#define HELLO_PORT 1234
#endif

#define DIE_S2(s) #s
#define DIE_S1(s) DIE_S2(s)
#define DIE(...) do { int _errno = errno; fprintf(stderr, __VA_ARGS__); errno = _errno; perror("File: " __FILE__ "\nLine: " DIE_S1(__LINE__)); abort(); } while(0)
#define EH(LABEL, ...) do { if (errno == EINTR) { goto LABEL; } DIE(__VA_ARGS__); } while(0)

int main(int argc, char **argv) {
        ssize_t hostnamelen = 1;
        char *hostname = malloc(hostnamelen);

        if (hostname == NULL) { DIE("malloc hates me.\n"); }
        while (gethostname(hostname, hostnamelen)) {
                if (errno != ENAMETOOLONG) { DIE("gethostname fail.\n"); }
                hostnamelen <<= 1;
                free(hostname);
                hostname = malloc(hostnamelen);
                if (hostname == NULL) { DIE("malloc hates me.\n"); }
        }
        hostnamelen = strlen(hostname) + 1;

        socklen_t addrlen;
        struct sockaddr* addr;

        struct addrinfo ai_hints, *ai_lookup;
        memset(&ai_hints, 0, sizeof(ai_hints));
        ai_hints.ai_family = PF_INET;
        ai_hints.ai_socktype = SOCK_DGRAM;
        ai_hints.ai_flags = AI_NUMERICSERV;
        if (getaddrinfo("tigger", "8087", &ai_hints, &ai_lookup)) { DIE("Looking up 'tigger' failed.\n"); }
        addrlen = ai_lookup->ai_addrlen;
        addr = malloc((size_t)addrlen);
        if (addr == NULL) { DIE("malloc hates me.\n"); }
        memcpy(addr, ai_lookup->ai_addr, (size_t)addrlen);
        freeaddrinfo(ai_lookup);

        int s = socket(AF_INET, SOCK_DGRAM, 0);
        if (s == -1) { DIE("Can't get a udp4 socket.\n"); }

        for (;;) {
                ssize_t r = sendto(s, hostname, hostnamelen, 0, addr, addrlen);
                if (r != hostnamelen) { EH(stopped, "sendto returned %zd.\n", r); }
                if (sleep(5)) { EH(stopped, "nightmare\n"); }
        }
        stopped:
        free(addr);
        return 0;
}
