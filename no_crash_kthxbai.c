/* This is a REALLY evil module that bumps RIP along by 1 on SIGILL
 * or SIGSEGV delivery. Turns many segfaults into infinite-ish loops. :)
 *
 * To compile:
 * gcc -fPIC -shared -o no_crash_kthxbai.so no_crash_kthxbai.c -Wall
 *
 * To use:
 * env LD_PRELOAD=./no_crash_kthxbai.so /some/other/program ...
 *
 */

#include <signal.h>
#include <sys/mman.h>
#include <unistd.h>

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void handle_inelegantly(int signum, siginfo_t *info, void *ucontext)
{
	ucontext_t *uc = (ucontext_t *)ucontext;
	greg_t *gregs = (greg_t *)&uc->uc_mcontext.gregs;
	gregs[16] += 1;
}

static void install_evil_signal_handler(void) __attribute__((constructor));
static void install_evil_signal_handler(void)
{
	stack_t ss;
	// I'll pretend to not care about leaking this one allocation.
	ss.ss_sp = malloc(SIGSTKSZ);
	if (ss.ss_sp == NULL) {
		perror("Warning, can't allocate alt stack.");
		return;
	}
	ss.ss_size = SIGSTKSZ;
	ss.ss_flags = 0;
	if (sigaltstack(&ss, NULL)) {
		perror("Warning, can't set alt stack.");
		return;
	}
	struct sigaction illh;
	memset(&illh, 0, sizeof(illh));
	illh.sa_sigaction = &handle_inelegantly;
	illh.sa_flags = SA_SIGINFO | SA_ONSTACK;
	sigemptyset(&illh.sa_mask);
	if (sigaction(SIGILL, &illh, NULL)) {
		perror("Warning, can't install SIGILL handler.\n");
		return;
	}
	if (sigaction(SIGSEGV, &illh, NULL)) {
		perror("Warning, can't install SIGSEGV handler.\n");
		return;
	}
}
