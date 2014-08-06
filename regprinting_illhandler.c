/* This is a really evil C program that installs a SIGILL handler in order to
   make the
   "opcodes" 0x06 and 0x07 (which normally just make your program crash) dump
   the
   current CPU registers on stderr.

   This blithely assumes that nothing else is going to try to use SIGILL for
   anything,
   which is obviously true because only a LUNATIC would invoke SIGILL on
   purpose, right?

   There's lots of unportable crap in here.
   Seriously do not even begin to hope for this to work on anything other
   than Linux on an amd64 CPU in 64-bit mode without selinux enabled.
   Also, all of this code takes fantastic liberties with your stderr.

   To use:
   regprinting_illhandler_install(); // in main(), then:

   asm(".byte 0x06"); // or...
   asm(".byte 0x07");

   0x06 will trigger every time the CPU hits it.
   0x07 will trigger just the first time it is hit.

   To use this in gdb without it always grabbing the signal from you, type
   "handle SIGILL pass noprint nostop" at the gdb prompt and hope like Heck
   that nothing ever goes wrong.

   To compile the demo/test here:
   gcc -o regprinting_illhandler_demo regprinting_illhandler.c -DILL_DEMO

   To compile to link into other code:
   gcc -c regprinting_illhandler.c -o regprinting_illhandler.o
*/

#include "regprinting_illhandler.h"
#include <signal.h>
#include <sys/mman.h>
#include <unistd.h>

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void regprinting_illhandler(int signum, siginfo_t *info, void *ucontext)
{
	ucontext_t *uc = (ucontext_t *)ucontext;
	greg_t *gregs = (greg_t *)&uc->uc_mcontext.gregs;

	char regdump[1024]; // more than enough, format is fixed-size.
	size_t regdumpsz = snprintf(
	    regdump, 1024, "rax      = %#018lx\n"
	                   "rbx      = %#018lx\n"
	                   "rcx      = %#018lx\n"
	                   "rdx      = %#018lx\n"
	                   "rsi      = %#018lx\n"
	                   "rdi      = %#018lx\n"
	                   "rbp      = %#018lx\n"
	                   "rsp      = %#018lx\n"
	                   "r8       = %#018lx\n"
	                   "r9       = %#018lx\n"
	                   "r10      = %#018lx\n"
	                   "r11      = %#018lx\n"
	                   "r12      = %#018lx\n"
	                   "r13      = %#018lx\n"
	                   "r14      = %#018lx\n"
	                   "r15      = %#018lx\n"
	                   "rip      = %#018lx\n"
	                   "efl      = %#018lx\n"
	                   "CGGSFS00 = %#018lx\n"
	                   "err      = %#018lx\n"
	                   "trapno   = %#018lx\n"
	                   "oldmask  = %#018lx\n"
	                   "cr       = %#018lx\n",
	    gregs[13], gregs[11], gregs[14], gregs[12], gregs[9], gregs[8],
	    gregs[10], gregs[15], gregs[0], gregs[1], gregs[2], gregs[3],
	    gregs[4], gregs[5], gregs[6], gregs[7], gregs[16], gregs[17],
	    gregs[18], gregs[19], gregs[20], gregs[21], gregs[22]);

	/* "Impossible" for this to fail, format is fixed-size. Check anyway. ;P
	 */
	if (regdumpsz < 1024) {
		/* I call write(2, ...) and don't check the result because there
		 * isn't anywhere
		 * sensible to which I could log a failure to write to stderr
		 * anyway, and
		 * calling abort() would be impolite. */
		write(2, regdump, regdumpsz);
	}

	char *manip = info->si_addr;
	if (*manip == 0x06) {
		/* What we're doing here is incrementing the instruction
		 * pointer. When the signal
		 * handler returns, control resumes at the next, probably legal,
		 * instruction. */
		gregs[16] += 1;
		const char skipped[] = "Skipped 0x06.\n";
		write(2, skipped, sizeof(skipped) - 1);
	} else if (*manip == 0x07) {
		/* This makes many unportable assumptions about how memory
		 * works.
		 * Monkey-patches the 0x07 and replaces it with a NOP instead.
		 */
		intptr_t page = (intptr_t)info->si_addr;
		intptr_t pagesz = 4096;
		page = page & ~(pagesz - 1);
		void *ppage = (void *)page;
		if (mprotect(ppage, (size_t)pagesz,
		             PROT_READ | PROT_WRITE | PROT_EXEC)) {
			const char mprotect_write_fail[] =
			    "Can't make page writable.\n";
			write(2, mprotect_write_fail,
			      sizeof(mprotect_write_fail) - 1);
		}
		*manip = 0x90;
		if (mprotect(ppage, (size_t)pagesz, PROT_READ | PROT_EXEC)) {
			const char mprotect_unwrite_fail[] =
			    "Can't make page unwritable.\n";
			write(2, mprotect_unwrite_fail,
			      sizeof(mprotect_unwrite_fail) - 1);
		}
		const char patched[] = "Patched 0x07.\n";
		write(2, patched, sizeof(patched) - 1);
	} else {
		/* Not one of our two magic bytes? Must have caught a
		 * non-deliberate ILL! */
		abort();
	}
}

int regprinting_illhandler_install()
{
	stack_t ss;
	ss.ss_sp = malloc(SIGSTKSZ);
	if (ss.ss_sp == NULL) {
		perror("Warning, can't allocate alt stack.");
		return -1;
	}
	ss.ss_size = SIGSTKSZ;
	ss.ss_flags = 0;
	if (sigaltstack(&ss, NULL)) {
		perror("Warning, can't set alt stack.");
		return -1;
	}
	struct sigaction illh;
	memset(&illh, 0, sizeof(illh));
	illh.sa_sigaction = &regprinting_illhandler;
	illh.sa_flags = SA_SIGINFO | SA_ONSTACK;
	sigemptyset(&illh.sa_mask);
	if (sigaction(SIGILL, &illh, NULL)) {
		perror("Warning, can't install illh.\n");
		return -1;
	}
	return 0;
}

#ifdef ILL_DEMO
static int add(int a, int b)
{
	int c;
	asm("movl %1, %%eax;"
	    "movl %2, %%ebx;"
	    "addl %%ebx, %%eax;"
	    "movl %%eax, %0;"
	    ".byte 0x07;"
	    ".byte 0x06;"
	    : "=r"(c)
	    : "r"(a), "r"(b)
	    : "eax", "ebx");
	return c;
}

void main()
{
	regprinting_illhandler_install();
	printf("1 + 2 = %d\n", add(1, 2));
	printf("6 + 1 = %d\n", add(6, 1));
	printf("9 + 5 = %d\n", add(9, 5));
	printf("2 + 8 = %d\n", add(2, 8));
}
#endif
