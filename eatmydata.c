/* gcc -fPIC -shared -o eatmydata.so */

/* When loaded with LD_PRELOAD, suppresses fsync(), fdatasync() and sync(). */
/* This is useful when you have a program that slows itself down trying to */
/* be crash-proof during an operation where you don't care about crashes */
/* (because if a crash occurs, you can just start again from scratch) such */
/* as restoring an svn dump with svnadmin load. */

/* svnadmin create ./fresh_restore  */
/* LD_PRELOAD=$(pwd)/eatmydata.so svnadmin load ./fresh_restore <backup.dump */

/* This is pretty much exactly the same thing as libeatmydata, only that one */
/* is written more sensibly and doesn't inject a destructor which spews */
/* graffiti onto stderr at program finish. */

#include <stdio.h>

static size_t fsyncs_eaten = 0, syncs_eaten = 0;

static void print_munchness(void) __attribute__((destructor));
static void print_munchness(void)
{
	fprintf(stderr, "Ate %zd fsyncs, %zd syncs.\n", fsyncs_eaten,
	        syncs_eaten);
}

int fdatasync(int fd)
{
	fsyncs_eaten++;
	return 0;
}

int fsync(int fd)
{
	fsyncs_eaten++;
	return 0;
}

void sync(void)
{
	syncs_eaten++;
}
