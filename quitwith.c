/* Think I wrote this while debugging a shell script. */
/* Exits with return code supplied as parameter. */
#include <stdlib.h>
int main(int argc, char** argv) { if (argc==1) return -1; else return atoi(argv[1]); }
