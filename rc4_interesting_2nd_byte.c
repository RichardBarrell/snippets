#include <openssl/rc4.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
	int buckets[256];
	memset(buckets, 0, 256*sizeof(int));
	int i;
	RC4_KEY key;
	unsigned char input_data[16];
	unsigned char zero[2];
	memset(zero, 0, 2);
	unsigned char data[2];
	for (i=0;i<(1<<21);++i) {
		read(0, input_data, 16);
		RC4_set_key(&key, 16, input_data);
		RC4(&key, 2, zero, data);
		buckets[data[1]]++;
	}
	for (i=0;i<256;++i) {
		printf("buckets[%2x] = %5d%c", i, buckets[i], ((i%8)==7)?'\n':' ');
	}
	return 0;
}
