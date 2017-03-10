CC=clang

all: last_message spmc_lossy
check-syntax: all

last_message: last_message.c
	$(CC) last_message.c -o last_message -Os -g -Wall -Wextra -std=c99 -lsqlite3 -lapr-1 -lrt

spmc_lossy: spmc_lossy.c
	$(CC) spmc_lossy.c -o spmc_lossy -Os -g -Wall -Wextra -std=c99 -lpthread -Werror

.PHONY: clean
clean:
	rm -f last_message spmc_lossy
