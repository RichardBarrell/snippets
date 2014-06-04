CC=clang

all: last_message
check-syntax: all

last_message: last_message.c
	$(CC) last_message.c -o last_message -g -O0 -Wall -Wextra -std=c99 -lsqlite3 -lapr-1 -lrt

.PHONY: clean
clean:
	rm -f last_message
