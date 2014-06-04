CC=clang

all: last_message
check-syntax: all

last_message: last_message.c
	$(CC) last_message.c -o last_message -Os -g -Wall -Wextra -std=c99 -lsqlite3 -lapr-1 -lrt

.PHONY: clean
clean:
	rm -f last_message
