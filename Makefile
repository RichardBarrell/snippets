CC=clang

all: last_message
check-syntax: all

last_message: last_message.c
	$(CC) last_message.c -o last_message -O3 -Wall -Wextra -std=c99 -lsqlite3 -lapr-1

.PHONY: clean
clean:
	rm -f last_message
