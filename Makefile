CC=clang

all: last_message spmc_lossy thread_tennis
check-syntax: all

last_message: last_message.c
	$(CC) last_message.c -o last_message -Os -g -Wall -Wextra -std=c99 -lsqlite3 -lapr-1 -lrt

spmc_lossy: spmc_lossy.c
	$(CC) spmc_lossy.c -o spmc_lossy -Os -g -Wall -Wextra -std=c99 -lpthread -Werror

thread_tennis: thread_tennis.cpp
	$(CXX) thread_tennis.cpp -o thread_tennis -Os -g -Wall -Wextra -lpthread -Werror

.PHONY: clean
clean:
	rm -f last_message spmc_lossy thread_tennis
