.PHONY: all
all: clockmono libcclockmono.so libjclockmono.so ClockMono.class

.PHONY: clean
clean:
	rm -f ClockMono.h ClockMono.class clockmono libjclockmono.so libcclockmono.so

.PHONY: run
run: all
	env LD_LIBRARY_PATH=`pwd` ./clockmono
	env LD_LIBRARY_PATH=`pwd` java ClockMono

ClockMono.h: ClockMono.java ClockMono.class
	javah -jni ClockMono

ClockMono.class: ClockMono.java
	javac ClockMono.java

clockmono: clockmono.c
	gcc clockmono.c -o clockmono -ldl -Os

libjclockmono.so: ClockMono.h libjclockmono.c
	gcc libjclockmono.c -o libjclockmono.so -shared -fPIC -lrt -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux

libcclockmono.so: libcclockmono.c
	gcc libcclockmono.c -o libcclockmono.so -shared -fPIC -lrt
