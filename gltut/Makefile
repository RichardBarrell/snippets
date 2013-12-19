all: Tut01
.PHONY: all clean

Tut01: Tut01.hs monotime.o
	ghc --make Tut01.hs -main-is Tut01.main monotime.o -lGL

monotime.o: monotime.c
	gcc -o monotime.o -c monotime.c -Os

clean:
	rm -f *.o *.hi
