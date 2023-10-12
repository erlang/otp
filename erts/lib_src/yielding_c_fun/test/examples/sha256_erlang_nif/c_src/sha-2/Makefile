CFLAGS = -Wall -Wextra -Wpedantic

.PHONY: all
all: test
	./test

test: test.o sha-256.o

.PHONY: clean
clean:
	rm test *.o
