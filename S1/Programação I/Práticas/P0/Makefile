# makefile para o Hello

all: hello

hello: hello.o
	$(CC) -o $@ $^

clean:
	rm -f *.o hello

%.o: %.c
	$(CC) -c $^