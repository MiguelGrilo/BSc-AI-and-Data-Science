all: ex4

ex4: ex4.o
	$(CC) -o $@ $^ -lm

clean:
	rm -f *.o ex4

run:
	./ex4

%.o: %.c
	$(CC) -c $^