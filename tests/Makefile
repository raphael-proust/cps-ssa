
CS= return.c fibonacci.c if0.c
BITCODES=$(CS:.c=.bc)
LLS=$(CS:.c=.ll)

all: $(BITCODES)

bitcodes: $(BITCODES)

lls: $(LLS)

clean:
	rm -f $(BITCODES) $(LLS)

%.bc: %.c
	clang $< -O0 -emit-llvm -c -o $@

%.ll: %.c
	clang $< -O0 -emit-llvm -S -o $@

