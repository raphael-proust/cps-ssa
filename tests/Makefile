
CS= return.c fibonacci.c if0.c for.c
BCS=$(CS:.c=.bc)
LLS=$(CS:.c=.ll)

all: lls

bcs: $(BCS)

lls: $(LLS)

clean:
	rm -f $(BCS) $(LLS)

%.bc: %.c
	clang $< -O0 -emit-llvm -c -o $@

%.ll: %.c
	clang $< -O0 -emit-llvm -S -o $@

