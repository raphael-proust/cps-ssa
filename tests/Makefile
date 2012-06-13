
CS= return.c fibonacci.c if0.c for.c ifif.c
BCS=$(CS:.c=.bc)
LLS=$(CS:.c=.ll)
SSAS=$(CS:.c=.ssa)
CPSS=$(CS:.c=.cps)

all: cpss

bcs: $(BCS)

lls: $(LLS)

ssas: $(SSAS)

cpss: $(CPSS)

clean:
	rm -f $(BCS) $(LLS)

purge: clean
	rm -f $(SSAS) $(CPSS)

%.bc: %.c
	clang $< -O0 -emit-llvm -c -o $@

%.ll: %.c
	clang $< -O0 -emit-llvm -S -o $@

%.cps %.ssa: %.ll
	../src/run.native $<
