
CS= \
		bar.c \
		bar2.c \
		calls_if.c \
		calls_simpl.c \
		calls_simpl_nested.c \
		constflow.c \
		dead.c \
		dead2.c \
		dead3.c \
		fibonacci.c \
		foo_after.c \
		foo_before.c \
		foo_both.c \
		forloop.c \
		ifif.c \
		ifs.c \
		smallish.c \
		inlinable.c \
		loops.c

HSS= \
		fibonacci_pat.hs \
		fibonacci_zip.hs \
		fun0.hs \
		funif.hs \
		mutual.hs \
		mutual_infinite.hs \
		y.hs

HWS= \
		 frompaper.ll.hw \
		 splitting.ll.hw \
		 cross.ll.hw


LLS=$(CS:.c=.ll) $(HSS:.hs=.ll) $(HWS:.ll.hw=.ll)
SSAS=$(CS:.c=.ssa) $(HSS:.hs=.ssa) $(HWS:.ll.hw=.ssa)
CPSS=$(CS:.c=.cps) $(HSS:.hs=.cps) $(HWS:.ll.hw=.cps)

NORMALIZING= -mem2reg -reassociate

all: run

run: lls
	../../src/run.native $(LLS)

bcs: $(BCS)

lls: $(LLS)

ssas: $(SSAS)

cpss: $(CPSS)

clean:
	rm -f $(BCS) *.ssa *.cps *.ll *.hi *.o

%.pre.ll: %.c
	clang $< -O0 -emit-llvm -S -o $@

%.ll: %.hs
	ghc -keep-llvm-file -O1 -c $<
	opt -S $(NORMALIZING) $@ > $@.pre
	mv $@.pre $@

%.ll: %.pre.ll
	opt -S $(NORMALIZING) $< > $@

%.ll: %.ll.hw
	opt -S $(NORMALIZING) $< > $@

%.cps %.ssa: %.ll
	../src/run.native $<
