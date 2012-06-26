
CS= \
		bar.c \
		blah.c \
		compressible.c \
		const.c \
		constflow.c \
		dead.c \
		dead2.c \
		eg.c \
		fibonacci.c \
		foo.c \
		fooo.c \
		foooo.c \
		for.c \
		if0.c \
		ifif.c \
		infinite.c \
		infinite_and_dead.c \
		return.c \
		smallish.c \
		supervals.c \
		vals.c \
		vars.c \
		calls.c \
		calls_if.c \
		calls_if_const.c

HSS= \
	fibonacci_pat.hs \
	fibonacci_zip.hs \
	fun.hs \
	mutual.hs \
	mutual_dead.hs \
	mutual_infinite.hs \
	x.hs \
	y.hs \
	funif.hs

LLS=$(CS:.c=.ll) $(HSS:.hs=.ll)
SSAS=$(CS:.c=.ssa) $(HSS:.hs=.ssa)
CPSS=$(CS:.c=.cps) $(HSS:.hs=.cps)

OPTS= \
			argpromotion adce basiccg constmerge constprop \
			dce die dse globaldce globalopt gvn indvars instcombine \
			instsimplify iv-users licm loop-deletion loop-extract \
			loop-extract-single loop-idiom loop-instsimplify loop-reduce \
			loop-rotate loop-simplify loop-unroll loop-unswitch \
			mergereturn reg2mem sccp sink tailcallelim

OPTS1= gvn mergereturn sccp indvars instcombine
OPTS2= gvn mergereturn sccp indvars instcombine

all: run

run: lls runner
	../src/run.native $(LLS) -- $(OPTS1) -- $(OPTS2)

runner:
	make -C ../src depend
	make -C ../src native

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
	opt -S -mem2reg $@ > $@.pre
	mv $@.pre $@

%.ll: %.pre.ll
	opt -S -mem2reg $< > $@

%.cps %.ssa: %.ll
	../src/run.native $<
