
CS= \
		return.c fibonacci.c if0.c for.c ifif.c foo.c bar.c vars.c eg.c blah.c \
		vals.c supervals.c const.c constflow.c smallish.c compressible.c dead.c \
		dead2.c
BCS=$(CS:.c=.bc)
LLS=$(CS:.c=.ll)
SSAS=$(CS:.c=.ssa)
CPSS=$(CS:.c=.cps)

OPTS= \
			argpromotion adce basiccg constmerge constprop \
			dce die dse globaldce globalopt gvn indvars instcombine \
			instsimplify iv-users licm loop-deletion loop-extract \
			loop-extract-single loop-idiom loop-instsimplify loop-reduce \
			loop-rotate loop-simplify loop-unroll loop-unswitch mem2reg \
			mergereturn reg2mem sccp sink tailcallelim

all: run

run: lls
	make -C ../src depend
	make -C ../src native
	../src/run.native $(LLS) -- $(OPTS)

bcs: $(BCS)

lls: $(LLS)

ssas: $(SSAS)

cpss: $(CPSS)

clean:
	rm -f $(BCS) *.ssa *.cps *.ll

%.bc: %.c
	clang $< -O0 -emit-llvm -c -o $@

%.ll: %.c
	clang $< -O0 -emit-llvm -S -o $@

%.cps %.ssa: %.ll
	../src/run.native $<
