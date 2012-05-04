
MLS= \
		 util.ml \
		 prim.ml \
		 SSA.ml \
		 CPS.ml \
		 CPS_diff.ml \
		 SSA2CPS.ml \
		 difftest.ml

CMXS=$(MLS:.ml=.cmx)

CMOS=$(MLS:.ml=.cmo)

MLIS=$(MLS:.ml=.mli)

CMIS=$(MLS:.ml=.cmi)


LIBXS=
LIBS=$(LIBXS:.cmxa=.cma)


BINNAME=run


OCAMLOPTOPTIONS=
OCAMLCOPTIONS=
OCAMLOPT=ocamlopt $(OCAMLOPTOPTIONS)
OCAMLC=ocamlc $(OCAMLCOPTIONS)
OCAMLDEP=ocamldep


all: native

native: $(CMXS)
	$(OCAMLOPT) $(LIBXS) $(CMXS) -o $(BINNAME).native

byte: $(CMOS)
	$(OCAMLC) $(LIBS) $(CMOS) -o $(BINNAME).byte


clean:
	rm -f $(CMOS) $(CMIS) $(CMXS) $(MLS:.ml=.o)

purge: clean
	rm -f .depend $(BINNAME).native $(BINNAME).byte


depend .depend: $(MLS) $(MLIS)
	$(OCAMLDEP) $(MLS) $(MLIS) >.depend


-include .depend

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

