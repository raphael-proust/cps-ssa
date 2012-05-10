
MLS= \
		 util.ml \
		 prim.ml \
		 SSA.ml \
		 CPS.ml \
		 CPS_diff.ml \
		 dom.ml \
		 SSA2CPS.ml \
		 test.ml

CMXS=$(MLS:.ml=.cmx)

CMOS=$(MLS:.ml=.cmo)

MLIS=$(MLS:.ml=.mli)

CMIS=$(MLS:.ml=.cmi)


LIBXS=$(shell ocamlfind query ocamlgraph)/graph.cmxa
LIBS=$(LIBXS:.cmxa=.cma)
LIBDIRS=$(shell ocamlfind query ocamlgraph)


BINNAME=run


OCAMLOPTOPTIONS=
OCAMLCOPTIONS=-g
OCAMLOPT=ocamlopt $(OCAMLOPTOPTIONS)
OCAMLC=ocamlc $(OCAMLCOPTIONS)
OCAMLDEP=ocamldep


all: $(BINNAME).byte

native: $(BINNAME).native

byte: $(BINNAME).byte

$(BINNAME).native: $(CMXS)
	$(OCAMLOPT) $(LIBXS) $(CMXS) -o $(BINNAME).native

$(BINNAME).byte: $(CMOS)
	$(OCAMLC) $(LIBS) $(CMOS) -o $(BINNAME).byte


clean:
	rm -f $(CMOS) $(CMIS) $(CMXS) $(MLS:.ml=.o)

purge: clean
	rm -f .depend $(BINNAME).native $(BINNAME).byte


depend: $(MLS) $(MLIS)
	$(OCAMLDEP) $(MLS) $(MLIS) >.depend


-include .depend

%.cmo: %.ml
	$(OCAMLC) -I $(LIBDIRS) -c $<

%.cmi: %.mli
	$(OCAMLC) -I $(LIBDIRS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -I $(LIBDIRS) -c $<

