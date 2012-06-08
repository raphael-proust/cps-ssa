
MLS= \
		 pprint.ml \
		 util.ml \
		 prim.ml \
		 prim_pp.ml \
		 SSA.ml \
		 CPS.ml \
		 CPS_pp.ml \
		 dom.ml \
		 SSA2CPS.ml \
		 LLVM.ml \
		 llvm_parser.ml \
		 llvm_lexer.ml \
		 LLVM2SSA.ml \
		 test.ml

CMXS=$(MLS:.ml=.cmx)

CMOS=$(MLS:.ml=.cmo)

MLIS=$(MLS:.ml=.mli)

CMIS=$(MLS:.ml=.cmi)

TRASH=llvm_parser.ml llvm_parser.mli llvm_lexer.ml llvm_lexer.mli


LIBXS=$(shell ocamlfind query ocamlgraph)/graph.cmxa
LIBS=$(LIBXS:.cmxa=.cma)
LIBDIRS= -I $(shell ocamlfind query ocamlgraph)/


BINNAME=run


OCAMLOPTOPTIONS=
OCAMLCOPTIONS=-g
OCAMLOPT=ocamlopt $(OCAMLOPTOPTIONS)
OCAMLC=ocamlc $(OCAMLCOPTIONS)
OCAMLLEX=ocamllex
MENHIROPTIONS=--explain
MENHIR=menhir $(MENHIROPTIONS)
OCAMLDEP=ocamldep


all: $(BINNAME).byte

native: $(BINNAME).native

byte: $(BINNAME).byte

$(BINNAME).native: $(CMXS)
	$(OCAMLOPT) $(LIBXS) $(CMXS) -o $(BINNAME).native

$(BINNAME).byte: $(CMOS)
	$(OCAMLC) $(LIBS) $(CMOS) -o $(BINNAME).byte


clean:
	rm -f $(CMOS) $(CMIS) $(CMXS) $(MLS:.ml=.o) $(TRASH)

purge: clean
	rm -f .depend $(BINNAME).native $(BINNAME).byte pprint.mli pprint.ml


depend: $(MLS) $(MLIS)
	$(OCAMLDEP) $(MLS) $(MLIS) >.depend


-include .depend

%.cmo: %.ml
	$(OCAMLC) $(LIBDIRS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(LIBDIRS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(LIBDIRS) -c $<

%.ml %.mli: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(MENHIR) $<

pprint.ml:
	ln -s ../pprint/pprint.ml .

pprint.mli:
	ln -s ../pprint/pprint.mli .

