OCAMLC   = ocamlfind ocamlc -g
OCAMLMLI = ocamlfind ocamlc
OCAMLOPT = ocamlfind opt -unsafe -noassert -inline 100
OCAMLOPT = ocamlfind opt -g
OCAMLDEP = ocamldep.opt
OCAMLDOC = ocamldoc -html
#CONTRAINTES = ./contraintes_coop
INCLUDES = -I .
#LIBS_USR = facile.cma
LIBS_OPAM = -package unix
#LIBS_OPT = $(LIBS_USR:.cma=.cmxa)
#LIBS_BIN = $(LIBS_USR)
SCML = const.ml geom.ml env.ml aircraft.ml plot.ml flyWithVectical.ml
SCCMO = $(SCML:.ml=.cmo)
SCCMX = $(SCML:.ml=.cmx)
all: opt
byte: orca.out
opt: orca.opt
orca.out: $(SCCMO)
	$(OCAMLC) -o $@ $(SCCMO)
orca.opt: $(SCCMX)
	$(OCAMLOPT) -linkpkg $(LIBS_OPAM) -o $@ $(SCCMX)
DIRS = sol reduced
.SUFFIXES: .ml .mli .cmi .cmo .cmx
.mli.cmi:
	$(OCAMLMLI) $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC) $(INCLUDES) $(LIBS_OPAM) -c $<
.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) $(LIBS_OPAM) -c $<
.depend:
	$(OCAMLDEP) *.mli *.ml > .depend
include .depend
init:
	mkdir -p $(DIRS)
cleanall: clean #cleandoc
clean:
	rm -f *.cm* *.annot *.o *.out *.opt *.a *~ .depend
