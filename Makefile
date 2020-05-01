OCAMLBUILD=ocamlbuild
OPTIONS=-use-ocamlfind
SRC=src
MAIN=pumplemuse
INCLUDES=-I $(SRC)
PARAMS=$(INCLUDES) $(LIBS) $(PACKAGES)

all: byte native

byte:
	$(OCAMLBUILD) $(PARAMS) $(SRC)/$(MAIN).byte

native:
	$(OCAMLBUILD) $(PARAMS) $(SRC)/$(MAIN).native

clean:
	$(OCAMLBUILD) -clean

