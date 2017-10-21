SRC=src

OC=ocamlbuild \
	-cflag -thread \
	-ocamlopt "ocamlopt -thread" \
	-I $(SRC) -use-ocamlfind

all: main.native

main.native:
	$(OC) $@

clean:
	rm -rf _build main.native

.PHONY: all main.native clean
