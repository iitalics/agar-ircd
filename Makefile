SRC=src

OC = ocamlbuild \
	-j 4 \
	-cflag -thread \
	-ocamlopt "ocamlopt -thread" \
	-I $(SRC) -use-ocamlfind

all: main.native

main.native:
	$(OC) $@

test:
	$(OC) test.native && ./test.native

clean:
	rm -rf _build main.native

.PHONY: all test main.native clean
