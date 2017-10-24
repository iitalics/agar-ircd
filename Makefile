
OC = ocamlbuild \
	-use-ocamlfind \
	-j 4 \
	-cflag -thread \
	-ocamlopt "ocamlopt $(OCOPT_FLAGS)" \
	-I src

OCOPT_FLAGS = -thread

all: main.native

release: OCOPT_FLAGS+= -O3
release: main.native

main.native:
	$(OC) $@

test:
	$(OC) -I test test_main.native && ./test_main.native

clean:
	rm -rf _build main.native

.PHONY: all test main.native clean
