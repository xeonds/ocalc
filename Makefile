OCAMLC_FLAGS = 

all: build/ocalc

build/ocalc: src/main.cmo
	cd src && \
	ocamlfind ocamlc $(OCAMLC_FLAGS) -linkpkg -o ../build/ocalc main.cmo

src/main.cmo: src/main.ml
	cd src && ocamlfind ocamlc $(OCAMLC_FLAGS) -c main.ml

clean:
	rm -f src/*.cmo src/*.cmi src/*.mli
