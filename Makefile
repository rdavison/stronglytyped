.PHONY: default
default: build-watch

.PHONY: install-dev-deps
install-dev-deps:
	opam install merlin ocaml-lsp-server odoc ocamlformat utop dune-release

.PHONY: install-deps
install-deps:
	opam install . --deps-only

.PHONY: build-watch
build-watch:
	dune build @all -w

.PHONY: build
build:
	dune build @all

.PHONY: test
test:
	dune build @runtest

.PHONY: clean
clean:
	dune clean || rm -rf _build
	rm -rf _build.prev

.PHONY: utop
utop:
	dune utop

.PHONY: fmt
fmt:
	find src -name '*.ml' -o -name '*.mli' -exec ocamlformat -i {} \;