ESY=npx esy
DUNE=$(ESY) dune

.PHONY: default
default: build-watch

.PHONY: install-deps
install-deps:
	$(ESY) install
	$(ESY) build

.PHONY: build-watch
build-watch:
	$(DUNE) build @all -w

.PHONY: build
build:
	$(DUNE) build @all

.PHONY: test
test:
	$(DUNE) build @runtest

.PHONY: clean
clean:
	$(DUNE) clean || rm -rf _build
	rm -rf _build.prev

.PHONY: distclean
distclean: clean
	rm -rf _esy
	rm -rf node_modules

.PHONY: utop
utop:
	$(DUNE) utop

.PHONY: fmt
fmt:
	$(ESY) find src -name '*.ml' -o -name '*.mli' -exec ocamlformat -i {} \;
