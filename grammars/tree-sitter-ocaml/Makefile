TS ?= tree-sitter

# Default target to build everything
all: libtree-sitter-ocaml.a

# Rule to build the static library
libtree-sitter-ocaml.a:
	$(MAKE) -C grammars/ocaml libtree-sitter-ocaml.a
	$(MAKE) -C grammars/interface libtree-sitter-ocaml-interface.a
	$(MAKE) -C grammars/type libtree-sitter-ocaml-type.a

# Install targets
install:
	$(MAKE) -C grammars/ocaml install
	$(MAKE) -C grammars/interface install
	$(MAKE) -C grammars/type install

# Uninstall targets
uninstall:
	$(MAKE) -C grammars/ocaml uninstall
	$(MAKE) -C grammars/interface uninstall
	$(MAKE) -C grammars/type uninstall

# Clean targets
clean:
	$(MAKE) -C grammars/ocaml clean
	$(MAKE) -C grammars/interface clean
	$(MAKE) -C grammars/type clean

# Test targets
test:
	$(TS) test
	$(TS) parse examples/* --quiet --time

# Generate parser source files
generate:
	cd grammars/ocaml && $(TS) generate
	cd grammars/interface && $(TS) generate
	cd grammars/type && $(TS) generate

.PHONY: all install uninstall clean test generate
