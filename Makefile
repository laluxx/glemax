CC = gcc
# CC = afl-cc
# -fsanitize=address
CFLAGS = -Wall -Wextra -g -I/usr/include/freetype2 -I. $(shell pkg-config --cflags guile-3.0)

TREE_SITTER_C_PATH = ./grammars/tree-sitter-c
TREE_SITTER_SCHEME_PATH = ./grammars/tree-sitter-scheme
TREE_SITTER_HTML_PATH = ./grammars/tree-sitter-html
TREE_SITTER_QUERY_PATH = ./grammars/tree-sitter-query
TREE_SITTER_GLSL_PATH = ./grammars/tree-sitter-glsl
TREE_SITTER_ZIG_PATH = ./grammars/tree-sitter-zig
TREE_SITTER_ODIN_PATH = ./grammars/tree-sitter-odin
TREE_SITTER_MAKE_PATH = ./grammars/tree-sitter-make
TREE_SITTER_COMMONLISP_PATH = ./grammars/tree-sitter-commonlisp
TREE_SITTER_SCSS_PATH = ./grammars/tree-sitter-scss
TREE_SITTER_HASKELL_PATH = ./grammars/tree-sitter-haskell
TREE_SITTER_LUA_PATH = ./grammars/tree-sitter-lua
TREE_SITTER_RUST_PATH = ./grammars/tree-sitter-rust
TREE_SITTER_BASH_PATH = ./grammars/tree-sitter-bash
TREE_SITTER_ELISP_PATH = ./grammars/tree-sitter-elisp
TREE_SITTER_PYTHON_PATH = ./grammars/tree-sitter-python
TREE_SITTER_OCAML_PATH = ./grammars/tree-sitter-ocaml/grammars/ocaml
TREE_SITTER_CSS_PATH = ./grammars/tree-sitter-css
TREE_SITTER_JAVASCRIPT_PATH = ./grammars/tree-sitter-javascript
TREE_SITTER_JULIA_PATH = ./grammars/tree-sitter-julia
TREE_SITTER_CPP_PATH = ./grammars/tree-sitter-cpp
TREE_SITTER_GO_PATH = ./grammars/tree-sitter-go
TREE_SITTER_JSON_PATH = ./grammars/tree-sitter-json
TREE_SITTER_REGEX_PATH = ./grammars/tree-sitter-regex
TREE_SITTER_D_PATH = ./grammars/tree-sitter-d

TREE_SITTER_C_LIB = $(TREE_SITTER_C_PATH)/libtree-sitter-c.a
TREE_SITTER_SCHEME_LIB = $(TREE_SITTER_SCHEME_PATH)/libtree-sitter-scheme.a
TREE_SITTER_HTML_LIB = $(TREE_SITTER_HTML_PATH)/libtree-sitter-html.a
TREE_SITTER_QUERY_LIB = $(TREE_SITTER_QUERY_PATH)/libtree-sitter-query.a
TREE_SITTER_GLSL_LIB = $(TREE_SITTER_GLSL_PATH)/libtree-sitter-glsl.a
TREE_SITTER_ZIG_LIB = $(TREE_SITTER_ZIG_PATH)/libtree-sitter-zig.a
TREE_SITTER_ODIN_LIB = $(TREE_SITTER_ODIN_PATH)/libtree-sitter-odin.a
TREE_SITTER_MAKE_LIB = $(TREE_SITTER_MAKE_PATH)/libtree-sitter-make.a
TREE_SITTER_COMMONLISP_LIB = $(TREE_SITTER_COMMONLISP_PATH)/libtree-sitter-commonlisp.a
TREE_SITTER_SCSS_LIB = $(TREE_SITTER_SCSS_PATH)/libtree-sitter-scss.a
TREE_SITTER_HASKELL_LIB = $(TREE_SITTER_HASKELL_PATH)/libtree-sitter-haskell.a
TREE_SITTER_LUA_LIB = $(TREE_SITTER_LUA_PATH)/libtree-sitter-lua.a
TREE_SITTER_RUST_LIB = $(TREE_SITTER_RUST_PATH)/libtree-sitter-rust.a
TREE_SITTER_BASH_LIB = $(TREE_SITTER_BASH_PATH)/libtree-sitter-bash.a
TREE_SITTER_ELISP_LIB = $(TREE_SITTER_ELISP_PATH)/libtree-sitter-elisp.a
TREE_SITTER_PYTHON_LIB = $(TREE_SITTER_PYTHON_PATH)/libtree-sitter-python.a
TREE_SITTER_OCAML_LIB = $(TREE_SITTER_OCAML_PATH)/libtree-sitter-ocaml.a
TREE_SITTER_CSS_LIB = $(TREE_SITTER_CSS_PATH)/libtree-sitter-css.a
TREE_SITTER_JAVASCRIPT_LIB = $(TREE_SITTER_JAVASCRIPT_PATH)/libtree-sitter-javascript.a
TREE_SITTER_JULIA_LIB = $(TREE_SITTER_JULIA_PATH)/libtree-sitter-julia.a
TREE_SITTER_CPP_LIB = $(TREE_SITTER_CPP_PATH)/libtree-sitter-cpp.a
TREE_SITTER_GO_LIB = $(TREE_SITTER_GO_PATH)/libtree-sitter-go.a
TREE_SITTER_JSON_LIB = $(TREE_SITTER_JSON_PATH)/libtree-sitter-json.a
TREE_SITTER_REGEX_LIB = $(TREE_SITTER_REGEX_PATH)/libtree-sitter-regex.a
TREE_SITTER_D_LIB = $(TREE_SITTER_D_PATH)/libtree-sitter-d.a


LIBS = -ljson-c -lGL -lssl -lcrypto -llume -ltree-sitter $(TREE_SITTER_C_LIB) $(TREE_SITTER_SCHEME_LIB) $(TREE_SITTER_HTML_LIB) $(TREE_SITTER_QUERY_LIB) $(TREE_SITTER_GLSL_LIB) $(TREE_SITTER_ZIG_LIB) $(TREE_SITTER_ODIN_LIB) $(TREE_SITTER_MAKE_LIB) $(TREE_SITTER_COMMONLISP_LIB) $(TREE_SITTER_SCSS_LIB) $(TREE_SITTER_HASKELL_LIB) $(TREE_SITTER_LUA_LIB) $(TREE_SITTER_RUST_LIB) $(TREE_SITTER_BASH_LIB) $(TREE_SITTER_ELISP_LIB) $(TREE_SITTER_PYTHON_LIB) $(TREE_SITTER_OCAML_LIB) $(TREE_SITTER_CSS_LIB) $(TREE_SITTER_JAVASCRIPT_LIB) $(TREE_SITTER_JULIA_LIB) $(TREE_SITTER_CPP_LIB) $(TREE_SITTER_GO_LIB) $(TREE_SITTER_JSON_LIB) $(TREE_SITTER_REGEX_LIB) $(TREE_SITTER_D_LIB) -lm $(shell pkg-config --libs guile-3.0) -lbfd


SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)


TARGET = glemax


all: $(TARGET)

$(TREE_SITTER_C_LIB):
	$(MAKE) -C $(TREE_SITTER_C_PATH) libtree-sitter-c.a

$(TREE_SITTER_SCHEME_LIB):
	$(MAKE) -C $(TREE_SITTER_SCHEME_PATH) libtree-sitter-scheme.a

$(TREE_SITTER_HTML_LIB):
	$(MAKE) -C $(TREE_SITTER_HTML_PATH) libtree-sitter-html.a

$(TREE_SITTER_QUERY_LIB):
	$(MAKE) -C $(TREE_SITTER_QUERY_PATH) libtree-sitter-query.a

$(TREE_SITTER_GLSL_LIB):
	$(MAKE) -C $(TREE_SITTER_GLSL_PATH) libtree-sitter-glsl.a

$(TREE_SITTER_ZIG_LIB):
	$(MAKE) -C $(TREE_SITTER_ZIG_PATH) libtree-sitter-zig.a

$(TREE_SITTER_ODIN_LIB):
	$(MAKE) -C $(TREE_SITTER_ODIN_PATH) libtree-sitter-odin.a

$(TREE_SITTER_MAKE_LIB):
	$(MAKE) -C $(TREE_SITTER_MAKE_PATH) libtree-sitter-make.a

$(TREE_SITTER_COMMONLISP_LIB):
	$(MAKE) -C $(TREE_SITTER_COMMONLISP_PATH) libtree-sitter-commonlisp.a

$(TREE_SITTER_SCSS_LIB):
	$(MAKE) -C $(TREE_SITTER_SCSS_PATH) libtree-sitter-scss.a

$(TREE_SITTER_HASKELL_LIB):
	$(MAKE) -C $(TREE_SITTER_HASKELL_PATH) libtree-sitter-haskell.a

$(TREE_SITTER_LUA_LIB):
	$(MAKE) -C $(TREE_SITTER_LUA_PATH) libtree-sitter-lua.a

$(TREE_SITTER_RUST_LIB):
	$(MAKE) -C $(TREE_SITTER_RUST_PATH) libtree-sitter-rust.a

$(TREE_SITTER_BASH_LIB):
	$(MAKE) -C $(TREE_SITTER_BASH_PATH) libtree-sitter-bash.a

$(TREE_SITTER_ELISP_LIB):
	$(MAKE) -C $(TREE_SITTER_ELISP_PATH) libtree-sitter-elisp.a

$(TREE_SITTER_PYTHON_LIB):
	$(MAKE) -C $(TREE_SITTER_PYTHON_PATH) libtree-sitter-python.a

$(TREE_SITTER_OCAML_LIB):
	$(MAKE) -C $(TREE_SITTER_OCAML_PATH) libtree-sitter-ocaml.a

$(TREE_SITTER_CSS_LIB):
	$(MAKE) -C $(TREE_SITTER_CSS_PATH) libtree-sitter-css.a

$(TREE_SITTER_JAVASCRIPT_LIB):
	$(MAKE) -C $(TREE_SITTER_JAVASCRIPT_PATH) libtree-sitter-javascript.a

$(TREE_SITTER_JULIA_LIB):
	$(MAKE) -C $(TREE_SITTER_JULIA_PATH) libtree-sitter-julia.a

$(TREE_SITTER_CPP_LIB):
	$(MAKE) -C $(TREE_SITTER_CPP_PATH) libtree-sitter-cpp.a

$(TREE_SITTER_GO_LIB):
	$(MAKE) -C $(TREE_SITTER_GO_PATH) libtree-sitter-go.a

$(TREE_SITTER_JSON_LIB):
	$(MAKE) -C $(TREE_SITTER_JSON_PATH) libtree-sitter-json.a

$(TREE_SITTER_REGEX_LIB):
	$(MAKE) -C $(TREE_SITTER_REGEX_PATH) libtree-sitter-regex.a

$(TREE_SITTER_D_LIB):
	$(MAKE) -C $(TREE_SITTER_D_PATH) libtree-sitter-d.a



$(TARGET): $(OBJ) $(TREE_SITTER_C_LIB) $(TREE_SITTER_SCHEME_LIB) $(TREE_SITTER_HTML_LIB) $(TREE_SITTER_QUERY_LIB) $(TREE_SITTER_GLSL_LIB) $(TREE_SITTER_ZIG_LIB) $(TREE_SITTER_ODIN_LIB) $(TREE_SITTER_MAKE_LIB) $(TREE_SITTER_COMMONLISP_LIB) $(TREE_SITTER_SCSS_LIB) $(TREE_SITTER_HASKELL_LIB) $(TREE_SITTER_LUA_LIB) $(TREE_SITTER_RUST_LIB) $(TREE_SITTER_BASH_LIB) $(TREE_SITTER_ELISP_LIB) $(TREE_SITTER_PYTHON_LIB) $(TREE_SITTER_OCAML_LIB) $(TREE_SITTER_CSS_LIB) $(TREE_SITTER_JAVASCRIPT_LIB) $(TREE_SITTER_JULIA_LIB) $(TREE_SITTER_CPP_LIB) $(TREE_SITTER_GO_LIB) $(TREE_SITTER_JSON_LIB) $(TREE_SITTER_REGEX_LIB) $(TREE_SITTER_D_LIB)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)

clean-grammars:
	$(MAKE) -C $(TREE_SITTER_C_PATH) clean
	$(MAKE) -C $(TREE_SITTER_SCHEME_PATH) clean
	$(MAKE) -C $(TREE_SITTER_HTML_PATH) clean
	$(MAKE) -C $(TREE_SITTER_QUERY_PATH) clean
	$(MAKE) -C $(TREE_SITTER_GLSL_PATH) clean
	$(MAKE) -C $(TREE_SITTER_ZIG_PATH) clean
	$(MAKE) -C $(TREE_SITTER_ODIN_PATH) clean
	$(MAKE) -C $(TREE_SITTER_MAKE_PATH) clean
	$(MAKE) -C $(TREE_SITTER_SCSS_PATH) clean
	$(MAKE) -C $(TREE_SITTER_HASKELL_PATH) clean
	$(MAKE) -C $(TREE_SITTER_LUA_PATH) clean
	$(MAKE) -C $(TREE_SITTER_RUST_PATH) clean
	$(MAKE) -C $(TREE_SITTER_BASH_PATH) clean
	$(MAKE) -C $(TREE_SITTER_ELISP_PATH) clean
	$(MAKE) -C $(TREE_SITTER_PYTHON_PATH) clean
	$(MAKE) -C $(TREE_SITTER_OCAML_PATH) clean
	$(MAKE) -C $(TREE_SITTER_CSS_PATH) clean
	$(MAKE) -C $(TREE_SITTER_JAVASCRIPT_PATH) clean
	$(MAKE) -C $(TREE_SITTER_JULIA_PATH) clean
	$(MAKE) -C $(TREE_SITTER_CPP_PATH) clean
	$(MAKE) -C $(TREE_SITTER_GO_PATH) clean
	$(MAKE) -C $(TREE_SITTER_JSON_PATH) clean
	$(MAKE) -C $(TREE_SITTER_REGEX_PATH) clean
	$(MAKE) -C $(TREE_SITTER_D_PATH) clean
	$(MAKE) -C $(TREE_SITTER_COMMONLISP_PATH) clean

clean-all: clean clean-grammars

.PHONY: all clean clean-grammars clean-all

