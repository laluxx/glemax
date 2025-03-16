CC = gcc
# CC = afl-cc
# -fsanitize=address
CFLAGS = -Wall -Wextra -g -I/usr/include/freetype2 -I. $(shell pkg-config --cflags guile-3.0)

# Path to the tree-sitter-c library
TREE_SITTER_C_PATH = ./grammars/tree-sitter-c

# Path to the tree-sitter-scheme library
TREE_SITTER_SCHEME_PATH = ./grammars/tree-sitter-scheme

# Link against the tree-sitter-c static library
TREE_SITTER_C_LIB = $(TREE_SITTER_C_PATH)/libtree-sitter-c.a

# Link against the tree-sitter-scheme static library
TREE_SITTER_SCHEME_LIB = $(TREE_SITTER_SCHEME_PATH)/libtree-sitter-scheme.a

# Link against the tree-sitter-c and tree-sitter-scheme libraries
LIBS = -lssl -lcrypto -llume -ltree-sitter $(TREE_SITTER_C_LIB) $(TREE_SITTER_SCHEME_LIB) -lm $(shell pkg-config --libs guile-3.0) -lbfd

# Source files and object files
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)

# Target executable
TARGET = glemax

# Default target
all: $(TARGET)

# Build the tree-sitter-c grammar
$(TREE_SITTER_C_LIB):
	$(MAKE) -C $(TREE_SITTER_C_PATH) libtree-sitter-c.a

# Build the tree-sitter-scheme grammar
$(TREE_SITTER_SCHEME_LIB):
	$(MAKE) -C $(TREE_SITTER_SCHEME_PATH) libtree-sitter-scheme.a

# Link the target executable
$(TARGET): $(OBJ) $(TREE_SITTER_C_LIB) $(TREE_SITTER_SCHEME_LIB)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

# Compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build artifacts
clean:
	rm -f $(OBJ) $(TARGET)

# Clean the grammars
clean-grammars:
	$(MAKE) -C $(TREE_SITTER_C_PATH) clean
	$(MAKE) -C $(TREE_SITTER_SCHEME_PATH) clean

# Clean everything (main project + grammars)
clean-all: clean clean-grammars

.PHONY: all clean clean-grammars clean-all






# CC = gcc
# # CC = afl-cc
# # -fsanitize=address
# CFLAGS = -Wall -Wextra -g -I/usr/include/freetype2 -I. $(shell pkg-config --cflags guile-3.0)

# # Path to the tree-sitter-c library
# TREE_SITTER_C_PATH = ./grammars/tree-sitter-c

# # Path to the tree-sitter-scheme library
# TREE_SITTER_SCHEME_PATH = ./grammars/tree-sitter-scheme

# # Link against the tree-sitter-c static library
# TREE_SITTER_C_LIB = $(TREE_SITTER_C_PATH)/libtree-sitter-c.a

# # Link against the tree-sitter-scheme static library
# TREE_SITTER_SCHEME_LIB = $(TREE_SITTER_SCHEME_PATH)/libtree-sitter-scheme.a

# # Link against the tree-sitter-c and tree-sitter-scheme libraries
# LIBS = -lssl -lcrypto -llume -ltree-sitter $(TREE_SITTER_C_LIB) $(TREE_SITTER_SCHEME_LIB) -lm $(shell pkg-config --libs guile-3.0) -lbfd

# # Source files and object files
# SRC = $(wildcard *.c)
# OBJ = $(SRC:.c=.o)

# # Target executable
# TARGET = glemax

# # Default target
# all: $(TARGET)

# # Link the target executable
# $(TARGET): $(OBJ)
# 	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

# # Compile source files into object files
# %.o: %.c
# 	$(CC) $(CFLAGS) -c $< -o $@

# # Clean up build artifacts
# clean:
# 	rm -f $(OBJ) $(TARGET)

# .PHONY: all clean

