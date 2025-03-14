CC = gcc
# CC = afl-cc
# -fsanitize=address
CFLAGS = -Wall -Wextra -g -I/usr/include/freetype2 -I. $(shell pkg-config --cflags guile-3.0)
LIBS = -lssl -lcrypto -llume -ltree-sitter -ltree-sitter-c -lm $(shell pkg-config --libs guile-3.0) -lbfd
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
TARGET = glemax

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)

.PHONY: all clean
