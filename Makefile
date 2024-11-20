CC = gcc
CFLAGS = -Wall -Wextra -g -I/usr/include/freetype2 -I.
LIBS = -llume -ltree-sitter -ltree-sitter-c -lm
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
