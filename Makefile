CC = gcc
CFLAGS = -Wall -Wextra -std=c23 -g3 $(shell pkg-config --cflags freetype2 guile-3.0)
LDFLAGS = -lobsidian -lm $(shell pkg-config --libs freetype2 guile-3.0)

TARGET = kink
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $(TARGET) $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
