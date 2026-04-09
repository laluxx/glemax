CC     = gcc
CFLAGS = -Wall -Wextra -std=c23 -g3 -D_DEFAULT_SOURCE \
         $(shell pkg-config --cflags freetype2 guile-3.0)
LDFLAGS = -lobsidian -lX11 -lm -lfontconfig -ltree-sitter \
          $(shell pkg-config --libs freetype2 guile-3.0)

ASAN_FLAGS = -fsanitize=address,undefined -fno-omit-frame-pointer

TARGET     = glemax
ASAN_TARGET = $(TARGET)_asan
SRCS       = $(wildcard *.c)
OBJS       = $(SRCS:.c=.o)
ASAN_OBJS  = $(SRCS:.c=.asan.o)

all: $(TARGET)
asan: $(ASAN_TARGET)

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $@ $(LDFLAGS)

$(ASAN_TARGET): $(ASAN_OBJS)
	$(CC) $(ASAN_FLAGS) $(ASAN_OBJS) -o $@ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.asan.o: %.c
	$(CC) $(CFLAGS) $(ASAN_FLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(ASAN_OBJS) $(TARGET) $(ASAN_TARGET)

.PHONY: all asan clean
