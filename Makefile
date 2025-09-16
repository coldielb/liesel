CC ?= cc
CFLAGS ?= -std=c17 -Wall -Wextra -Werror -pedantic -g
CFLAGS += -Iinclude
LDFLAGS ?=
LDLIBS ?= -lm

SRC := $(wildcard src/*.c)
OBJ := $(patsubst src/%.c,build/%.o,$(SRC))
TARGET := build/liesel

.PHONY: all clean run

all: $(TARGET)

build:
	mkdir -p build

$(TARGET): build $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) $(LDFLAGS) $(LDLIBS) -o $@

build/%.o: src/%.c | build
	$(CC) $(CFLAGS) -c $< -o $@

run: $(TARGET)
	$(TARGET) $(RUNARGS)

clean:
	rm -rf build
