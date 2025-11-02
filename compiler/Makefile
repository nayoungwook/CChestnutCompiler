CC := gcc
CFLAGS := -Iincludes -Wall -O2

SRC := $(wildcard *.c)
OBJ := $(patsubst %.c,build/compiler/%.o,$(SRC))

all: $(OBJ)

build/compiler/%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	if exist build\compiler rmdir /s /q build\compiler
