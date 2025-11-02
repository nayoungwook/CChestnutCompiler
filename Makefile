CC = gcc
CFLAGS = -finput-charset=UTF-8 -Wall -Icompiler/includes -std=c11
TARGET = chestnut


COMPILER_SRC = $(wildcard compiler/*.c)
COMPILER_OBJ = $(patsubst compiler/%.c, build/compiler/%.o, $(COMPILER_SRC))

MAIN_SRC = src/main.c
MAIN_OBJ = build/src/main.o

.PHONY: all clean

all: $(TARGET)


$(TARGET): $(MAIN_OBJ) $(COMPILER_OBJ)
	$(CC) $(CFLAGS) $^ -o $@

$(MAIN_OBJ): $(MAIN_SRC)
	@if not exist build\src mkdir build\src
	$(CC) $(CFLAGS) -c $< -o $@

build/compiler/%.o: compiler/%.c
	@if not exist build\compiler mkdir build\compiler
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf build $(TARGET)
