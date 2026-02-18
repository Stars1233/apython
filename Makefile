# Makefile for apython - Python bytecode interpreter in x86-64 assembly

NASM = nasm
NASMFLAGS = -f elf64 -I include/ -g -F dwarf
CC = cc
LDFLAGS = -lc -lgmp
TARGET = apython

# Source files
SRCS = $(wildcard src/*.asm)
PYO_SRCS = $(wildcard src/pyo/*.asm)
LIB_SRCS = $(wildcard src/lib/*.asm)
OBJS = $(SRCS:src/%.asm=build/%.o) $(PYO_SRCS:src/pyo/%.asm=build/%.o) $(LIB_SRCS:src/lib/%.asm=build/%.o)

# Python compiler for tests
PYTHON = python3

.PHONY: all clean check gen-cpython-tests check-cpython

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

build/%.o: src/%.asm | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build/%.o: src/pyo/%.asm | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build/%.o: src/lib/%.asm | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build:
	mkdir -p build

clean:
	rm -rf build $(TARGET) tests/__pycache__

# Test target: compile .py to .pyc, run both python3 and apython, diff
check: $(TARGET)
	@bash tests/run_tests.sh

# Compile a single .py to .pyc
tests/__pycache__/%.cpython-312.pyc: tests/%.py
	$(PYTHON) -m py_compile $<

# CPython test suite targets
gen-cpython-tests:
	@echo "Compiling lib/ tree..."
	@find lib -name '*.py' -exec $(PYTHON) -m py_compile {} \;
	@echo "Compiling tests/cpython/test_int.py..."
	@$(PYTHON) -m py_compile tests/cpython/test_int.py
	@echo "Compiling tests/cpython/test_float.py..."
	@$(PYTHON) -m py_compile tests/cpython/test_float.py
	@echo "Compiling tests/cpython/test_bool.py..."
	@$(PYTHON) -m py_compile tests/cpython/test_bool.py
	@echo "Done."

check-cpython: $(TARGET) gen-cpython-tests
	@echo "Running CPython test_int.py..."
	@./apython tests/cpython/__pycache__/test_int.cpython-312.pyc
	@echo "Running CPython test_float.py..."
	@./apython tests/cpython/__pycache__/test_float.cpython-312.pyc
	@echo "Running CPython test_bool.py..."
	@./apython tests/cpython/__pycache__/test_bool.cpython-312.pyc
