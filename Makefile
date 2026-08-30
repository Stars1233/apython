# Makefile for apython - Python bytecode interpreter in x86-64 assembly

VERSION_MAJOR = 0
VERSION_MINOR = 6
VERSION_PATCH = 0
VERSION = $(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)

NASM = nasm
NASMFLAGS = -f elf64 -I include/ -I compiler/ -g -F dwarf \
    -DVERSION_MAJOR=$(VERSION_MAJOR) -DVERSION_MINOR=$(VERSION_MINOR) \
    -DVERSION_PATCH=$(VERSION_PATCH) -DVERSION_STR=\"$(VERSION)\"
# INT_STRESS=N boxes every |int| >= N as a heap PyIntObject, so the normal
# test suite exercises the heap-int paths.  Use it to shake out code that only
# handles SmallInt operands.  INT_STRESS=1 defaults the threshold to 8.
ifdef INT_STRESS
ifeq ($(INT_STRESS),1)
INT_STRESS_THRESHOLD = 8
else
INT_STRESS_THRESHOLD = $(INT_STRESS)
endif
NASMFLAGS += -DINT_STRESS_BOX=$(INT_STRESS_THRESHOLD)
endif

CC = cc
LDFLAGS = -no-pie -lc -lgmp
TARGET = apython

# Source files
SRCS = $(wildcard src/*.asm)
PYO_SRCS = $(wildcard src/pyo/*.asm)
LIB_SRCS = $(wildcard src/lib/*.asm)
# The Python source compiler is its own subsystem, peer to src/.
COMPILER_SRCS = $(wildcard compiler/*.asm)
OBJS = $(SRCS:src/%.asm=build/%.o) $(PYO_SRCS:src/pyo/%.asm=build/%.o) \
       $(LIB_SRCS:src/lib/%.asm=build/%.o) $(COMPILER_SRCS:compiler/%.asm=build/%.o)

# Every object depends on every header: nasm has no depfile support here, and
# a stale build after editing a struct layout in include/*.inc is a silent,
# very confusing failure.
HEADERS = $(wildcard include/*.inc) $(wildcard compiler/*.inc)

# Python compiler for tests
PYTHON = python3

.PHONY: all clean check gen-cpython-tests check-cpython check-stdlib lib-pyc

all: $(TARGET) lib-pyc

$(TARGET): $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

# apython reads .pyc and never .py, so the modules it ships in lib/ are not
# importable until they are byte-compiled.  This is part of building the
# interpreter, not of testing it: `make check` used to run before anything
# compiled them, so any test that imported one failed on a fresh checkout.
lib-pyc:
	@find lib -name '*.py' -exec $(PYTHON) -m py_compile {} \; 2>/dev/null || true

build/%.o: src/%.asm $(HEADERS) | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build/%.o: src/pyo/%.asm $(HEADERS) | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build/%.o: src/lib/%.asm $(HEADERS) | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build/%.o: compiler/%.asm $(HEADERS) | build
	$(NASM) $(NASMFLAGS) -o $@ $<

build:
	mkdir -p build

clean:
	rm -rf build $(TARGET) tests/__pycache__
	find lib -name '__pycache__' -type d -exec rm -rf {} + 2>/dev/null || true

# Test target: compile .py to .pyc, run both python3 and apython, diff
check: $(TARGET) lib-pyc
	@bash tests/run_tests.sh

# How much of CPython 3.12's own Lib/ can we import?  Ratchets against
# tests/stdlib_floor.txt.  Needs a CPython source checkout; set CPYTHON_LIB
# to point at its Lib/ directory.  Skips cleanly when it is absent.
check-stdlib: $(TARGET)
	@bash tests/stdlib_probe.sh

# Compile a single .py to .pyc
tests/__pycache__/%.cpython-312.pyc: tests/%.py
	$(PYTHON) -m py_compile $<

# CPython test suite targets
# The CPython-derived test corpus.  One list, used by both the compile step
# and the run step -- and, later, by the run-from-source variant.
CPYTHON_TESTS = \
	test_int test_float test_bool test_str_ops \
	test_str_methods test_sort test_enumerate test_keywordonlyarg \
	test_augassign test_list test_tuple test_dict \
	test_set test_isinstance test_decorators test_scope \
	test_generators test_unary test_pow test_contains \
	test_exception_variations test_genexps test_listcomps test_raise \
	test_class test_compare test_with test_opcodes \
	test_baseexception test_extcall test_iter test_lambda \
	test_property test_string test_bytes test_builtin \
	test_types test_closures test_dict_extra test_tuple_extra \
	test_set_extra test_list_extra test_controlflow test_math_basic \
	test_global_nonlocal test_unpacking test_inheritance test_del \
	test_assert test_assignment test_exceptions_extra test_generators_extra \
	test_format test_slice_ops test_numeric test_comprehensions \
	test_decorators_extra test_walrus test_match test_datastructures \
	test_exceptions_builtin test_functions test_range_extra test_conditional

gen-cpython-tests: lib-pyc
	@for t in $(CPYTHON_TESTS); do \
	    echo "Compiling tests/cpython/$$t.py..."; \
	    $(PYTHON) -m py_compile tests/cpython/$$t.py || exit 1; \
	done
	@echo "Done."

check-cpython: $(TARGET) gen-cpython-tests
	@for t in $(CPYTHON_TESTS); do \
	    echo "Running CPython $$t.py..."; \
	    ./apython tests/cpython/__pycache__/$$t.cpython-312.pyc || exit 1; \
	done
	@echo "All CPython tests passed."
