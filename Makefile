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
METHODS_SRCS = $(wildcard src/methods/*.asm)
OPCODES_SRCS = $(wildcard src/opcodes/*.asm)
# The Python source compiler is its own subsystem, peer to src/.
COMPILER_SRCS = $(wildcard compiler/*.asm)
# Objects mirror the source tree.  A flat build/ would put every basename in
# one namespace across the four source directories, and a collision there is
# silent: make picks the first pattern rule whose prerequisite exists, the
# other file is never assembled, and the only symptom is a pile of undefined
# references at link time naming nothing useful.
OBJS = $(SRCS:src/%.asm=build/%.o) $(PYO_SRCS:src/pyo/%.asm=build/pyo/%.o) \
       $(METHODS_SRCS:src/methods/%.asm=build/methods/%.o) \
       $(OPCODES_SRCS:src/opcodes/%.asm=build/opcodes/%.o) \
       $(COMPILER_SRCS:compiler/%.asm=build/compiler/%.o)

# Every object depends on every header: nasm has no depfile support here, and
# a stale build after editing a struct layout in include/*.inc is a silent,
# very confusing failure.
HEADERS = $(wildcard include/*.inc) $(wildcard compiler/*.inc)

# Python compiler for tests
PYTHON = python3

.PHONY: all clean regen check gen-cpython-tests check-cpython check-cpython-source check-stdlib check-source lib-pyc

all: $(TARGET) lib-pyc

# Regenerate the machine-written assembly.  Deliberately phony and deliberately
# not a prerequisite of anything: the outputs are committed so that building
# apython never needs Python, and gen_tables.py refuses to run on anything but
# CPython 3.12, so a real file rule would break the build for everyone else the
# moment a fresh clone's mtimes came out in the wrong order.
regen:
	$(PYTHON) compiler/gen_tables.py > compiler/tables.asm.new
	mv compiler/tables.asm.new compiler/tables.asm
	$(PYTHON) compiler/gen_prule.py
	$(PYTHON) compiler/gen_unicodename.py > compiler/unicodename.asm.new
	mv compiler/unicodename.asm.new compiler/unicodename.asm

$(TARGET): $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

# apython reads .pyc and never .py, so the modules it ships in lib/ are not
# importable until they are byte-compiled.  This is part of building the
# interpreter, not of testing it: `make check` used to run before anything
# compiled them, so any test that imported one failed on a fresh checkout.
lib-pyc:
	@find lib -name '*.py' -exec $(PYTHON) -m py_compile {} \; 2>/dev/null || true

build/%.o: src/%.asm $(HEADERS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

build/pyo/%.o: src/pyo/%.asm $(HEADERS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

build/methods/%.o: src/methods/%.asm $(HEADERS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

build/opcodes/%.o: src/opcodes/%.asm $(HEADERS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

build/compiler/%.o: compiler/%.asm $(HEADERS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

# The generated compiler/tables.asm and compiler/unicodename.asm are checked-in
# sources, not build products -- see `regen` below.  clean must never touch them.
clean:
	rm -rf build $(TARGET)
	find lib tests compiler -name '__pycache__' -type d -exec rm -rf {} + 2>/dev/null || true

# Test target: compile .py to .pyc, run both python3 and apython, diff
check: $(TARGET) lib-pyc
	@bash tests/run_tests.sh

# Run the whole test corpus through our own compiler rather than CPython's:
# apython is handed the .py and compiles it itself.  Ratchets against
# tests/compile_floor.txt.
check-source: $(TARGET) lib-pyc
	@bash tests/source_probe.sh

# How much of CPython 3.12's own Lib/ can we import?  Ratchets against
# tests/stdlib_floor.txt.  Needs a CPython source checkout; set CPYTHON_LIB
# to point at its Lib/ directory.  Skips cleanly when it is absent.
check-stdlib: $(TARGET)
	@bash tests/stdlib_probe.sh

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

# The same corpus, compiled by OUR compiler rather than CPython's: apython is
# handed the .py.  Ratchets against tests/cpython_source_floor.txt.  It is a
# harder corpus than tests/ -- it is CPython's own, written to be adversarial --
# and most of the compiler's later bugs were found here.
check-cpython-source: $(TARGET) lib-pyc
	@bash tests/cpython_source_probe.sh

check-cpython: $(TARGET) gen-cpython-tests
	@for t in $(CPYTHON_TESTS); do \
	    echo "Running CPython $$t.py..."; \
	    ./apython tests/cpython/__pycache__/$$t.cpython-312.pyc || exit 1; \
	done
	@echo "All CPython tests passed."
