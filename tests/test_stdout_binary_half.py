# sys.stdout has a binary half and a writelines().
#
# sys.stdout here is not a Python TextIOWrapper -- it is the assembly file
# object, whose tp_name is the string "TextIOWrapper" -- and its attributes come
# from a fixed ladder in fileobj_getattr.  `buffer` and `writelines` were not on
# it, so `sys.stdout.buffer` was an AttributeError (which is what stopped
# test_argparse) and so was `sys.stdout.writelines` (test_audit).
#
# The buffer is built on FIRST ASK and kept, not at start-up: making it eagerly
# means importing _io before anything runs, and that is 1.1 ms -> 2.9 ms on
# EVERY invocation of the interpreter.  Only a program that asks pays.
#
# Two things this file does NOT compare against CPython, both recorded in
# DIVERGENCES.md.  The binary half is an _io.FileIO over the same descriptor
# rather than the BufferedWriter CPython's text layer sits on -- so its type
# name differs.  And because the two halves here write to one descriptor
# independently, handing out the binary half turns the text half's buffering
# OFF, which keeps mixed writes in program order; CPython, whose text layer sits
# on its binary one, emits its buffered text last instead.

import sys

# --- writelines -------------------------------------------------------------
sys.stdout.writelines(["first\n", "second\n"])
sys.stdout.writelines([])
sys.stdout.writelines(iter(["from an iterator\n"]))
sys.stdout.writelines(x for x in ["from a generator\n"])

try:
    sys.stdout.writelines(42)
    print("NO ERROR for a non-iterable")
except TypeError:
    print("writelines refuses a non-iterable")

try:
    sys.stdout.writelines([b"bytes are not str"])
    print("NO ERROR for bytes")
except TypeError:
    print("writelines refuses bytes to a text stream")

# --- the binary half --------------------------------------------------------
buf = sys.stdout.buffer
print(buf is sys.stdout.buffer, "the same object every time")
print(hasattr(buf, "write"), hasattr(buf, "fileno"), "and it is a binary stream")
print(buf.fileno() == sys.stdout.fileno(), "over the same descriptor")

# What it writes is not compared here: the two halves reach one descriptor
# independently, so mixing them orders differently from CPython, which is the
# divergence the header names.  That it WRITES, and says how much, is the part
# that agrees.
print(sys.stdout.buffer.write(b""), "an empty write reports zero bytes")

# stderr has one too, which is what a traceback printer reaches for.
print(sys.stderr.buffer.fileno() == sys.stderr.fileno(), "stderr has one too")

# --- what did not change ----------------------------------------------------
print(sys.stdout.writable(), sys.stdout.readable(), "still says what it is")
print(sys.stdout.encoding, "and what it encodes as")
