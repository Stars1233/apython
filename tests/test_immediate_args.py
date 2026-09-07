# A builtin handed an int or float IMMEDIATE where it wants an object.
#
# A Value is one machine word: a pointer is stored raw, but an int inside
# +-2^50 and every float are immediates with no ob_type to read.  A type check
# written as `mov rax, [rbx + PyObject.ob_type]` therefore dereferences the
# NUMBER, and every case below was a SIGSEGV -- not a TypeError, a signal.
#
# None is here for a different reason: it IS a pointer, so it survives the
# dereference and reads a heap singleton's bytes as though they were a type.
# `__import__(None)` answered "No module named 'plemented'", which is a
# fragment of a string that happens to sit near None in .rodata.
#
# The wording matters as much as the refusal: CPython names the type it was
# given, and a caller that catches TypeError and prints it should see the same
# sentence.


def show(label, fn):
    try:
        print(label, "->", repr(fn()))
    except BaseException as e:
        print(label, "->", type(e).__name__ + ":", e)


# --- the compiler entry points ------------------------------------------
for bad in (1, 1.5, 10 ** 60, True, None, [], object):
    show("exec(%r)" % (bad,), lambda b=bad: exec(b))
    show("eval(%r)" % (bad,), lambda b=bad: eval(b))
    show("compile(%r)" % (bad,), lambda b=bad: compile(b, "<s>", "exec"))

# bytes source is legal in all three, and is not a crash but a feature.
show("exec(b'x=1')", lambda: exec(b"x = 1"))
show("eval(b'1+1')", lambda: eval(b"1 + 1"))
show("compile(b'1', eval)", lambda: compile(b"1", "<s>", "eval").co_name)

# --- __import__ ---------------------------------------------------------
for bad in (1, 1.5, 10 ** 60, True, None, [], object):
    show("__import__(%r)" % (bad,), lambda b=bad: __import__(b))

# --- map, whose first argument is called ---------------------------------
for bad in (1, 1.5, 10 ** 60, True, None):
    show("map(%r)" % (bad,), lambda b=bad: list(map(b, [1])))

# --- staticmethod/classmethod, which STORE the argument and hand it back --
for bad in (1, 1.5, 10 ** 60, True, None, "s"):
    show("staticmethod(%r).__func__" % (bad,), lambda b=bad: staticmethod(b).__func__)
    show("classmethod(%r).__func__" % (bad,), lambda b=bad: classmethod(b).__func__)

# __wrapped__ is the same field read through a second name.
show("staticmethod(2).__wrapped__", lambda: staticmethod(2).__wrapped__)
show("classmethod(2.5).__wrapped__", lambda: classmethod(2.5).__wrapped__)

# And they stay callable when the argument really is callable.
show("staticmethod(len)([1,2])", lambda: staticmethod(len).__func__([1, 2]))
