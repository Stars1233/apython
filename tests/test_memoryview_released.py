# Every operation on a released memoryview refuses, including the ones that do
# not go through a method.
#
# `bytes(m)` reaches memoryview_as_bytes straight from bytes_type_call, and
# that funnel -- which tobytes(), hex() and every comparison also use -- never
# asked whether the view had been released.  It copied from a buffer that was
# no longer there.  `.obj` answered None where CPython refuses, and
# `memoryview(released)` built a second view onto nothing.
#
# repr is left out: CPython's carries the address.

def probe(name, fn):
    try:
        r = fn()
        print(name, "->", repr(r))
    except Exception as e:
        print(name, "->", type(e).__name__, str(e)[:55])

def fresh():
    m = memoryview(bytearray(b"abcdefgh"))
    m.release()
    return m

ops = [
    ("bytes()",      lambda: bytes(fresh())),
    ("bytearray()",  lambda: bytearray(fresh())),
    ("tobytes",      lambda: fresh().tobytes()),
    ("tolist",       lambda: fresh().tolist()),
    ("hex",          lambda: fresh().hex()),
    ("len",          lambda: len(fresh())),
    ("index",        lambda: fresh()[0]),
    ("slice",        lambda: fresh()[1:3]),
    ("setitem",      lambda: fresh().__setitem__(0, 1)),
    ("iter",         lambda: list(fresh())),
    ("eq bytes",     lambda: fresh() == b"abcdefgh"),
    ("contains",     lambda: 97 in fresh()),
    ("cast",         lambda: fresh().cast("I")),
    ("nbytes",       lambda: fresh().nbytes),
    ("obj",          lambda: fresh().obj),
    ("format",       lambda: fresh().format),
    ("readonly",     lambda: fresh().readonly),
    ("shape",        lambda: fresh().shape),
    ("release again",lambda: fresh().release()),
    ("with again",   lambda: fresh().__enter__()),
    ("toreadonly",   lambda: fresh().toreadonly()),
    ("memoryview()", lambda: memoryview(fresh())),
]
for n, f in ops:
    probe(n, f)
print("done")
