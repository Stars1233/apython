# str.join() and bytes.join() have to notice that draining the iterable failed.
#
# join indexes ob_item directly, which only a list or a tuple has, so anything
# else is materialised into a tuple first.  That step runs whatever produced
# the iterable, and a generator that raises part way through leaves NULL
# behind with the exception already pending -- and this read ob_size off the
# NULL.  `"".join(codecs.iterdecode(gen, "idna"))` is exactly that shape, and
# it is what CPython's test_codecs has been dying on.


def boom(n, exc):
    for i in range(n):
        yield str(i)
    raise exc("from the generator")


for exc in (ValueError, KeyError, LookupError, RuntimeError, StopAsyncIteration):
    try:
        r = "".join(boom(3, exc))
        print(exc.__name__, "-> joined", r)
    except Exception as e:
        print(exc.__name__, "->", type(e).__name__, e)

# Raising on the very first pull, before anything is produced.
def boom_first():
    raise TypeError("immediately")
    yield "x"


try:
    "".join(boom_first())
    print("first -> joined")
except TypeError as e:
    print("first ->", e)

# The same through a __iter__ that raises, and through an iterator whose
# __next__ raises.
class BadIter:
    def __iter__(self):
        raise ZeroDivisionError("no iterator for you")


try:
    "-".join(BadIter())
    print("bad __iter__ -> joined")
except ZeroDivisionError as e:
    print("bad __iter__ ->", e)


class BadNext:
    def __iter__(self):
        return self

    def __next__(self):
        raise MemoryError("out")


try:
    "-".join(BadNext())
    print("bad __next__ -> joined")
except MemoryError as e:
    print("bad __next__ ->", e)

# bytes.join and bytearray.join take the same path.
try:
    b"".join(x.encode() for x in boom(2, ValueError))
    print("bytes -> joined")
except ValueError as e:
    print("bytes ->", e)

# And the ordinary cases still work.
print("list:", "-".join(["a", "b", "c"]))
print("tuple:", "-".join(("a", "b")))
print("generator:", "-".join(str(i) for i in range(4)))
print("set of one:", "-".join({"solo"}))
print("empty gen:", repr("-".join(x for x in [])))
print("dict keys:", "-".join({"k": 1, "j": 2}))
print("bytes ok:", b"-".join([b"a", b"b"]))
print("done")
