# bytes(n) and bytearray(n) take anything with __index__ as the count.
#
# CPython's bytes_new_impl consults __bytes__, then PyIndex_Check, then
# PyBytes_FromObject.  byteslike_source's count arm named int, an int subclass
# and bool and nothing else, so an object whose only numeric face is __index__
# fell past the buffer test into the iterable arm and came back as
# "cannot convert 'C' object to bytes".


def check(label, fn):
    try:
        print(label, "->", fn())
    except Exception as e:
        print(label, "->", type(e).__name__, e)


class Three:
    def __index__(self):
        return 3


class Zero:
    def __index__(self):
        return 0


class Negative:
    def __index__(self):
        return -1


class Huge:
    def __index__(self):
        return 1 << 70


class Raises:
    def __index__(self):
        raise ValueError("no index for you")


class NotAnInt:
    def __index__(self):
        return "x"


class Floaty:
    def __index__(self):
        return 1.5


for name, cls in (("three", Three), ("zero", Zero), ("negative", Negative),
                  ("huge", Huge), ("raises", Raises), ("notint", NotAnInt),
                  ("floaty", Floaty)):
    check("bytes %s" % name, lambda cls=cls: bytes(cls()))
    check("bytearray %s" % name, lambda cls=cls: bytearray(cls()))

# __bytes__ wins over __index__, which is CPython's order.
class Both:
    def __bytes__(self):
        return b"from dunder"

    def __index__(self):
        return 3


check("both", lambda: bytes(Both()))

# bytearray has no __bytes__ arm in CPython -- it takes the __index__.
check("bytearray both", lambda: bytearray(Both()))

# An int subclass with an __index__ of its own still goes by its VALUE, because
# the int arm is reached first.
class Weird(int):
    def __index__(self):
        return 99


check("int subclass", lambda: bytes(Weird(2)))

# The four named types are unaffected, and a plain object with neither face
# still gets the conversion error.
class Nothing:
    pass


check("nothing", lambda: bytes(Nothing()))
check("plain int", lambda: bytes(3))
check("bool", lambda: bytes(True))
check("str no encoding", lambda: bytes("ab"))
check("list", lambda: bytes([1, 2, 3]))
check("bytes copy", lambda: bytes(b"ab"))
check("bytearray copy", lambda: bytes(bytearray(b"ab")))
check("memoryview", lambda: bytes(memoryview(b"ab")))
check("generator", lambda: bytes(x for x in (1, 2)))

# __index__ on the *element* side is a different arm and already worked.
check("index elements", lambda: bytes([Three(), Zero()]))

print("done")
