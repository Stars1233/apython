"""co_lnotab, checked against the co_lines() it is derived from.

The two compilers lay out bytecode differently, so the BYTES cannot be
compared against CPython's.  What can be is the relationship: co_lnotab is a
pure function of co_lines() and co_firstlineno, and that function is written
here in Python and run on both.
"""

import types
import warnings


def expected(code):
    out = bytearray()
    prev_addr = 0
    line = code.co_firstlineno
    for start, _end, ln in code.co_lines():
        if ln is None or ln == line:
            continue
        bdelta = start - prev_addr
        ldelta = ln - line
        prev_addr = start
        line = ln
        while bdelta > 255:
            out += bytes((255, 0))
            bdelta -= 255
        while ldelta > 127:
            out += bytes((bdelta, 127))
            bdelta = 0
            ldelta -= 127
        while ldelta < -128:
            out += bytes((bdelta, 128))
            bdelta = 0
            ldelta += 128
        out += bytes((bdelta, ldelta & 0xff))
    return bytes(out)


def walk(code):
    yield code
    for k in code.co_consts:
        if isinstance(k, types.CodeType):
            yield from walk(k)


SRC = '''
def f(a):
    x = 1
    if a:
        x = 2
    else:
        x = 3
    return x


class C:
    """doc"""

    def m(self):
        try:
            return [i
                    for i in range(3)
                    if i]
        except ValueError:
            raise


async def g():
    yield 1


def backwards():
    for i in range(3):
        if i:
            continue
        pass
    return i
'''


def main():
    warnings.simplefilter("ignore")

    top = compile(SRC, "<lnotab>", "exec")
    codes = list(walk(top))
    print("codes:", len(codes) >= 6)
    print("all match:", all(c.co_lnotab == expected(c) for c in codes))
    print("bytes:", all(isinstance(c.co_lnotab, bytes) for c in codes))
    print("even:", all(len(c.co_lnotab) % 2 == 0 for c in codes))

    # A body that spans more than 127 lines needs the split form.
    wide = compile("x = 1\n" + "\n" * 400 + "y = 2\n", "<wide>", "exec")
    print("wide:", wide.co_lnotab == expected(wide))
    print("wide split:", len(wide.co_lnotab) > 2)

    # And one that goes backwards by more than 128.
    back = compile(
        "def a():\n    pass\n" + "\n" * 300 + "def b():\n    pass\n"
        "a()\n",
        "<back>", "exec")
    print("back:", all(c.co_lnotab == expected(c) for c in walk(back)))

    # An empty body has an empty table.
    print("empty:", compile("", "<e>", "exec").co_lnotab == b"")

    # Reading it twice gives equal, freshly built bytes.
    one = top.co_lnotab
    two = top.co_lnotab
    print("stable:", one == two)


main()
