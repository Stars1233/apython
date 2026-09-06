# repr() of a long bytes or bytearray.
#
# It used to render into a 1024-byte buffer on the stack with a hard `if
# outpos >= 1000: stop` in the loop, so any object whose repr ran past that was
# silently CUT SHORT -- no exception, no ellipsis, just a string that ends in
# the middle and still carries a closing quote, so it looks well-formed.
#
# The sizes here straddle that limit from both directions, and each case is
# checked by LENGTH and by round-trip through eval-free reconstruction rather
# than by eyeballing the text: a truncated repr is a plausible-looking repr.
#
# The worst case is four output bytes per input byte (\xHH), so a 250-byte
# object was already past the old limit if none of it was printable -- which is
# why the non-printable cases start much smaller than the printable ones.


def show(b):
    r = repr(b)
    print(len(b), len(r), r[:12], r[-12:])
    return r


def printable_lengths():
    # One output byte per input byte, so the old limit bit at about 1000.
    for n in (0, 1, 100, 900, 990, 995, 996, 997, 1000, 1001, 1024, 2000,
              5000):
        show(b"x" * n)


def non_printable_lengths():
    # Four output bytes per input byte, so the old limit bit at about 250.
    for n in (0, 1, 100, 240, 246, 247, 248, 250, 256, 300, 1000, 2000):
        show(bytes([0x80]) * n)
        show(bytes([0xff]) * n)


def mixed():
    # Every byte value, several times over: printable runs, the short escapes
    # (\n \r \t \0), and \xHH, in one object.
    b = bytes(range(256))
    show(b)
    show(b * 2)
    show(b * 8)
    show(b * 40)
    # The short escapes on their own.
    show(b"\n\r\t\0" * 500)
    show(b"\\" * 800)


def quote_choice():
    # The delimiter is chosen by scanning the whole object, so the choice has
    # to survive past the old limit too.
    show(b"x" * 2000 + b"'")
    show(b"x" * 2000 + b'"')
    show(b"x" * 2000 + b"'" + b'"')
    show(b"'" + b"x" * 2000)
    show(b'"' + b"x" * 2000)


def bytearrays():
    # bytearray's repr carries a 10-character prefix and a closing paren, so
    # it hits the limit sooner and its wrapper is what gets cut first.
    for n in (0, 1, 985, 990, 1000, 2000, 5000):
        show(bytearray(b"x" * n))
    show(bytearray(bytes(range(256)) * 8))
    show(bytearray(b"'" + b"y" * 2000))
    show(bytearray(b"\n" * 1000))


def structure_is_intact():
    # A repr that was cut short still ends in a quote, so the check has to be
    # on the CONTENT: every byte must be accounted for.
    for b in (b"x" * 3000, bytes(range(256)) * 20, b"\xff" * 1000,
              b"a'b" * 700):
        r = repr(b)
        print(r.startswith("b'") or r.startswith('b"'),
              r.endswith("'") or r.endswith('"'), len(r))
        # Count the escapes: each \xHH is four characters, each \\ or \' two.
        print(r.count("\\x"), r.count("\\\\"), r.count("\\'"), r.count("\\n"))
    for b in (bytearray(b"z" * 3000), bytearray(bytes(range(256)) * 20)):
        r = repr(b)
        print(r.startswith("bytearray(b"), r.endswith(")"), len(r))


def str_and_format():
    # str() and %-format and f-strings all go through the same renderer.
    b = bytes(range(256)) * 4
    print(len(str(b)), len("%r" % b), len(f"{b!r}"), str(b) == repr(b))
    a = bytearray(b"q" * 2000)
    print(len(str(a)), len("%r" % a), str(a) == repr(a))
    print(len(repr([b"x" * 1500])), len(repr({b"k" * 1500: 1})))
    print(len(repr((b"t" * 1200, bytearray(b"u" * 1200)))))


printable_lengths()
non_printable_lengths()
mixed()
quote_choice()
bytearrays()
structure_is_intact()
str_and_format()
