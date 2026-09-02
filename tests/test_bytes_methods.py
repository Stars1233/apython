# Test bytes methods

# hex
print(b''.hex())              # (empty string)
print(b'\xab\xcd'.hex())      # abcd
print(b'hello'.hex())         # 68656c6c6f

# startswith
print(b'hello'.startswith(b'hel'))   # True
print(b'hello'.startswith(b'xyz'))   # False
print(b'hello'.startswith(b''))      # True
print(b'hi'.startswith(b'hello'))    # False

# endswith
print(b'hello'.endswith(b'llo'))     # True
print(b'hello'.endswith(b'xyz'))     # False
print(b'hello'.endswith(b''))        # True
print(b'hi'.endswith(b'hello'))      # False

# count
print(b'hello'.count(b'l'))         # 2
print(b'hello'.count(b'x'))         # 0
print(b'aaa'.count(b'aa'))          # 1 (non-overlapping)
print(b'hello'.count(b''))          # 6 (len+1)

# find
print(b'hello'.find(b'l'))          # 2
print(b'hello'.find(b'x'))          # -1
print(b'hello'.find(b''))           # 0
print(b'hello'.find(b'lo'))         # 3

# replace
print(b'hello world'.replace(b'world', b'there'))  # b'hello there'
print(b'hello'.replace(b'l', b'L'))                 # b'heLLo'
print(b'hello'.replace(b'x', b'y'))                 # b'hello'
print(b'hello'.replace(b'o', b''))                  # b'hell'
print(b'aaa'.replace(b'a', b'bb'))                  # b'bbbbbb'

# split
print(b'hello world foo'.split())            # [b'hello', b'world', b'foo']
print(b'a,b,c'.split(b','))                  # [b'a', b'b', b'c']
print(b'  hello  '.split())                  # [b'hello']
print(b'abc'.split(b'x'))                    # [b'abc']
print(b'a::b::c'.split(b'::'))              # [b'a', b'b', b'c']

# join
print(b', '.join([b'a', b'b', b'c']))       # b'a, b, c'
print(b''.join([b'a', b'b']))                # b'ab'
print(b'-'.join([b'hello']))                 # b'hello'
print(b' '.join([]))                         # b''


# --- rfind, index, rindex, the strip family, partition and rpartition --------
#
# bytes had find and nothing else of the search family, and none of the strip
# or partition ones.  find, rfind, index and rindex are one body now: the two
# directions differ only in where the scan starts, and index and rindex differ
# from the first two only in answering a miss with a ValueError.  bytearray
# reaches all of them through the shared-call trampoline, so the two stay one
# implementation.

def t(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "=> RAISE", type(e).__name__)

d = b"abcabc"
for n in (b"a", b"c", b"abc", b"z", b"", b"bc"):
    print(n, d.find(n), d.rfind(n), d.index(n) if n in d else "-", d.rindex(n) if n in d else "-")
t("index missing", lambda: d.index(b"z"))
t("rindex missing", lambda: d.rindex(b"z"))
print(d.find(b"a", 1), d.rfind(b"a", 0, 3), d.index(b"a", 1), d.rindex(b"a", 0, 4))
print(d.find(97), d.rfind(97), d.index(99), d.rindex(99))
b = bytearray(b"abcabc")
print(b.rfind(b"a"), b.index(b"b"), b.rindex(b"c"), b.find(b"z"))
print(b"".rfind(b""), b"".find(b""), b"ab".rfind(b""), b"ab".find(b""))

for d in (b"  ab  ", b"\t\nab\r\n", b"ab", b"", b"   ", b"xxabxx"):
    print(d, d.strip(), d.lstrip(), d.rstrip())
for d, c in ((b"xxabxx", b"x"), (b"abcba", b"ab"), (b"abc", b"z"), (b"aaa", b"a")):
    print(d, c, d.strip(c), d.lstrip(c), d.rstrip(c))
print(b"  ab  ".strip(None), b"ab".strip(b""))
for d, sep in ((b"a,b,c", b","), (b"abc", b","), (b",abc", b","), (b"abc,", b","),
               (b"a,b", b"a,b"), (b"aXXbXXc", b"XX")):
    print(d, sep, d.partition(sep), d.rpartition(sep))
t("empty sep", lambda: b"abc".partition(b""))
b = bytearray(b"  ab  ")
print(b.strip(), b.lstrip(), b.rstrip(), type(b.strip()).__name__)
ba = bytearray(b"a,b")
print(ba.partition(b","), [type(x).__name__ for x in ba.partition(b",")])
