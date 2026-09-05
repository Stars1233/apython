# Every builtin method, handed arguments of the wrong shape.
#
# A method that reads its argument as the type it expected -- without asking
# what it actually is -- dereferences a small integer's Value as a pointer.
# str.center did, and str.partition, and float.fromhex; each was a segfault
# from one line of ordinary Python.  What this asserts is that the process is
# still here at the end, which is the only thing that can be compared: CPython
# and this implementation raise for different reasons in places, and the point
# is not to match the reason.
#
# The method list is FIXED rather than taken from dir(), so both sides make
# exactly the same calls and the count at the end is comparable.

BAD = [None, 0, -1, 2 ** 70, -(2 ** 70), 1.5, float("nan"), float("inf"),
       "", "x", "xy", b"", b"x", bytearray(b"x"), [], [1], (), (1,), {},
       {"a": 1}, set(), object(), type, slice(None), range(3), Ellipsis,
       True, complex(1, 2), memoryview(b"ab")]

SUBJECTS = [
    ("abc", ["center", "ljust", "rjust", "zfill", "expandtabs", "partition",
             "rpartition", "split", "rsplit", "join", "replace", "startswith",
             "endswith", "strip", "lstrip", "rstrip", "count", "find",
             "index", "rfind", "rindex", "removeprefix", "removesuffix",
             "encode", "format", "format_map", "translate", "maketrans",
             "splitlines", "__getitem__", "__contains__", "__add__",
             "__mul__", "__mod__", "__eq__", "__lt__"]),
    (b"abc", ["center", "ljust", "rjust", "zfill", "expandtabs", "partition",
              "rpartition", "split", "rsplit", "join", "replace",
              "startswith", "endswith", "strip", "count", "find", "index",
              "decode", "translate", "hex", "removeprefix",
              "__getitem__", "__contains__", "__add__", "__mul__", "__mod__"]),
    (bytearray(b"abc"), ["center", "ljust", "rjust", "append", "extend",
                         "insert", "remove", "pop", "find", "index",
                         "replace", "join", "translate", "hex",
                         "__getitem__", "__setitem__", "__contains__",
                         "__add__", "__mul__"]),
    ([1, 2, 3], ["append", "extend", "insert", "remove", "pop", "index",
                 "count", "sort", "__getitem__", "__setitem__",
                 "__contains__", "__add__", "__mul__"]),
    ((1, 2, 3), ["index", "count", "__getitem__", "__contains__", "__add__",
                 "__mul__"]),
    ({"a": 1}, ["get", "pop", "setdefault", "update", "fromkeys",
                "__getitem__", "__setitem__", "__contains__"]),
    ({1, 2}, ["add", "discard", "remove", "union", "intersection",
              "difference", "symmetric_difference", "issubset",
              "issuperset", "update", "__contains__", "__or__", "__and__"]),
    (frozenset({1}), ["union", "intersection", "difference", "issubset",
                      "__contains__", "__or__"]),
    # __pow__, __lshift__ and __round__ are deliberately absent: `7 ** 2**70`
    # and `round(7, -(2**70))` are not bad arguments, they are very long
    # calculations, and both sides would sit in them.
    (7, ["to_bytes", "bit_length", "__add__", "__mul__", "__and__", "__eq__",
         "__lt__"]),
    (1.5, ["hex", "is_integer", "__add__", "__mul__", "__eq__", "__lt__"]),
    (complex(1, 2), ["conjugate", "__add__", "__mul__", "__eq__"]),
    (memoryview(bytearray(b"abcd")), ["cast", "tobytes", "tolist", "hex",
                                      "__getitem__", "__setitem__",
                                      "__contains__"]),
    (range(4), ["index", "count", "__getitem__", "__contains__"]),
    (slice(1, 2), ["indices"]),
]

CLASSMETHODS = [(int, "from_bytes"), (float, "fromhex"),
                (bytes, "fromhex"), (bytearray, "fromhex"),
                (dict, "fromkeys"), (str, "maketrans")]

calls = 0
raised = 0
for subject, names in SUBJECTS:
    for name in names:
        try:
            method = getattr(subject, name)
        except Exception:
            continue
        for a in BAD:
            for args in ((a,), (a, a)):
                calls += 1
                try:
                    method(*args)
                except BaseException:
                    raised += 1

for cls, name in CLASSMETHODS:
    method = getattr(cls, name)
    for a in BAD:
        for args in ((a,), (a, a)):
            calls += 1
            try:
                method(*args)
            except BaseException:
                raised += 1

print("made", calls, "calls")
print("still here")

# ...and the same methods still work on arguments that ARE the right shape.
print("abc".center(7, "-"), "abc".partition("b"), "abc".ljust(5, "."))
print(b"abc".center(7, b"-"), b"abc".partition(b"b"))
print(float.fromhex("0x1.8p+0"), (1.5).hex(), int.from_bytes(b"\x01\x02", "big"))
print("a\tb".expandtabs(4), "7".zfill(3), dict.fromkeys("ab", 0))
print("done")
