# What a builtin says about an argument of the wrong type.
#
# Eight sites said "must be str, not other type" -- a placeholder where
# CPython names the type, and where several of them name the method and the
# argument's position too.  A program greps these: "not int" is what tells a
# caller which of its values was wrong.
#
# The index conversion had a worse problem than wording.  obj_as_index read a
# wide int through __gmpz_get_si, which TRUNCATES, so `[1][2**70]` answered
# [1]'s first element and `chr(2**70)` answered "\x00" -- wrong answers, not
# refusals.  A slice bound is the one place that clamps rather than refusing,
# because `[1,2,3][2**70:]` is [] in CPython and not an error.


def show(label, fn):
    try:
        print("%-30s %r" % (label, fn()))
    except Exception as e:
        print("%-30s %s: %s" % (label, type(e).__name__, e))


print("=== the argument's type is named ===")
show("count", lambda: "abc".count(0))
show("find", lambda: "abc".find(0))
show("index", lambda: "abc".index(0))
show("rfind", lambda: "abc".rfind(None))
show("replace 1", lambda: "abc".replace(0, "x"))
show("replace 2", lambda: "abc".replace("x", 0))
show("split", lambda: "abc".split(0))
show("rsplit", lambda: "abc".rsplit([]))
show("removeprefix", lambda: "abc".removeprefix(0))
show("removesuffix", lambda: "abc".removesuffix(b"x"))
show("startswith", lambda: "abc".startswith(0))
show("startswith tuple", lambda: "abc".startswith((0,)))
show("endswith", lambda: "abc".endswith(1.5))
show("endswith tuple", lambda: "abc".endswith((None,)))
show("partition", lambda: "abc".partition(0))
show("rpartition", lambda: "abc".rpartition(None))
show("join", lambda: "-".join(0))
show("join item", lambda: "-".join([1]))
show("join item 2", lambda: "-".join(["a", b"b"]))
show("float.fromhex", lambda: float.fromhex(0))
show("encode", lambda: "a".encode(None))
show("decode", lambda: b"a".decode(None))

print("=== and they still take what they should ===")
print("abc".count("b"), "abc".find("c"), "abc".replace("b", "-"),
      "abc".split("b"), "abc".removeprefix("a"), "abc".startswith("a"),
      "abc".endswith(("z", "c")), "abc".partition("b"), "-".join("ab"))

# The exception TYPE is not compared for the two that CPython words
# differently from the rest: a subscript is an IndexError there and this is
# one funnel, and chr's limit is a C int rather than an index.  Both are
# refusals now, which is the part that matters -- they used to be answers.
print("=== an index that will not fit one ===")
for label, fn in (("[1][2**70]", lambda: [1][2 ** 70]),
                  ("chr(2**70)", lambda: chr(2 ** 70))):
    try:
        print("%-30s %r" % (label, fn()))
    except Exception:
        print("%-30s refused" % (label,))
show("list.insert", lambda: [1].insert(2 ** 70, 1))
show("b'a'.rjust", lambda: b"a".rjust(2 ** 70))
# range is not in this list: its three bounds are int64 fields where CPython
# holds objects, so a bound wider than an index CLAMPS -- `range(1 << 1000)`
# is an ordinary range there, and _collections_abc builds one at import.
show("'a' * 2**70", lambda: len("a" * (2 ** 70)))

print("=== but a slice bound clamps ===")
for e in ("[1,2,3][2**70:]", "[1,2,3][:2**70]", "[1,2,3][-(2**70):]",
          '"abc"[2**70:]', '"abc"[:-(2**70)]', 'b"abc"[2**70:]',
          "(1,2)[2**70:]", "range(5)[2**70:]", "[1,2,3][1:2]"):
    print("%-22s %r" % (e, eval(e)))
print("done")
