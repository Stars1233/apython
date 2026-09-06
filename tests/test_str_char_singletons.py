# The 256 one-character latin-1 strings are shared objects, so `s[i]` and
# `for ch in s` hand back a pointer instead of allocating.  latin-1 rather
# than ASCII because that is the range CPython's own singletons cover, and
# these are diffed against CPython's answers.
#
# Sharing makes identity observable, and that is what most of this file is:
# `s[0] is s[0]` must now be True, and it must be True for the SAME character
# reached different ways -- indexing, iterating, chr(), a slice, a case
# mapping.  CPython's answers are the same because it keeps the identical
# latin-1 singletons, so the harness's diff against python3 is the check.
#
# The two things that must NOT become identical are a character above U+00FF,
# which has no singleton and is allocated as before, and a str subclass, which
# has to keep its own type.
#
# The objects are immortal, so the other risk is a refcount one: a singleton
# handed out a million times and released a million times must still be there
# at the end, and it must never be resized by the in-place `+=` path, which
# needs a refcount of exactly 2 and can never see one on an object whose count
# starts at 0x7fffffffffffffff.


def identity_ascii():
    s = "abcdefghij"
    print(s[0] is s[0], s[0] is s[0:1], s[0] is chr(97))
    print(list(s)[0] is s[0], [c for c in s][3] is s[3])
    # NOT compared against a literal: our string constants are not interned,
    # so `s[0] is "a"` is False here and True in CPython for a reason that has
    # nothing to do with the singletons.  Both sides of every `is` below are
    # produced at run time.
    print(s[0] is "abc"[0:1], (s + "")[0] is s[0])
    # Every ASCII code point, reached two ways.
    same = all(chr(i) is chr(i) for i in range(128))
    print(same)
    same2 = all(("z" + chr(i))[1] is chr(i) for i in range(1, 128))
    print(same2)


def identity_wide():
    # No singleton exists for these, so each is its own object.
    w = "é中\U0001f600"
    print(w[0] is w[0], w[1] is w[1], w[0] == w[0])
    print(chr(200) is chr(200), chr(0x4e2d) is chr(0x4e2d))
    # But they must still be EQUAL, and the right length.
    print(w[0] == "é", w[1] == "中", w[2] == "\U0001f600")
    print([len(c) for c in w], [len(c.encode()) for c in w])


def contents():
    s = "abcdefghij"
    print([s[i] for i in range(10)])
    print([s[-i] for i in range(1, 11)])
    print(list(s), "".join(list(s)) == s)
    every = "".join(chr(i) for i in range(1, 128))
    print(len(every), [every[i] == chr(i + 1) for i in range(127)].count(True))
    print(list(every)[:5], list(every)[-3:])
    print("".join(list(every)) == every)


def wide_indexing():
    w = "aé中b\U0001f600c"
    print(len(w), len(w.encode()))
    for i in range(len(w)):
        print(i, repr(w[i]), ord(w[i]), len(w[i]), len(w[i].encode()))
    for i in range(1, len(w) + 1):
        print(-i, repr(w[-i]), ord(w[-i]))
    print(list(w), [ord(c) for c in w])
    print("".join(w[i] for i in range(len(w))) == w)


def errors():
    for s in ("", "abc", "é"):
        for i in (len(s), len(s) + 1, -len(s) - 1, 100, -100):
            try:
                s[i]
            except IndexError as e:
                print(repr(s), i, "IndexError", e)


def hashing_and_containers():
    # A singleton's hash is cached in the shared object, so the first taker
    # fills it in for everyone.  It must still be the hash of the string.
    s = "abcdefghij"
    print(hash(s[0]) == hash("a"), hash(s[0]) == hash(s[0]))
    d = {}
    for c in "abcabcabc":
        d[c] = d.get(c, 0) + 1
    print(sorted(d.items()))
    print({s[0]: 1}["a"], "a" in {s[0]}, set("aabbcc") == set("abc"))
    print(sorted(set("hello world")))


def immortality():
    # Handed out and released many times over; the object has to survive it.
    s = "abcdefghij"
    for _ in range(20000):
        c = s[0]
    print(s[0] == "a", s[0] is c, len(s[0]))
    # And through iteration, which is the other producer.
    n = 0
    for _ in range(2000):
        for ch in s:
            n += 1
    print(n, s[0] == "a", "".join(s[i] for i in range(10)) == s)


def not_resizable():
    # The in-place append needs a refcount of exactly 2; a singleton's count
    # starts immortal, so it can never be taken.  If it ever were, the shared
    # object would grow and every other holder would see it.
    c = "abcdefghij"[0]
    acc = c
    for _ in range(5):
        acc += "b"
    print(acc, c, len(c), "abcdefghij"[0], chr(97))
    d = c
    d += "z"
    print(d, c, len(c), c == "a")


def subclass_and_type():
    class S(str):
        pass

    v = S("abc")
    print(v[0], type(v[0]).__name__, v[0] is list("abc")[0])
    print(list(v), [type(x).__name__ for x in v])
    print(S("a") is S("a"), S("a") == "a", len(S("a")))


def slices_and_methods():
    s = "AbCdEf"
    print(s[0:1], s[1:2], s[0:1] is s[0], s[-1:] is s[-1])
    print(s.lower()[0], s.upper()[0], s.lower()[0] is s.lower()[0])
    print(s[0].lower() is s[0].lower(), s[0].upper() is s[0])
    print("a".join([]), "-".join(list("abc")))
    print(max("hello"), min("hello"), max("hello") is max("hello"))
    print(sorted("hello"), "".join(sorted("hello")))


def nul_and_edges():
    print(repr(chr(0)), len(chr(0)), chr(0) is chr(0))
    s = "a\0b"
    print([repr(c) for c in s], s[1] is chr(0), len(s[1]))
    print(repr(chr(127)), chr(127) is chr(127), ord(chr(127)))
    print(repr(chr(128)), len(chr(128)), len(chr(128).encode()),
          chr(128) is chr(128))
    print(repr(chr(255)), len(chr(255).encode()))


identity_ascii()
identity_wide()
contents()
wide_indexing()
errors()
hashing_and_containers()
immortality()
not_resizable()
subclass_and_type()
slices_and_methods()
nul_and_edges()
