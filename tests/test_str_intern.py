# String interning, across a module boundary.
#
# A single file proves almost nothing here: everything in one .pyc already
# shares through marshal's FLAG_REF back-references, so `A is B` inside one
# module can be True with no intern table at all.  The question is whether two
# SEPARATELY compiled modules end up with one object or two, and only
# `internhelper` can ask it.
#
# The rows that must be True are the point.  The rows that must be FALSE are
# the guard rail: over-interning is invisible until two modules' unrelated long
# strings turn out to share an object, and by then `is` has become a lie
# everywhere.  CPython's rule -- a NAME always, a string CONSTANT only when
# every byte is [A-Za-z0-9_] -- is what draws the line, and the harness diffs
# against CPython, so CPython's answers are the specification.
#
# The growth test at the end is the nastiest failure mode this can have: a
# resize that silently drops entries makes `is` depend on how many strings
# happen to have been interned first, so it is load-dependent and would come
# and go with unrelated changes.

import sys

import internhelper


def shared_constants():
    # All-name-chars constants, written in both modules.  The literals are
    # bound to locals first only so that python3 does not emit a SyntaxWarning
    # for `is` against a literal; the object is the same either way, since a
    # local load hands back the very constant from co_consts.
    hello = "hello"
    underscored = "_private_name"
    digits = "abc123"
    empty = ""
    print(internhelper.NAME is hello)
    print(internhelper.UNDERSCORED is underscored)
    print(internhelper.DIGITS is digits)
    print(internhelper.EMPTY is empty)


def not_shared_constants():
    # These must stay separate objects: a space, punctuation, non-ASCII.
    phrase = "hello world"
    punct = "hello!"
    accent = "été"
    print(internhelper.PHRASE is phrase)
    print(internhelper.PUNCT is punct)
    print(internhelper.ACCENT is accent)
    # And they must still compare equal.
    print(internhelper.PHRASE == "hello world",
          internhelper.PUNCT == "hello!",
          internhelper.ACCENT == "été")


def runtime_strings_are_not_interned():
    # A string built at run time is nobody's constant, so it is its own object
    # until something interns it.
    x = "a"
    ab = "ab"
    print((x + "b") is ab)
    print("".join(["h", "e", "l", "l", "o"]) is internhelper.NAME)
    print(("hell" + "o") == internhelper.NAME)


def sys_intern_identity():
    foo = "foo"
    print(sys.intern("foo") is foo)
    print(sys.intern("hello") is internhelper.NAME)
    # sys.intern on a string nothing has interned yet returns that very object.
    made = "".join(["u", "n", "s", "e", "e", "n", "w", "o", "r", "d"])
    print(sys.intern(made) is made)
    # And a second, equal string then resolves to the first.
    again = "".join(["u", "n", "s", "e", "e", "n", "w", "o", "r", "d"])
    print(sys.intern(again) is made, again is made)
    # Even for something the constant rule would never have taken.
    phrase = "".join(["a", " ", "b"])
    print(sys.intern(phrase) is phrase)
    print(sys.intern("".join(["a", " ", "b"])) is phrase)

    class S(str):
        pass

    try:
        sys.intern(S("sub"))
    except TypeError:
        print("TypeError on subclass")


def attribute_names_across_modules():
    # The reason any of this exists: an attribute written by __init__ and read
    # by another method, in a module compiled on its own.
    h = internhelper.make()
    print(h.read(), h.shared_attr, h.other)
    for _ in range(200):
        h.shared_attr = h.shared_attr + 1
    print(h.read(), sorted(h.__dict__.keys()))
    # The name reached through getattr, built at run time, still finds it.
    print(getattr(h, "shared" + "_attr"), "shared_attr" in h.__dict__)


def dict_and_set_behaviour():
    # Interning must not change what a dict or a set answers, only how fast.
    d = {internhelper.NAME: 1, "hello": 2}
    print(len(d), d["hello"])
    e = {internhelper.PHRASE: 1, "hello world": 2}
    print(len(e), e["hello world"])
    s = {internhelper.NAME, "hello", "".join(["h", "e", "l", "l", "o"])}
    print(len(s), sorted(s))
    print(hash(internhelper.NAME) == hash("hello"))


def growth():
    # Many distinct names, each still resolving to the same object on a second
    # intern.  A resize that drops entries shows up here and nowhere else.
    n = 20000
    first = [sys.intern("name_%d" % i) for i in range(n)]
    ok = True
    for i in range(n):
        if sys.intern("name_%d" % i) is not first[i]:
            ok = False
            break
    print(ok, len(first), len(set(first)))
    # And the values are all still intact.
    print(first[0], first[n - 1], first[n // 2])
    print(all(first[i] == "name_%d" % i for i in range(0, n, 977)))
    # Strings interned before the growth still resolve.
    print(sys.intern("hello") is internhelper.NAME)
    foo = "foo"
    print(sys.intern("foo") is foo)


shared_constants()
not_shared_constants()
runtime_strings_are_not_interned()
sys_intern_identity()
attribute_names_across_modules()
dict_and_set_behaviour()
growth()
