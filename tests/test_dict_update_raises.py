# dict.update() propagates what the mapping it is reading raises.
#
# The mapping path treated a NULL return from keys(), from materialising the
# key sequence, or from the mapping's own __getitem__ as "the loop is done":
# it fell through to the success tail and returned None with the exception
# still pending, so it surfaced later at an unrelated instruction with a
# partially updated dict behind it.
class KeysRaises:
    def keys(self):
        raise ValueError("keys blew up")


class KeysNotIterable:
    def keys(self):
        return 42


class GetitemRaises:
    def keys(self):
        return ["a", "b"]

    def __getitem__(self, k):
        if k == "b":
            raise RuntimeError("getitem blew up")
        return 1


class NoGetitem:
    def keys(self):
        return ["a"]


def attempt(label, src):
    d = {"pre": 0}
    try:
        d.update(src)
        print(label, "no error", sorted(d))
    except Exception as e:
        # The type, not the message: our wording for a malformed pair differs
        # from CPython's, and the point here is which exception escapes.
        print(label, type(e).__name__)


attempt("keys-raises", KeysRaises())
attempt("keys-not-iterable", KeysNotIterable())
attempt("getitem-raises", GetitemRaises())
attempt("no-getitem", NoGetitem())


# An iterable of pairs whose iterator raises.
class BadIter:
    def __iter__(self):
        return self

    def __next__(self):
        raise ValueError("iter blew up")


attempt("bad-iter", BadIter())


# A pair of the wrong length, and a non-iterable element.
attempt("short-pair", [("a", 1), ("b",)])
attempt("long-pair", [("a", 1), ("b", 2, 3)])
attempt("not-a-pair", [1, 2])


# The ordinary forms all still work.
d = {"a": 1}
d.update({"b": 2})
print(sorted(d.items()))
d.update([("c", 3)])
print(sorted(d.items()))
d.update(d=4)
print(sorted(d.items()))
d.update({"e": 5}, f=6)
print(sorted(d.items()))


class GoodMapping:
    def keys(self):
        return ["g"]

    def __getitem__(self, k):
        return 7


d.update(GoodMapping())
print(sorted(d.items()))
print(d.update() is None)


# A mappingproxy, which is what put the keys() path here in the first place.
class C:
    x = 1


d2 = {}
d2.update(C.__dict__)
print("x" in d2, d2["x"])
