# dict.__getitem__ called as an unbound function.
#
# It was a bare tail-jump into dict_subscript with no arity check and no type
# check, and builtin_func_call validates neither for it -- so
# dict.__getitem__(5, "a") handed the immediate integer 5 to dict_get as a
# pointer and dereferenced it.  Its two siblings, __setitem__ and __delitem__,
# have both guards; this copies them.
d = {"a": 1, "b": 2}

print(dict.__getitem__(d, "a"))
print(dict.__setitem__(d, "c", 3), d["c"])
print(dict.__delitem__(d, "c"), "c" in d)

for call in (
    lambda: dict.__getitem__(5, "a"),
    lambda: dict.__getitem__("str", "a"),
    lambda: dict.__getitem__([], "a"),
    lambda: dict.__getitem__(None, "a"),
    lambda: dict.__getitem__(1.5, "a"),
    lambda: dict.__getitem__(d),
    lambda: dict.__getitem__(),
    lambda: dict.__getitem__(d, "a", "b"),
):
    try:
        call()
        print("no error")
    except TypeError:
        print("TypeError")
    except KeyError:
        print("KeyError")

# A missing key still raises KeyError with the key as its argument.
try:
    dict.__getitem__(d, "zz")
except KeyError as e:
    print("KeyError", e.args)


# A dict subclass is still accepted -- that is what REQUIRE_DICT_TYPE is for,
# and it is how super().__getitem__ is written.
class D(dict):
    def __getitem__(self, k):
        return "via " + str(dict.__getitem__(self, k))


sub = D()
sub["k"] = 9
print(sub["k"], dict.__getitem__(sub, "k"))
