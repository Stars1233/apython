# A Value may be tested for NULL and dereferenced when it is a pointer.
# A *payload* — what V_UNPACK yields — may not: the payload of the float 1.5
# is the raw bit pattern 0x3FF8000000000000, and the payload of a small int
# is the integer itself.  Code that checks for one non-pointer tag and then
# dereferences on the fall-through uses those bit patterns as an address.
#
# Every case below crashed the interpreter before these guards were added.


def raises(fn, *a):
    try:
        fn(*a)
    except TypeError as e:
        return "TypeError: " + str(e).split(",")[0]
    except Exception as e:
        return type(e).__name__ + ": " + str(e)
    return "no error"


# dict.get must hand dict_get a Value, not a decoded payload -----------------
d = {1: "one", 2: "two", 1.5: "float", "s": "str"}
print(d.get(1), d.get(2), d.get(1.5), d.get("s"))
print(d.get(99), d.get(99, "dflt"), d.get(3.5, "dflt"))

# A zero-valued hit is still a hit
z = {0: "zero", 0.0: "zerofloat"}
print(z.get(0), len(z))

# Keys of every immediate kind round-trip through setdefault/pop/in
e = {}
print(e.setdefault(7, "a"), e.setdefault(7, "b"), 7 in e, 8 in e)
print(e.pop(7), len(e))

# Non-integer subscripts raise instead of dereferencing ----------------------
for seq in ([1, 2, 3], (1, 2, 3), "abc", b"abc"):
    print(type(seq).__name__, raises(lambda s: s[1.5], seq))

# A str key is not an index either — int_to_i64 would read PyIntObject.compact
for seq in ([1, 2, 3], (1, 2, 3), "abc", b"abc"):
    print(type(seq).__name__, raises(lambda s: s["x"], seq))

# Valid indices still work, including negative and heap-int indices
big = 2 ** 60
lst = [10, 20, 30]
print(lst[0], lst[-1], (1, 2)[1], "abc"[2], b"abc"[1])
print(lst[True], raises(lambda: lst[big]))

# Slices are unaffected
print(lst[1:], (1, 2, 3)[:2], "abc"[::-1], b"abc"[1:])
