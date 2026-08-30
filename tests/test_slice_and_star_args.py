# BINARY_SLICE and STORE_SLICE performed three unguarded loads in a row --
# ob_type on an unvalidated Value, then tp_as_mapping, then mp_subscript --
# so 5[0:1] and obj[1:2] on any user class were both SIGSEGV.
#
# CALL_FUNCTION_EX assumed "not a tuple" meant "a list" and read [obj+16] as
# the argument count and [obj+32] as the Value array.  CPython emits that
# opcode with whatever iterable the * was applied to, so f(*aset), f(*"ab"),
# f(*gen) and f(*range(3)) all corrupted memory, and f(*5) dereferenced the
# payload.


def raises(fn, *a):
    try:
        fn(*a)
    except Exception as e:
        return type(e).__name__
    return "no error"


class Sliceable:
    def __getitem__(self, k):
        return ("get", k.start, k.stop, k.step)

    def __setitem__(self, k, v):
        print("set", k.start, k.stop, v)


s = Sliceable()
print(s[1:2], s[::2], s[:])
s[1:2] = 9

# Builtin sequences are unaffected
print([1, 2, 3][0:2], "abcd"[1:3], (1, 2, 3)[:2], b"abcd"[1:3])
lst = [1, 2, 3]
lst[0:2] = [9, 8]
print(lst)

# Immediates and objects with no mapping protocol raise
for v in (5, 1.5, None, True):
    print(raises(lambda x: x[0:1], v), end=" ")
print()


def store(x):
    x[0:1] = 1


print([raises(store, v) for v in (5, 1.5, None)])


# Every iterable is a valid * argument, not just tuple and list
def f(*a):
    return a


print(f(*[1, 2]), f(*(3, 4)), f(*"ab"))
print(f(*{5}), f(*range(3)), f(*(i * i for i in range(3))))
print(f(*{"k": 1, "j": 2}), f(*[]), f(*()))
print([raises(lambda v: f(*v), v) for v in (5, 1.5, None, True)])


def g(a, b=0, **k):
    return (a, b, k)


print(g(*[1], **{"b": 2, "c": 3}))
print(g(*"x"), g(*(1, 2)))
