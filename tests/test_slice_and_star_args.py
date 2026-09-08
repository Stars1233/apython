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


# A star in an ARGUMENT list takes a whole expression.  CPython has two
# productions: `star_expressions` for a display, an assignment target and a for
# target, whose star takes a bitwise_or, and `starred_expression` for a call,
# which admits or/and/not, a comparison and a ternary.  One binding power
# cannot say both, and this was parsed with the display's -- so `f(*() or ())`,
# which CPython's own test_grammar exercises, was a SyntaxError.
def d(*a, **k):
    return a, sorted(k.items())


print(d(*() or (1,)))
print(d(*[] or [2]))
print(d(*() or (), *{} and (), **() or {}))
print(d(**{"a": 2} or {}))
print(d(*(1, 2) if True else ()))
print(d(*[1] if 1 else [2], **{"k": 1} if 1 else {}))
print(d(*[1] if 0 else [2, 3]))
print(d(*(x for x in (1, 2))))

# A class statement's bases share the argument parser, so it follows.
BASES = ()


class C(*BASES or (object,)):
    pass


print(C.__mro__[-1] is object)

# The display and target forms are unchanged: the star there stops before
# `or`, which is what makes `[*a or b]` a SyntaxError in both.
try:
    exec("[*a or b]")
    print("NOT REFUSED")
except SyntaxError:
    print("display star still refused")

for first, *rest in [(1, 2, 3)]:
    print(first, rest)

head, *tail = [1, 2, 3]
print(head, tail)


# A dict takes any hashable key, and DictEntry.key is a full Value -- so an
# int key in a `**` spread put an IMMEDIATE in the names tuple and the INCREF
# that gives the tuple its reference wrote through the number itself.
# `print(**{1: 2})` was one line of Python and a segfault; CPython refuses a
# non-string keyword before anything else happens, and now so does this.
class StrSub(str):
    pass


def kw(**k):
    return sorted(k.items())


for mapping in ({1: 2}, {1.5: 2}, {None: 2}, {(1,): 2}, {True: 2},
                {"a": 1, 2: 3}, {b"a": 1}):
    try:
        print(kw(**mapping))
    except TypeError as e:
        print("refused:", e)

print(kw(**{}), kw(**{"a": 1}), kw(**{StrSub("b"): 2}))
print(kw(**{"a": 1}, **{"b": 2}))

# The refusal happens for any callable, and before the callable is entered.
entered = []


def records(**k):
    entered.append(1)
    return k


try:
    records(**{1: 2})
except TypeError:
    pass
print("callee not entered:", entered == [])
