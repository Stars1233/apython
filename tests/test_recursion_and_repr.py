# There was no recursion counter anywhere in the interpreter: at roughly 320
# machine-stack bytes per Python call, `def f(n): return f(n+1)` died at about
# 25 000 frames with a bare SIGSEGV and no RecursionError.
#
# The container reprs had two further problems.  Only list_repr carried the
# cycle guard, so a self-referential dict, tuple or set recursed to a stack
# overflow.  And repr_pop is skipped by any raise from inside a nested
# __repr__, so repr_depth saturated at 64 and then *every* container repr in
# the process raised RecursionError -- with repr_stack holding 64 dangling
# pointers that the cycle check compares by address.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


def deep(n):
    return deep(n + 1)


print(t(lambda: deep(0)))

# The interpreter is still usable afterwards
print(sum(range(10)), len([1, 2, 3]))

# Cycles in every container render as CPython's markers
a = []
a.append(a)
print(repr(a))

d = {}
d["k"] = d
print(repr(d))

lst = []
tup = (lst,)
lst.append(tup)
print(repr(tup), repr(lst))

nested = {"outer": {}}
nested["outer"]["back"] = nested
print(repr(nested))

# A deep but finite nest renders, and leaves repr() working afterwards --
# this is the saturation case.  The nest is kept under apython's 64-entry
# repr_stack; CPython's own limit is far higher, which is a separate gap.
deep_list = []
cur = deep_list
for _ in range(50):
    nxt = []
    cur.append(nxt)
    cur = nxt
print(t(lambda: repr(deep_list)))
print(repr([1, 2]), repr({"a": 1}), repr((1,)), repr({1}))


# An exception raised inside __repr__ or __str__ must reach the caller at the
# call, not surface later at an unrelated instruction.  dunder_call_1 returns
# NULL both for "not defined" and for "it raised", and only the first is a
# fallback -- but current_exception cannot tell them apart on its own, since
# it stays set for the whole of an except block.
class Boom:
    def __repr__(self):
        raise ValueError("boom")


print(t(lambda: repr(Boom())))
print(t(lambda: str(Boom())))
print(t(lambda: repr([Boom()])))
print(t(lambda: repr({"k": Boom()})))
print(t(lambda: repr((Boom(),))))

# Repeatedly, to catch the saturation, and inside an except block, to catch
# the "already handling an exception" case
for _ in range(70):
    try:
        repr([Boom()])
    except ValueError:
        pass
print(repr([1, 2]))

try:
    raise KeyError("outer")
except KeyError:
    class Plain:
        pass
    print(len(repr(Plain())) > 0, t(lambda: repr(Boom())))


# A raising __len__ propagates, and len() no longer has a last-resort
# ob_size read -- for an iterator that field is it_seq, so len(reversed(x))
# used to return a heap address.
class BadLen:
    def __len__(self):
        raise ValueError("nolen")


print(t(lambda: len(BadLen())), t(lambda: len(reversed([1, 2, 3]))))
print(t(lambda: len(iter([1, 2]))), t(lambda: len(object())))
print(len([1, 2]), len("abc"), len({1: 2}), len({1, 2}), len((1,)), len(b"ab"))


# A descriptor __set__ that raises reaches the assignment, not a later line
class Failing:
    def __set__(self, obj, value):
        raise ValueError("setfail")

    def __get__(self, obj, objtype=None):
        return 1


class Holder:
    slot = Failing()


h = Holder()
try:
    h.slot = 5
except ValueError as e:
    print("set raised:", e)
print("done")
