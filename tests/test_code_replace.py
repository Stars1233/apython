"""code.replace(), and assigning a function's __code__.

`func.__code__ = co.replace(co_flags=co.co_flags | 0x100)` is the whole of
types.coroutine, and so the whole of `import asyncio`.  Neither half was
there: a code object had no replace(), and __code__ was read-only in effect
because an assignment landed in the function's __dict__ where nothing looked
for it.

Every argument to replace() is keyword-only, as CPython's is, and what is not
named is copied.
"""

import types


def sample(a, b=1):
    """a docstring"""
    return a + b


co = sample.__code__

print("--- what it copies ---")
new = co.replace()
print(new.co_name, new.co_argcount, new.co_flags == co.co_flags)
print(new.co_consts == co.co_consts, new.co_names == co.co_names)
print(new.co_filename == co.co_filename, new.co_firstlineno == co.co_firstlineno)

print("--- and what it changes ---")
print(co.replace(co_name="renamed").co_name, co.co_name)
print(co.replace(co_firstlineno=999).co_firstlineno, co.co_firstlineno)
print(co.replace(co_argcount=1).co_argcount, co.co_argcount)
flagged = co.replace(co_flags=co.co_flags | 0x100)
print(flagged.co_flags == co.co_flags | 0x100, co.co_flags & 0x100)
print(co.replace(co_stacksize=40).co_stacksize)
print(co.replace(co_qualname="q.name").co_qualname)
print(co.replace(co_consts=(None, 5)).co_consts)

print("--- several at once ---")
multi = co.replace(co_name="multi", co_firstlineno=7, co_flags=co.co_flags | 0x100)
print(multi.co_name, multi.co_firstlineno, multi.co_flags & 0x100 != 0)

print("--- the result still runs ---")


def target(a, b=1):
    return a * b


target.__code__ = co.replace(co_name="swapped")
print(target(3, 4), target.__code__.co_name)

print("--- types.coroutine, which is what all of it is for ---")


@types.coroutine
def gen():
    yield 1


print(gen.__code__.co_flags & 0x100 != 0)
print(list(gen()))

print("--- what it refuses ---")
for kw in ("nope", "co_notafield"):
    try:
        co.replace(**{kw: 1})
    except TypeError:
        print("TypeError for", kw)
try:
    co.replace(1)
except TypeError:
    print("TypeError for a positional argument")
try:
    co.replace(co_name=5)
except TypeError:
    print("TypeError for the wrong type")
try:
    sample.__code__ = "not a code object"
except TypeError as e:
    print("TypeError", e)

# The closure tuple belongs to the FUNCTION and COPY_FREE_VARS copies it into
# the frame by the NEW code's count, so a code object that closes over more
# names reads past the tuple.  CPython refuses the pair; so does this.


def one_free():
    a = 1

    def inner():
        return a

    return inner


def two_free():
    a, b = 1, 2

    def inner():
        return a + b

    return inner


for target_fn, source_fn in ((one_free(), two_free()),
                             (two_free(), one_free()),
                             (sample, two_free())):
    try:
        target_fn.__code__ = source_fn.__code__
        print("accepted")
    except ValueError as e:
        print("ValueError", e)
print(one_free()(), two_free()())

print("--- and the function still works after all that ---")
print(sample(2), sample.__code__.co_name, sample.__doc__)
print("done")
