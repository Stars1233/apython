# next(it) hands on whatever __next__ raised.
#
# The builtin manufactured a StopIteration over the top of the real exception,
# so `next(it)` turned a ValueError from __next__ into an exhausted iterator
# while `it.__next__()` and `for x in it` both propagated it.  Both forms of
# next() -- with and without a default -- have to tell "raised" from "ended".

class Boom:
    def __iter__(self):
        return self
    def __next__(self):
        raise ValueError("boom")

class Done:
    def __iter__(self):
        return self
    def __next__(self):
        raise StopIteration

class Counter:
    def __init__(self):
        self.n = 0
    def __iter__(self):
        return self
    def __next__(self):
        self.n += 1
        if self.n == 3:
            raise KeyError("three")
        if self.n > 4:
            raise StopIteration
        return self.n

for label, fn in (("next(Boom())", lambda: next(Boom())),
                  ("Boom().__next__()", lambda: Boom().__next__()),
                  ("next(Boom(), 'dflt')", lambda: next(Boom(), "dflt")),
                  ("next(Done())", lambda: next(Done())),
                  ("next(Done(), 'dflt')", lambda: next(Done(), "dflt")),
                  ("list(Boom())", lambda: list(Boom())),
                  ("list(Done())", lambda: list(Done()))):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "=> raised", type(e).__name__, e)

c = Counter()
print(next(c), next(c))
try:
    next(c)
except KeyError as e:
    print("KeyError", e)
print(next(c), next(c, "end"), next(c, "end"))

# A for loop sees the same exception, not an early end.
try:
    for x in Boom():
        print("got", x)
except ValueError as e:
    print("for loop raised", e)

# The ordinary iterators are unaffected.
i = iter([1, 2])
print(next(i), next(i), next(i, "gone"))
g = (x for x in range(2))
print(next(g), next(g), next(g, "gone"))

# An exception raised inside a generator propagates through next() too.
def gen():
    yield 1
    raise RuntimeError("inside")
h = gen()
print(next(h))
try:
    next(h)
except RuntimeError as e:
    print("generator raised", e)
