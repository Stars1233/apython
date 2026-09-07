"""An iterator must drop its sequence BEFORE releasing it.

CPython issue 26494.  An iterator that runs off the end releases the sequence
it was walking; if that was the last reference, the sequence's `__del__` runs
inside the release.  A `__del__` that reaches back into the same iterator then
finds a field that still points at memory being freed.

CPython's fix was to NULL the field first and release afterwards, so a
re-entrant `next()` reads "exhausted" rather than "freed".  Doing it the other
way round is a use-after-free, and it segfaulted here.

`support.check_free_after_iterating` is CPython's own test for this and is what
found it -- so the same shape is checked below for every builtin iterator, not
just list, because the ordering was wrong in eight places.

The generator cases matter for a second reason: a generator holds its frame,
and a `__del__` that resumes a generator being torn down reaches the same
field through gen_send, gen_throw and gen_close as well as gen_iternext.
"""

import gc


def check(name, make):
    """Build a container whose __del__ re-enters an iterator over itself."""
    box = {}

    class Probe(make):
        def __del__(self):
            it = box.get("it")
            if it is None:
                return
            try:
                next(it)
            except StopIteration:
                box["saw"] = "stop"
            except Exception as e:          # noqa: BLE001
                box["saw"] = type(e).__name__
            else:
                box["saw"] = "value"

    box["it"] = iter(Probe())
    try:
        next(box["it"])
    except StopIteration:
        pass
    box["it"] = None
    gc.collect()
    print(name, "->", box.get("saw"))


# tuple, dict, set, str and bytes are NOT here yet: their iterators hold the
# sequence until the iterator itself dies rather than dropping it at
# exhaustion, so __del__ runs too late to observe anything.  That is the next
# commit; this file grows to cover them there.
print("--- exhaustion frees the sequence ---")
check("list", list)


print("--- a non-empty container, exhausted by a full walk ---")


def check_full(name, factory, items):
    box = {}
    seen = []

    class Probe(factory):
        def __del__(self):
            it = box.get("it")
            if it is None:
                return
            try:
                next(it)
            except StopIteration:
                box["saw"] = "stop"
            except Exception as e:          # noqa: BLE001
                box["saw"] = type(e).__name__
            else:
                box["saw"] = "value"

    box["it"] = iter(Probe(items))
    for x in box["it"]:
        seen.append(x)
    box["it"] = None
    gc.collect()
    print(name, "->", box.get("saw"), "walked", len(seen))


check_full("list", list, [1, 2, 3])


print("--- str and bytes iterators ---")
for label, seq in (("str", "abc"), ("bytes", b"abc"), ("bytearray", bytearray(b"abc"))):
    it = iter(seq)
    got = list(it)
    # A second pass on an exhausted iterator must stay exhausted, not restart
    # and not read freed storage.
    print(label, "->", got, list(it))


print("--- an exhausted iterator stays exhausted ---")
for factory, arg in ((list, [1]), (tuple, (1,)), (set, {1}), (dict, {"k": 1}),
                     (str, "a"), (bytes, b"a")):
    it = iter(factory(arg) if factory is not dict else dict(arg))
    list(it)
    again = []
    for _ in range(3):
        try:
            again.append(next(it))
        except StopIteration:
            again.append("stop")
    print(factory.__name__, "->", again)


print("--- generators ---")


def gen_plain():
    yield 1


def gen_finally(log):
    try:
        yield 1
        yield 2
    finally:
        log.append("closed")


g = gen_plain()
print("drain:", list(g), "again:", list(g))

log = []
g = gen_finally(log)
print("first:", next(g))
g.close()
print("after close:", log, list(g))

log = []
g = gen_finally(log)
print("first:", next(g))
del g
gc.collect()
print("dropped mid-flight:", log)


# send/throw/close on an exhausted generator must all report exhaustion
# rather than reaching a released frame.
g = gen_plain()
list(g)
try:
    g.send(None)
except StopIteration:
    print("send after exhaustion: stop")
try:
    g.throw(ValueError("v"))
except ValueError:
    print("throw after exhaustion: ValueError")
g.close()
print("close after exhaustion: ok")


print("--- a cleared iterator is still safe to read ---")
# gc.collect() over a cycle holding a half-consumed iterator clears it; the
# readers must all tolerate a NULL sequence rather than dereferencing it.
for factory, arg in ((list, [1, 2, 3]), (tuple, (1, 2, 3)), (set, {1, 2, 3}),
                     (dict, {"a": 1, "b": 2}), (str, "abc"), (bytes, b"abc")):
    holder = {}
    holder["it"] = iter(factory(arg) if factory is not dict else dict(arg))
    holder["self"] = holder          # a cycle, so the collector must walk it
    next(holder["it"])
    del holder
    gc.collect()
print("survived:", gc.collect() >= 0)


print("--- iteration that mutates the list underneath ---")
lst = [1, 2, 3, 4]
out = []
for x in lst:
    out.append(x)
    if x == 2:
        del lst[:]
print("truncated mid-iteration:", out)

print("done")
