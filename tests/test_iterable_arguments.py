# str.join, bytes.join and dict.update each read their argument as a
# concrete list (or dict) without checking what it was.  An immediate's
# payload is not an address, and no other type carries ob_size and ob_item
# where a list does, so every one of these either crashed or silently
# produced garbage.  All three take any iterable in Python.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


# str.join over every iterable kind
print(",".join(["a", "b"]), ",".join(("c", "d")), ",".join("xy"))
print(",".join(i for i in ["p", "q"]), ",".join({"k": 1, "j": 2}))
print(repr("".join([])), "-".join(sorted({"b", "a"})))
print([t(lambda: ",".join(v)) for v in (5, 1.5, None, True, [1])])

# bytes.join likewise, and each element must really be bytes
print(b",".join([b"a", b"b"]), b",".join((b"c",)), b"".join([]))
print(b"-".join(i for i in [b"p", b"q"]))
print([t(lambda: b",".join(v)) for v in (5, None, "ab", [b"a", "b"])])

# dict.update: mapping, iterable of pairs, keyword arguments, and both
d = {"x": 0}
d.update({"a": 1})
print(sorted(d.items()))
d = {}
d.update([("a", 1), ("b", 2)])
print(sorted(d.items()))
d = {}
d.update([["k", 1]])
print(sorted(d.items()))
d = {}
d.update(zip("ab", [1, 2]))
print(sorted(d.items()))
d = {}
d.update((c + "1", i) for i, c in enumerate("ab"))
print(sorted(d.items()))
d = {}
d.update(a=1, b=2)
print(sorted(d.items()))
d = {}
d.update({"a": 1}, b=2)
print(sorted(d.items()))
d = {}
d.update()
print(d)
print([t(lambda: {}.update(v)) for v in (5, 1.5, None, True)])
print(t(lambda: {}.update(["ab", "cd"])), t(lambda: {}.update([(1, 2, 3)])))
print(t(lambda: {}.update({}, {})))
