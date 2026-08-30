# Operations that took a non-int where an int was required, or a non-string
# where a string was required, and produced a value instead of a TypeError.
# `"a" * 1.5` returned 0.0 and `"a" + 1.5` returned 1.5.


def err(fn, *a):
    try:
        return fn(*a)
    except Exception as e:
        return type(e).__name__


# Sequence repetition takes an index, not any number
for seq in ([1], (1,), "a", b"a"):
    print(type(seq).__name__,
          err(lambda: seq * 1.5), err(lambda: seq * None),
          err(lambda: seq * "2"), err(lambda: 1.5 * seq))
    print("  ", seq * 2, seq * True, len(seq * 0))

# Concatenation takes the same type
print(err(lambda: "a" + 1.5), err(lambda: "a" + None), err(lambda: "a" + 5))
print(err(lambda: b"a" + 1.5), err(lambda: [1] + 1.5), err(lambda: (1,) + 1.5))
print("a" + "b", b"a" + b"b", [1] + [2], (1,) + (2,))

# float() and int() of the wrong thing
print(err(float, None), err(float, [1]), err(int, None), err(int, [1]))
print(float("1.5"), float(1), float(True), int("7"), int(2.9), int(True))

# reversed() needs a sequence
print(err(lambda: list(reversed(None))), err(lambda: list(reversed(True))),
      err(lambda: list(reversed(1.5))))
print(list(reversed([1, 2])), list(reversed("ab")), list(reversed((1, 2))))

# round() and pow() accept bools, which are ints
print(round(True), round(False), round(1.5), round(2.5), round(-1.5))
print(pow(True, True, True), pow(2, 10, 1000), pow(2, 10))

# setattr on something with no __dict__ is an AttributeError
print(err(setattr, 5, "x", 1), err(setattr, 1.5, "x", 1),
      err(setattr, None, "x", 1), err(setattr, True, "x", 1),
      err(setattr, "s", "x", 1))

# format() of None with a non-empty spec
print(err(format, None, ">5"), format(None, ""), format(5, ">5"), format(1.5, ".3f"))
