# A conditional expression's condition is a `disjunction`: `or` belongs to it,
# a nested ternary does not.  It was parsed AT BP_OR, and the Pratt driver
# continues only while lbp > min_bp, so it stopped before the `or` and the
# `else` looked missing.  `and` binds tighter and so worked, which hid it.
a, b, c = 0, 1, 2
print(1 if a or b else 2)
print(1 if a and b else 2)
print(1 if a or b or c else 2)
print(1 if not a or b else 2)
print("y" if a == 0 or b == 9 else "n")
print([x for x in (0, 1, 2) if (1 if x or True else 0)])

# The else branch still nests to the right, and a ternary is still not taken
# as part of the condition.
print(1 if a else 2 if b else 3)
print((1 if a else 2) if b else 3)

# In the places a ternary shows up in real code.
d = {"k": 1 if a or b else 2}
print(d)
print([1 if v or a else 0 for v in (0, 1)])
f = lambda v: "hi" if v or b else "lo"
print(f(0), f(1))


def g(v=1 if 0 or 1 else 2):
    return v


print(g())

# And through our own compiler at run time.
print(eval("1 if a or b else 2", {"a": a, "b": b}))
