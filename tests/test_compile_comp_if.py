# A comprehension's `if` condition is a disjunction: `or` and `and` belong to
# it, the ternary `if`/`else` does not.  Parsing the condition AT the `or`
# binding power stopped before `or`, which came out as "the comprehension was
# never closed" rather than as a wrong answer.  The iterable has the same
# grammar and had the same bug.
a = [1, 2, 3, 4]

print([o for o in a if o == 1 or o == 3])
print([o for o in a if o == 1 and o < 9])
print([o for o in a if o == 1 or o == 2 or o == 4])
print([o for o in a if not o == 1 and o < 3])
print([o for o in a if o == 1 if o < 9])
print([o for o in a if o == 1 or o == 3 if o > 2])
print({o for o in a if o == 1 or o == 2})
print({o: o * 2 for o in a if o == 1 or o == 4})
print(sorted(o for o in a if o == 2 or o == 3))

# The iterable is a disjunction too.
print([o for o in [] or [7, 8]])
print([o for o in a and [9]])

# A ternary inside the condition still needs its parentheses, but one in the
# element expression does not.
print([("lo" if o < 3 else "hi") for o in a if o == 1 or o == 4])
print(["y" if o % 2 else "n" for o in a if o > 1 or o < 0])

# Nested clauses, each with its own conditions.
print([(x, y) for x in a if x == 1 or x == 2 for y in a if y == 3 or y == 4])

# And the same shapes through our own compiler at runtime.
print(eval("[o for o in a if o == 1 or o == 4]", {"a": a}))
exec("r = [o for o in a if o == 2 or o == 3]", {"a": a}, d := {})
print(d["r"])
