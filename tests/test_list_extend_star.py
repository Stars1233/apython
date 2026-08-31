# LIST_EXTEND with a list operand used to append every element twice, so
# [*a] came out as a + a.  A tuple operand was fine, which is why nothing
# noticed: most starred unpacking in the stdlib is over tuples.
a = [1, 2]
t = (3, 4)
print([*a])
print([0, *a])
print([*a, *a])
print([*t])
print([*a, *t])
print((*a,))
print({*a})
print([*"xy"])
print([*range(3)])
b = []
b.extend(a)
print(b)
