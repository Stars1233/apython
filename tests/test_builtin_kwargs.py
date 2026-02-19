# Test builtin keyword arguments

# print sep
print(1, 2, 3, sep=', ')
print(1, 2, 3, sep='-')
print("a", "b", "c", sep='')

# print end
print("hello", end='!\n')
print("no newline", end='')
print(" followed")

# print sep + end combo
print("x", "y", sep='.', end=';\n')

# print with no positional args
print(sep=',')
print(end='')

# print with None sep/end (means default)
print(1, 2, sep=None)
print("hi", end=None)
