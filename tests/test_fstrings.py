# Test f-string formatting
x = 42
name = "world"
print(f"hello {name}")      # hello world
print(f"x = {x}")           # x = 42
print(f"x = {x!r}")         # x = 42
print(f"{name!s}")          # world
print(f"{'a'} {'b'}")       # a b
print(f"one{2}three")       # one2three
print(f"")                  # (empty line)
print(f"no interpolation")  # no interpolation
y = 3.14
print(f"pi = {y}")          # pi = 3.14
print(f"{True} and {False}") # True and False
