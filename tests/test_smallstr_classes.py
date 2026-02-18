# String as class attribute names
class Foo:
    x = 1
    y = 2
    longname = 3

f = Foo()
print(f.x)       # 1
print(f.y)       # 2
f.x = 10
print(f.x)       # 10

# __str__ and __repr__
class Bar:
    def __str__(self):
        return "bar"     # SmallStr return from __str__
    def __repr__(self):
        return "Bar()"

b = Bar()
print(b)          # bar
print(repr(b))    # Bar()
print(str(b))     # bar
print(f"{b}")     # bar

# String comparisons in control flow
def classify(s):
    if s == "a":
        return 1
    elif s == "bb":
        return 2
    elif s == "ccc":
        return 3
    return 0

print(classify("a"))    # 1
print(classify("bb"))   # 2
print(classify("ccc"))  # 3
print(classify("d"))    # 0

# Exception messages (often short strings)
try:
    raise ValueError("oops")
except ValueError as e:
    print(str(e))  # oops

try:
    x = {}["missing"]
except KeyError:
    print("KeyError caught")
