# Freeing a deeply nested structure must not recurse once per level.
#
# obj_decref -> obj_dealloc -> list_dealloc -> obj_decref is a real machine
# frame per level, and nothing bounded it: 300k nested lists dropped at once
# walked the stack off its guard page and took SIGSEGV.  CPython's answer is
# the trashcan -- past a nesting limit the object is put on a pending list and
# freed by the outermost dealloc instead of the innermost.
#
# The counts here are well past any plausible stack, and small enough to run
# in well under a second.

def nest(n, make):
    a = make()
    for _ in range(n):
        a = make(a)
    return a

print("=== lists ===")
a = []
for _ in range(200000):
    a = [a]
del a
print("freed")

print("=== tuples ===")
t = ()
for _ in range(200000):
    t = (t,)
del t
print("freed")

print("=== dicts, nested by value ===")
d = {}
for _ in range(200000):
    d = {"k": d}
del d
print("freed")

print("=== instances ===")
class Node:
    __slots__ = ("child",)
    def __init__(self, child=None):
        self.child = child

n = Node()
for _ in range(200000):
    n = Node(n)
del n
print("freed")

print("=== a chain that is dropped by rebinding rather than del ===")
a = []
for _ in range(150000):
    a = [a]
a = None
print("freed")

print("=== mixed containers ===")
x = []
for i in range(60000):
    x = [(x, {"n": i})]
del x
print("freed")

print("=== the same, reached through a local going out of scope ===")
def build_and_drop(n):
    a = []
    for _ in range(n):
        a = [a]
    return len(a)

print(build_and_drop(150000))
print("done")
