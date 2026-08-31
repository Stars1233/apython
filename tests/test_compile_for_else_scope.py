# A for/else keeps its else block in AstNode.clist with nchild left at 0, and
# sym_visit's generic walk goes by nchild -- so the whole else body was never
# visited by the symbol table.  A nested def there got no scope at all (its
# flags stayed 0), and compiling it segfaulted.  while/else is unaffected: it
# stores its else in `b`, which is visited.
a = 5


for j in range(1):
    pass
else:
    def h():
        return a

    print("for-else", h())


i = 0
while i < 1:
    i += 1
else:
    def g():
        return a

    print("while-else", g())


# A comprehension, a lambda and a class in a for/else body.
for k in range(1):
    pass
else:
    print([a + n for n in range(3)])
    f = lambda: a * 2
    print(f())

    class C:
        v = a

    print(C.v)


# Nested loops, each with an else.
for p in range(2):
    for q in range(2):
        if q:
            break
    else:
        def inner():
            return p

        print("inner", inner())
else:
    print("outer else", a)


# The else runs only when the loop was not broken out of.
for r in range(3):
    if r == 1:
        break
else:
    print("NOT REACHED")

print("done", a)
