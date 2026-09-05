# Test generator return values via StopIteration.value

def gen_return_42():
    yield 1
    yield 2
    return 42

# Test 1: Catch StopIteration and check .value
g = gen_return_42()
print(next(g))  # 1
print(next(g))  # 2
try:
    next(g)
    print("ERROR: should have raised StopIteration")
except StopIteration as e:
    print(e.value)  # 42

# Test 2: Generator with no explicit return -> .value is None
def gen_no_return():
    yield 10

g2 = gen_no_return()
print(next(g2))  # 10
try:
    next(g2)
except StopIteration as e:
    print(e.value)  # None


# A generator that returns None raises a BARE StopIteration in CPython: args
# is (), so str(e) is "" and an unhandled one prints "StopIteration" rather
# than "StopIteration: None".  gi_return_value holds the None SINGLETON for
# such a generator, which is not the same as holding nothing -- the two paths
# that raise the exhaustion, next() and tp_iternext, each tested only for
# nothing and passed the singleton through as an argument.
print("=== the exhaustion carries nothing ===")


def show(label, fn):
    try:
        print("%-24s %r" % (label, fn()))
    except StopIteration as e:
        print("%-24s args=%r value=%r str=%r"
              % (label, e.args, e.value, str(e)))


def implicit():
    yield 1


def explicit_none():
    yield 1
    return None


def a_value():
    yield 1
    return 7


def never_yields():
    return
    yield


for name, g in (("implicit", implicit()), ("explicit None", explicit_none()),
                ("returns 7", a_value()), ("never yields", never_yields())):
    if name != "never yields":
        next(g)
    show(name, lambda g=g: next(g))

for name, mk in (("send, implicit", implicit), ("send, a value", a_value)):
    g = mk()
    next(g)
    show(name, lambda g=g: g.send(None))

show("an empty list", lambda: next(iter([])))
print("with a default", next(iter([]), "d"))
print("constructed", StopIteration().args, repr(str(StopIteration())),
      StopIteration(None).args, repr(str(StopIteration(None))))
