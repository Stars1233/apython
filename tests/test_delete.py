# Test delete operations
x = 10
del x
try:
    print(x)
except NameError:
    print("x deleted")

# Delete from dict
d = {"a": 1, "b": 2, "c": 3}
del d["b"]
print(len(d))    # 2
print(d["a"])    # 1
print(d["c"])    # 3

# Delete local variable
def test_del():
    y = 42
    del y
    try:
        print(y)
    except NameError:
        print("y deleted")

test_del()

# Delete global
z = 100
del z
try:
    print(z)
except NameError:
    print("z deleted")


# Deleting an attribute of a static builtin whose type has no tp_setattr took
# the object's reference off the stack AND decref'd it, so the unwinder --
# which restores the stack pointer to where it stood before the instruction --
# released the same slot a second time.  The heap was corrupted; the crash
# landed somewhere else entirely, in the collector or inside malloc.  Nothing
# is printed per iteration on purpose: what is being tested is that the loop
# survives at all.
def del_readonly(make, name):
    for i in range(200):
        try:
            delattr(make(i), name)
        except AttributeError:
            pass
    return "survived"


print(del_readonly(lambda i: range(i), "start"))
print(del_readonly(lambda i: slice(i), "stop"))
print(del_readonly(lambda i: memoryview(bytes(i)), "obj"))
print(del_readonly(lambda i: (i).to_bytes(2, "big"), "hex"))

# The same shape written out, so a reader sees what the loop is doing.
r = range(3)
try:
    del r.start
except AttributeError as e:
    print("range:", type(e).__name__)
print(r.start, r.stop, list(r))

# An object that is not a context manager, and one that is raised without
# being an exception: both pop an operand and then raise, and both used to be
# audited by hand rather than by the rule.
class NotAManager:
    pass


def with_nonmanager(n):
    for i in range(n):
        try:
            with NotAManager():
                pass
        except TypeError:
            pass
    return "survived"


def raise_nonexception(n):
    for i in range(n):
        try:
            raise NotAManager()
        except TypeError:
            pass
    return "survived"


print(with_nonmanager(200))
print(raise_nonexception(200))
