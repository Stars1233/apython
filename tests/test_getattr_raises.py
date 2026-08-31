# getattr() and hasattr() swallow a missing attribute, not a getter that blew
# up -- and "missing" means AttributeError specifically.
#
# Both used DUNDER_RAISED, which is a pointer-identity test on
# current_exception: it fires for ANY newly raised exception.  So a
# __getattr__ or a property that raised AttributeError -- the very exception
# the protocol uses to say "absent" -- propagated instead of yielding the
# default or False.
class Dyn:
    def __getattr__(self, name):
        if name == "missing":
            raise AttributeError(name)
        if name == "boom":
            raise ValueError("boom")
        return "dyn:" + name


d = Dyn()
print(d.present)
print(getattr(d, "present"))
print(getattr(d, "missing", "dflt"))
print(hasattr(d, "missing"))
print(hasattr(d, "present"))

try:
    getattr(d, "boom", "dflt")
    print("swallowed, should not have")
except ValueError as e:
    print("propagated", e)

try:
    hasattr(d, "boom")
    print("swallowed, should not have")
except ValueError as e:
    print("propagated", e)

try:
    getattr(d, "missing")
    print("no error")
except AttributeError as e:
    print("AttributeError", e)


# The same through a property, which reaches the descriptor protocol instead.
class P:
    @property
    def absent(self):
        raise AttributeError("absent")

    @property
    def bad(self):
        raise ZeroDivisionError("nope")

    @property
    def fine(self):
        return "fine"


p = P()
print(getattr(p, "fine"))
print(getattr(p, "absent", "dflt"))
print(hasattr(p, "absent"))
try:
    getattr(p, "bad", "dflt")
    print("swallowed, should not have")
except ZeroDivisionError:
    print("propagated ZeroDivisionError")
print(hasattr(p, "fine"))


# A subclass of AttributeError counts as absent, as it does in CPython.
class MyAttrError(AttributeError):
    pass


class Sub:
    def __getattr__(self, name):
        raise MyAttrError(name)


print(getattr(Sub(), "x", "dflt"), hasattr(Sub(), "x"))


# And inside an except block, where current_exception is already set to the
# exception being handled -- the case the identity test exists for.
try:
    raise KeyError("outer")
except KeyError:
    print(getattr(d, "missing", "dflt"), hasattr(d, "missing"), getattr(d, "ok"))

# A plain object with no __getattr__ at all.
print(getattr(object(), "nope", "dflt"), hasattr(object(), "nope"))
