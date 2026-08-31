# A property getter, or a user __get__, that raises.
#
# LOAD_ATTR never checked whether the call returned NULL: it pushed the (0, 0)
# pair on the value stack and carried on dispatching.  So the exception escaped
# its own try block -- the frame's handler never ran -- and a getter that
# touched self segfaulted.  A user descriptor's __get__ went the other way and
# unwound natively, past the handler that should have caught it.
class C:
    @property
    def bad(self):
        raise ValueError("getter blew up")

    @property
    def touches(self):
        return self.missing_attr

    @property
    def fine(self):
        return "fine"


c = C()
print(c.fine)

try:
    c.bad
    print("no error")
except ValueError as e:
    print("ValueError", e)

try:
    c.touches
    print("no error")
except AttributeError:
    print("AttributeError")


class Desc:
    def __get__(self, obj, objtype=None):
        raise RuntimeError("descr blew up")


class D:
    d = Desc()


try:
    D().d
    print("no error")
except RuntimeError as e:
    print("RuntimeError", e)


# The failure has to be catchable from a frame further out, too.
def outer():
    def inner():
        return c.bad

    try:
        inner()
        return "no error"
    except ValueError as e:
        return "caught " + str(e)


print(outer())


# And a raising getter reached through a method call form.
class E:
    @property
    def fn(self):
        raise KeyError("k")


try:
    E().fn
    print("no error")
except KeyError:
    print("KeyError")

# The ordinary property still works after all that.
print(c.fine)
