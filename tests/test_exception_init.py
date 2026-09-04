# BaseException had no __init__ of its own.  A subclass whose __init__ calls
# super().__init__(msg) therefore walked the whole exception MRO and reached
# object.__init__, which takes its arguments and ignores them -- so .args
# still held whatever the constructor was called with, and str() and repr(),
# which are built from .args, reported that instead of the composed message.
#
# re.error is exactly this shape: error(msg, pattern, pos) composes
# "msg at position N" and hands it to super().__init__.


class Composed(Exception):
    def __init__(self, msg, pattern=None, pos=None):
        self.msg = msg
        self.pattern = pattern
        self.pos = pos
        if pattern is not None and pos is not None:
            msg = "%s at position %d" % (msg, pos)
        super().__init__(msg)


e = Composed("bad escape", r"\q", 0)
print(e.args)
print(str(e))
print(repr(e))
print(e.msg, e.pattern, e.pos)

plain = Composed("no position")
print(plain.args, str(plain))

try:
    raise Composed("thrown", "abc", 2)
except Composed as caught:
    print(caught.args, str(caught), caught.pos)

# The direct calls, too.
direct = Exception("a")
print(direct.args)
direct.__init__("b", "c")
print(direct.args, str(direct))
direct.__init__()
print(direct.args, str(direct))


# A subclass that does not call super() keeps whatever the type call built.
class Silent(Exception):
    def __init__(self, a, b):
        self.pair = (a, b)


s = Silent(1, 2)
print(s.args, s.pair)


# And one that passes several arguments up.
class Several(Exception):
    def __init__(self, *parts):
        super().__init__(*parts)


print(Several(1, 2, 3).args)
print(str(Several(1, 2, 3)))
print(Several().args, repr(str(Several())))
