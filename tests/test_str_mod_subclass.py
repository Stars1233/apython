# `fmt % args` where args is a tuple SUBCLASS.
#
# str_mod decided whether its right operand was a tuple with an exact type
# comparison, so a subclass was taken for a single value: one `%r` printed the
# whole object, and a format with more than one conversion walked off the end
# of a one-element argument list.  collections.namedtuple's __repr__ is
# `'(x=%r, y=%r)' % self`, with self a tuple subclass, so it crashed there.
class T(tuple):
    pass


class Named(tuple):
    def __repr__(self):
        return "Named" + "(a=%r, b=%r)" % self


t = T((1, 2))
print("%r %r" % t)
print("%s-%s" % t)
print("%d/%d" % t)
print("%r" % (t,))
print(repr(Named((3, "x"))))

# A plain tuple and a single value still behave.
print("%r %r" % (1, 2))
print("%s" % "one")
print("%s" % [1, 2])
print("%(k)s" % {"k": "v"})


class D(dict):
    pass


print("%(k)s" % D(k="sub"))

# An empty subclass tuple, and one longer than the format needs.
print("[%s]" % T(("only",)))
try:
    print("%s %s" % T(("one",)))
except TypeError as e:
    print("TypeError:", e)
try:
    print("%s" % T(("a", "b")))
except TypeError as e:
    print("TypeError:", e)
