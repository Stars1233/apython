# str.format: attribute and index access in a field, and a spec that has
# fields of its own.
#
# A field was a position, a name, or empty; "{0.attr}" and "{0[key]}" both
# raised KeyError, though the scanner already counted bracket depth so that a
# ':' inside [] would not be read as a spec -- the text arrived intact and
# unread.  A nested spec, "{:{}}", was handed to the formatter with the braces
# still in it and rejected as invalid.

class P:
    def __init__(self, x, y):
        self.x, self.y = x, y

p = P(1, P(2, 3))
d = {"k": "v", "n": 7, "inner": {"deep": "yes"}}
l = [10, [20, 30]]

print("{0.x}".format(p), "{0.y.x}".format(p), "{a.y.y}".format(a=p))
print("{0.real}".format(1), "{0.imag}".format(1.5), "{0.real}".format(1 + 2j))
print("{0[k]}".format(d), "{0[inner][deep]}".format(d), "{n[n]}".format(n=d))
print("{0[0]}".format(l), "{0[1][1]}".format(l), "{0[1][0]}".format(l))
print("{0.x:>4}".format(p), "{0[n]:03d}".format(d), "{0[0]:+}".format(l))
print("{0.__class__.__name__}".format(1), "{0.__class__.__name__}".format("s"))

# A spec that is itself a format string.
print("{:{}}".format(1, "3d"), "|", "{:>{}}".format("a", 5), "|")
print("{0:{1}}".format(1, "05d"), "{:{w}}".format(7, w="4d"))
print("{:{a}{b}}".format(3, a=">", b=6), "|")
print("{:.{p}f}".format(3.14159, p=2))
print("{0:{1}{2}}".format("x", "^", 7), "|")

# The auto counter runs through the spec, as CPython's does.
print("{:{}}".format(42, "5d"), "|")

# What must still work, and still fail.
print("{} {} {}".format(1, 2, 3), "{a}-{b}".format(a=1, b=2), "{0}{0}".format("z"))
print("{0!r} {0!s}".format("q"), "{{literal}}".format())
for e in ("'{0.nope}'.format(1)", "'{0[9]}'.format([1])", "'{0[z]}'.format({})",
          "'{0[0]}'.format(1)", "'{2}'.format(1)", "'{z}'.format(a=1)"):
    try:
        eval(e)
        print(e, "=> no error")
    except (AttributeError, IndexError, KeyError, TypeError) as x:
        print(e, "=>", type(x).__name__)

# format_map takes the same suffixes, and a missing key is a KeyError rather
# than a crash -- nothing checked dict_get's NULL before obj_str dereferenced it.
print("{a}".format_map({"a": 3}), "{a.real}".format_map({"a": 3}))
print("{a[k]}".format_map({"a": {"k": "in"}}), "{a[0]}".format_map({"a": [9]}))
try:
    "{z}".format_map({"a": 1})
except KeyError:
    print("format_map missing key => KeyError")
