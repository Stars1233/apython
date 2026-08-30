# %-formatting parsed flags, width and precision only to skip past them, so
# "%5s" % "x" returned "x", and it never parsed a %(name)s mapping key at all
# -- the whole directive was copied through unchanged.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


# Mapping keys
print("%(a)s-%(b)d" % {"a": "x", "b": 2})
print("%(x)s" % {"x": [1, 2]}, "%(k)r" % {"k": "q"})
print("%(a)5s|" % {"a": "x"}, "%(a)-5s|" % {"a": "x"})
print(t(lambda: "%(z)s" % {"a": 1}))

# Width, flags and precision
print("%5s|" % "x", "%-5s|" % "x", "%05d" % 42, "%+d" % 42, "%.2f" % 3.14159)
print("%8.3f|" % 3.14159, "%-8.3f|" % 3.14159, "%10r|" % "q")
print("% d" % 42, "%+.1f" % 1.25, "%06.2f" % 3.14159)

# Bases, which the direct path never learned
print("%x" % 255, "%X" % 255, "%o" % 255, "%#x" % 255, "%#o" % 255)

# The plain forms are unchanged
print("%s %d" % ("a", 1), "%d%%" % 5, "%s" % "z", "%r" % "q", "%i" % 7)
print("%s" % 1.5, "%s" % [1], "%s" % None, "%d" % True)
