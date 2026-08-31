# getattr() on an immediate int or float must not dereference it.  The
# classmethod branch of the shared lookup read ob_type off the object without
# the pointer test its neighbours have, so `getattr(5, "from_bytes")` treated
# the number as an address.
print(getattr(5, "from_bytes")(b"\x02"))
print(getattr(5, "bit_length")(), getattr(-5, "bit_length")())
print(getattr(1.5, "is_integer")(), getattr(2.0, "is_integer")())
print(getattr(5, "to_bytes")(1, "big"))
print(hasattr(5, "from_bytes"), hasattr(5, "nope"), hasattr(1.5, "hex"))
print(getattr(5, "nope", "dflt"), getattr(1.5, "nope", "dflt"))
print(getattr(True, "bit_length")())

# A property reached through an instance runs the getter.  (Reached through
# the class CPython hands back the property object; we run the getter there
# too, which is a separate, older divergence -- see .la_handle_property.)
class P:
    @property
    def v(self):
        return 42

    @classmethod
    def who(cls):
        return cls.__name__


print(getattr(P(), "v"))
print(getattr(P, "who")(), getattr(P(), "who")())

# The same shapes through attribute syntax.
print((5).from_bytes(b"\x03"), P().v)
