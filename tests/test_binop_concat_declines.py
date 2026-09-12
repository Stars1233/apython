# `seq + obj` asks obj, the way `seq * obj` already does.
#
# A binary operator whose left operand cannot handle the right one must
# DECLINE -- return NotImplemented -- so the interpreter can try the right
# operand's reflected dunder.  The repeat bodies learned that when
# binop_is_count went in, and its comment named this bug for `*`.  The concat
# bodies never did: list_concat, str_concat, tuple_concat, bytes_concat and
# bytearray_concat raised TypeError from inside the slot, so
# op_binary_op's fall-through to __radd__ was unreachable and
#
#     [1] + obj
#
# was "can only concatenate list (not "R") to list" where CPython answers
# obj.__radd__([1]).
#
# It is the reason collections.UserList and UserString could not be added to a
# real list or str, and it breaks every wrapper class that implements only the
# reflected half.

class Reflected:
    """Handles the operation from the right, and only from the right."""

    def __radd__(self, other):
        return ("radd", type(other).__name__)

    def __rmul__(self, other):
        return ("rmul", type(other).__name__)


r = Reflected()

print([1, 2] + r, "list + obj")
print("ab" + r, "str + obj")
print((1, 2) + r, "tuple + obj")
print(b"ab" + r, "bytes + obj")
print(bytearray(b"ab") + r, "bytearray + obj")

# The repeat half already worked; it must keep working.
print([1, 2] * r, "list * obj")
print("ab" * r, "str * obj")
print((1, 2) * r, "tuple * obj")
print(b"ab" * r, "bytes * obj")

# In place: `a += obj` is the same protocol, and reaches __radd__ when the
# right operand is not iterable.
a = [1, 2]
a += r
print(a, "list += obj")

s = "ab"
s += r
print(s, "str += obj")

# The shape the standard library's own wrappers have -- collections.UserList
# and UserString are written exactly this way, and neither could be added to a
# real list or str.  Spelled out here rather than imported, because this tree's
# lib/collections does not ship them.
class SeqWrapper:
    def __init__(self, data):
        self.data = data

    def __radd__(self, other):
        return other + self.data


print([1] + SeqWrapper([2]), "list + a UserList-shaped wrapper")
print("a" + SeqWrapper("b"), "str + a UserString-shaped wrapper")

# A right operand with nothing to offer still gets CPython's error, and it
# still names both types.
class Nothing:
    pass


# eval over a fixed list of literals written here, so that one loop can check
# five operators' messages; there is no input of any kind involved.
for expr in ("[1] + Nothing()", "'a' + Nothing()", "(1,) + Nothing()",
             "b'a' + Nothing()", "bytearray(b'a') + Nothing()"):
    try:
        eval(expr)
        print("NO ERROR", expr)
    except TypeError as e:
        print(expr, "->", e)

# The dunder called BY NAME still raises, in CPython as here: only the
# operator declines, because only the operator has a reflected half to fall
# back to.  `[1].__add__(obj)` is a TypeError on both sides.
for label, call in (("list", lambda: [1].__add__(r)),
                    ("tuple", lambda: (1,).__add__(r)),
                    ("str", lambda: "a".__add__(r)),
                    ("bytes", lambda: b"a".__add__(r))):
    try:
        print(label, "__add__ ->", call())
    except TypeError as e:
        print(label, "__add__ raises:", e)

# And a genuinely wrong left operand still raises through the dunder.
try:
    list.__add__(5, [1])
    print("NO ERROR", "list.__add__(5, [1])")
except TypeError as e:
    print("list.__add__(5, [1]) ->", type(e).__name__)

# Concatenating the real thing must be untouched.
print([1] + [2], "list + list")
print("a" + "b", "str + str")
print((1,) + (2,), "tuple + tuple")
print(b"a" + b"b", "bytes + bytes")
print(bytearray(b"a") + bytearray(b"b"), "bytearray + bytearray")
print(bytearray(b"a") + b"b", "bytearray + bytes")
