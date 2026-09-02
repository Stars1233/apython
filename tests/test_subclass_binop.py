# A subclass of a builtin can override an operator.
#
# It could not.  type_install_slots filled a heaptype's slots from the dunders
# it defined, and its table had rows for the unary operators, the comparisons,
# __len__ and the subscripts -- and not one binary operator.  So a subclass
# kept the base's nb_add by pointer, nothing overwrote it, and
#
#     class D(int):
#         def __add__(self, o): return "D.add"
#
# answered 3 for D(1) + 2.  The same for float, str, list, dict, set, bytes
# and their in-place forms.  op_binary_op never reached its by-name fallback,
# because that only runs when the slot DECLINES, and int's adder never does.
#
# Reflected names get no slot of their own: the wrapper here is
# one-directional, unlike CPython's SLOT1BIN, and the reflected-dunder arm
# already serves that direction.


class D(int):
    def __add__(self, o): return "D.add"
    def __mul__(self, o): return "D.mul"
    def __mod__(self, o): return "D.mod"
class F(float):
    def __add__(self, o): return "F.add"
    def __truediv__(self, o): return "F.truediv"
class S(str):
    def __add__(self, o): return "S.add"
    def __mod__(self, o): return "S.mod"
class L(list):
    def __add__(self, o): return "L.add"
    def __mul__(self, o): return "L.mul"
class T(tuple):
    def __add__(self, o): return "T.add"
class Dd(dict):
    def __or__(self, o): return "Dd.or"
class St(set):
    def __sub__(self, o): return "St.sub"
    def __and__(self, o): return "St.and"
class By(bytes):
    def __add__(self, o): return "By.add"
class Ba(bytearray):
    def __add__(self, o): return "Ba.add"

print("=== the override wins ===")
print("D  :", D(1) + 2, D(1) + 2.5, D(2) * 3, D(7) % 2)
print("F  :", F(1.5) + 1, F(3.0) / 2)
print("S  :", S("a") + "b", S("%s") % 1)
print("L  :", L([1]) + [2], L([1]) * 2)
print("T  :", T((1,)) + (2,))
print("Dd :", Dd() | {})
print("St :", St({1}) - {2}, St({1}) & {1})
print("By :", By(b"a") + b"b")
print("Ba :", Ba(b"a") + b"b")

print("=== the specialisations must not run ahead of it ===")
# D(1) + 2.5 used to take the float-coercion shortcut and answer 3.5, and
# L([1]) * 2 used to take sq_repeat and repeat the list.
print("float shortcut:", D(1) + 2.5)
print("sq_repeat     :", L([1]) * 2)

print("=== and a subclass that overrides nothing is unchanged ===")
class E(int): pass
class Lp(list): pass
class Sp(str): pass
print("E+2.5 :", E(1) + 2.5, type(E(1) + 2.5).__name__)
print("E+2   :", E(1) + 2, type(E(1) + 2).__name__)
print("E*3   :", E(2) * 3, E(7) // 2, E(7) % 2, E(2) ** 8)
print("Lp+   :", Lp([1]) + [2], Lp([1]) * 2, 2 * Lp([1]))
print("Sp+   :", Sp("a") + "b", Sp("a") * 2, 2 * Sp("a"))

print("=== the reflected side is still the other operand's ===")
# 2 * L([1]) is list.__rmul__, not L.__mul__ -- CPython installs no generic
# dispatcher into sq_repeat, and neither do we.
print("2*L :", 2 * L([1]))
print("2+D :", 2 + D(1))
print("(2,)+T:", (2,) + T((1,)))

print("=== plain builtins are untouched ===")
print(1 + 2, 1.5 * 2, "ab" * 2, [1] + [2], (1,) + (2,), 7 // 2, 7 % 2,
      2 ** 10, {1} | {2}, {"a": 1} | {"b": 2}, b"a" + b"b")
s = "x"
s += "y"
print("str +=:", s)
l = [1]
l += [2]
print("list +=:", l, l is not None)

print("=== the in-place forms too ===")
class Ii(int):
    def __iadd__(self, o): return "Ii.iadd"
    def __imul__(self, o): return "Ii.imul"
class Li(list):
    def __iadd__(self, o): return "Li.iadd"
    def __imul__(self, o): return "Li.imul"
class Si(set):
    def __ior__(self, o): return "Si.ior"

x = Ii(1); x += 1; print("Ii +=:", x)
x = Ii(2); x *= 3; print("Ii *=:", x)
y = Li([1]); y += [2]; print("Li +=:", y)
y = Li([1]); y *= 2; print("Li *=:", y)
z = Si({1}); z |= {2}; print("Si |=:", z)

# And a subclass that overrides none of them still mutates in place.
class Lp2(list): pass
p = Lp2([1]); q = p; p += [2]
print("plain +=:", sorted(p), p is q)

print("=== every operator, on one class ===")
class All(int):
    def __add__(s, o): return "add"
    def __sub__(s, o): return "sub"
    def __mul__(s, o): return "mul"
    def __mod__(s, o): return "mod"
    def __pow__(s, o): return "pow"
    def __lshift__(s, o): return "lshift"
    def __rshift__(s, o): return "rshift"
    def __and__(s, o): return "and"
    def __xor__(s, o): return "xor"
    def __or__(s, o): return "or"
    def __floordiv__(s, o): return "floordiv"
    def __truediv__(s, o): return "truediv"
    def __divmod__(s, o): return "divmod"
    def __matmul__(s, o): return "matmul"
a = All(6)
print(a + 1, a - 1, a * 2, a % 4, a ** 2, a << 1, a >> 1)
print(a & 3, a ^ 3, a | 3, a // 2, a / 2, divmod(a, 2), a @ 2)
