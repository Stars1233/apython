# Four things the compiler rejected or mangled that CPython accepts.
#
#  * PEP 515 underscores reached int_from_cstr_base and strtod verbatim, and
#    both stop at the first one, so `1_000` was 1 and `1_000.5` a SyntaxError.
#  * The literal parts of an f-string had a private escape decoder that knew
#    only \n, \t and \r, so f"\x41" printed "x41" while "\x41" printed "A".
#  * `f"{x = }"` -- a space before the brace -- was read as a conversion.
#  * `del (a, b)` and a parenthesised with-item list were hard SyntaxErrors.
print(1_000, 1_000.5, 1_0.0_1, 0x_FF, 0b1_01, 0o1_7, 1_000e1_0)
print(1_000_000 + 1, type(1_000).__name__, type(1_0.0).__name__)

x = 5
print(f"\x41é\101\a\t|", f"{x}\x42")
print("\x41é\101\a\t|")
print(rf"\x41{x}", f"{x:>{1 + 2}}")

print(f"{x = }")
print(f"{x=}")
print(f"{x  =  }")
print(f"{x = :>8}|")
print(f"{x = !s}")
print(f"{x + 1 = }")

a, b = 1, 2
del (a, b)
print("del tuple", "a" in globals())

c, d = 3, 4
del [c, d]
print("del list")

e = {"k": 1}
f = [1, 2]
del (e["k"], f[0])
print(e, f)

g = 9
del (g,)
print("del single")


class CM:
    def __init__(self, t):
        self.t = t

    def __enter__(self):
        return self.t

    def __exit__(self, *a):
        return False


with (CM(1) as p, CM(2) as q):
    print("paren two", p, q)

with (CM(3) as r,):
    print("paren trailing", r)

with (CM(4)):
    print("paren single")

with (CM(5) as s):
    print("paren single as", s)

with CM(6) as t, CM(7) as u:
    print("plain", t, u)

# A parenthesised expression after `with` is still an expression.
with (CM(8)) as v:
    print("expr then as", v)
