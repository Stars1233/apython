# Subclassing float and complex.
#
# Both constructors discarded the type they were handed, so `class F(float)`
# and `class M(complex)` produced a plain float and a plain complex: the
# subclass name was lost, its __init__ never ran, and any attribute it defined
# was gone.  int, str, list, tuple, dict, set and bytes were all right; these
# two were the only builtins with neither a family flag nor a constructor that
# read its type argument.
#
# The flag is the load-bearing half.  float and complex keep their value
# inline, exactly as int and str do, so a subclass instance cannot come from
# instance_new -- and every helper that reads that value has to recognise one.
# For float those are float_to_f64, float_binop_accepts and binop_is_number;
# for complex, complex_to_parts.  float_repr and float_hash need the tag as
# well, because a subnormal's bit pattern is a small integer and so is
# indistinguishable from a pointer by inspection.

class F(float):
    pass


class M(complex):
    pass


class Tagged(float):
    def __init__(self, v):
        self.tag = "tagged-%s" % v


class TaggedComplex(complex):
    def __init__(self, *args):
        self.tag = "tc-%d" % len(args)


class Doubling(float):
    """__new__ and __init__ both run, and __new__ sees the subclass."""

    def __new__(cls, v):
        return super().__new__(cls, v * 2)


print("--- identity ---")
print("type(F(1.5))     :", type(F(1.5)).__name__)
print("type(M(1, 2))    :", type(M(1, 2)).__name__)
print("F is not float   :", type(F(1.5)) is not float)
print("M is not complex :", type(M(1, 2)) is not complex)
print("isinstance float :", isinstance(F(1.5), float))
print("isinstance cplx  :", isinstance(M(1, 2), complex))
print("issubclass float :", issubclass(F, float))
print("issubclass cplx  :", issubclass(M, complex))

print()
print("--- the value survives ---")
print("F(1.5)           :", F(1.5))
print("repr             :", repr(F(1.5)))
print("str              :", str(F(1.5)))
print("F(-0.25)         :", F(-0.25))
print("F(1e300)         :", F(1e300))
print("F(0.1)           :", F(0.1))
print("M(1, 2)          :", M(1, 2))
print("repr             :", repr(M(1, 2)))
print("M(0, -1)         :", M(0, -1))
print("M(1.5, -2.5)     :", M(1.5, -2.5))

print()
print("--- arithmetic reaches the base's slots ---")
print("F(1.5) + 1       :", F(1.5) + 1)
print("1 + F(1.5)       :", 1 + F(1.5))
print("F(1.5) * 2       :", F(1.5) * 2)
print("F(1.5) - 0.5     :", F(1.5) - 0.5)
print("F(3.0) / F(2.0)  :", F(3.0) / F(2.0))
print("F(7.5) // F(2.0) :", F(7.5) // F(2.0))
print("F(7.5) % 2.0     :", F(7.5) % 2.0)
print("-F(1.5)          :", -F(1.5))
print("abs(F(-1.5))     :", abs(F(-1.5)))
print("M(1, 2) + 1j     :", M(1, 2) + 1j)
print("1j + M(1, 2)     :", 1j + M(1, 2))
print("M(1, 2) * 2      :", M(1, 2) * 2)
print("M(1, 2) - M(0, 1):", M(1, 2) - M(0, 1))
print("M(1, 2) / 2      :", M(1, 2) / 2)
print("-M(1, 2)         :", -M(1, 2))
print("abs(M(3, 4))     :", abs(M(3, 4)))
print("M(1, 1) ** 2     :", M(1, 1) ** 2)

print()
print("--- comparison, hashing and conversion ---")
print("F(1.5) == 1.5    :", F(1.5) == 1.5)
print("1.5 == F(1.5)    :", 1.5 == F(1.5))
print("F(1.5) < 2.0     :", F(1.5) < 2.0)
print("F(1.5) > 1       :", F(1.5) > 1)
print("F(1.0) == 1      :", F(1.0) == 1)
print("F(1.0) == True   :", F(1.0) == True)
print("hash matches     :", hash(F(1.5)) == hash(1.5))
print("float(F(1.5))    :", float(F(1.5)))
print("int(F(1.5))      :", int(F(1.5)))
print("bool(F(0.0))     :", bool(F(0.0)))
print("bool(F(1.5))     :", bool(F(1.5)))
print("M(1, 2) == 1+2j  :", M(1, 2) == (1 + 2j))
print("1+2j == M(1, 2)  :", (1 + 2j) == M(1, 2))
print("M(1,2) == M(1,2) :", M(1, 2) == M(1, 2))
print("M(1,2) != M(1,3) :", M(1, 2) != M(1, 3))
print("hash matches     :", hash(M(1, 2)) == hash(1 + 2j))
print("complex(M(1, 2)) :", complex(M(1, 2)))
print("bool(M(0, 0))    :", bool(M(0, 0)))
print("bool(M(0, 1))    :", bool(M(0, 1)))
print("M(1, 2).real     :", M(1, 2).real)
print("M(1, 2).imag     :", M(1, 2).imag)
print("M(1, 2).conjugate:", M(1, 2).conjugate())

print()
print("--- __init__ runs, and __new__ sees the subclass ---")
print("Tagged(2.5).tag  :", Tagged(2.5).tag)
print("Tagged(2.5)      :", Tagged(2.5))
print("Tagged(2.5) + 1  :", Tagged(2.5) + 1)
print("TaggedComplex.tag:", TaggedComplex(1, 2).tag)
print("TaggedComplex    :", TaggedComplex(1, 2))
print("Doubling(2.0)    :", Doubling(2.0))
print("type(Doubling)   :", type(Doubling(2.0)).__name__)

print()
print("--- the instance carries its own __dict__ ---")
t = Tagged(3.5)
t.extra = [1, 2, 3]
print("extra            :", t.extra)
print("tag              :", t.tag)
print("value            :", float(t))

print()
print("--- as dict keys and in containers ---")
print("dict lookup      :", {F(1.5): 'a'}[1.5])
print("dict lookup back :", {1.5: 'a'}[F(1.5)])
print("complex key      :", {M(1, 2): 'z'}[1 + 2j])
print("in a set         :", F(1.5) in {1.5, 2.5})
print("sorted           :", sorted([F(2.0), F(1.0), F(3.0)]))
print("min              :", min([F(2.0), F(1.0)]))
print("max              :", max([F(2.0), F(1.0)]))
print("sum              :", sum([F(1.0), F(2.0)]))
print("sum complex      :", sum([M(1, 1), M(2, 2)]))
print("list of them     :", [F(1.0), F(2.5)])
print("tuple of them    :", (M(1, 1), M(0, 2)))

print()
print("--- a subclass with a mixin, so the MRO has more than one step ---")


class Mixin:
    def describe(self):
        return "mixed %s" % (self,)


class MF(Mixin, float):
    pass


class MC(Mixin, complex):
    pass


print("MF(1.5)          :", MF(1.5))
print("MF describe      :", MF(1.5).describe())
print("MF + 1           :", MF(1.5) + 1)
print("isinstance       :", isinstance(MF(1.5), float))
print("MC(1, 2)         :", MC(1, 2))
print("MC describe      :", MC(1, 2).describe())
print("MC + 1j          :", MC(1, 2) + 1j)

print()
print("--- churn: the instance holds its type and its value ---")
kept = [F(i / 2) for i in range(20)] + [M(i, -i) for i in range(20)]
print("churn            :", len([[i, i] for i in range(3000)]))
print("still there      :", kept[3], kept[25])
print("still typed      :", type(kept[3]).__name__, type(kept[25]).__name__)
print("still adds       :", kept[3] + 1, kept[25] + 1j)
