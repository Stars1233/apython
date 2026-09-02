# Struct sequences: a tuple whose entries are also named.
#
# os.stat() returns one, and so do sys.version_info and its three siblings.
# version_info was a bare 5-tuple here, so type(sys.version_info).__name__ was
# 'tuple' and sys.version_info.major was an AttributeError -- which platform.py
# reads.  float_info, int_info and hash_info did not exist at all; sysmod.asm
# carried "Skip for now" where each should have been.
#
# The instance IS a tuple: the PyTupleObject header sits at offset 0, so every
# piece of tuple code reads it unchanged and the named-only tail lives past
# the header, where nothing tuple does can reach.  That is what this file
# checks -- not that the names exist, but that adding them cost nothing a
# tuple could do.
#
# Values that differ between CPython builds (the micro version) or that
# describe apython honestly rather than CPython (hash_info.algorithm is 'fnv',
# because str_hash is FNV-1a and not siphash) are compared as predicates.

import sys

INFOS = ("version_info", "float_info", "int_info", "hash_info")

print("--- each is its own type, and a tuple ---")
for name in INFOS:
    o = getattr(sys, name)
    print("%-14s %-14s tuple=%s len=%d" % (
        name, type(o).__name__, isinstance(o, tuple), len(o)))

print()
print("--- the type is named after the field set, not 'tuple' ---")
print("distinct types:", len({type(getattr(sys, n)).__name__ for n in INFOS}))
print("none is tuple :", all(type(getattr(sys, n)) is not tuple for n in INFOS))
print("all subclass  :", all(issubclass(type(getattr(sys, n)), tuple)
                             for n in INFOS))

v = sys.version_info
print()
print("--- version_info: named and positional agree ---")
print("major   :", v.major, v.major == v[0])
print("minor   :", v.minor, v.minor == v[1])
print("micro   :", v.micro == v[2])
print("release :", v.releaselevel, v.releaselevel == v[3])
print("serial  :", v.serial, v.serial == v[4])
print("is 3.12 :", v.major == 3 and v.minor == 12)
print("compares:", v >= (3, 0), v < (4, 0), v[:2] == (3, 12))

print()
print("--- and everything a tuple can do ---")
print("len       :", len(v))
print("iter      :", [type(x).__name__ for x in v])
# The micro version differs between builds, so slices that would show
# it are compared rather than printed.
print("slice     :", v[:2], v[1:3] == (v[1], v[2]), v[-2:])
print("negative  :", v[-1], v[-5])
print("in        :", 12 in v, "final" in v, 999 in v)
print("count     :", v.count("final"))
print("index     :", v.index("final"))
print("concat    :", (v + (1, 2))[5:], len(v + (1, 2)))
print("repeat    :", len(v * 2))
print("tuple()   :", tuple(v) == (v[0], v[1], v[2], v[3], v[4]))
print("list()    :", len(list(v)))
print("reversed  :", list(reversed(v))[0] == v[4])
print("min/max   :", max(v[:2]), min(v[:2]))
print("sorted    :", sorted(v[:3]) == sorted([v[0], v[1], v[2]]))
a, b, c, d, e = v
print("unpack    :", (a, b, d) == (v[0], v[1], v[3]))
first, *rest = v
print("star      :", first == v[0], len(rest) == 4)

print()
print("--- hashing and equality, which need tuple's own slots ---")
plain = tuple(v)
print("== tuple  :", v == plain)
print("tuple ==  :", plain == v)
print("hash      :", hash(v) == hash(plain))
print("dict key  :", {v: "found"}[plain])
print("in a set  :", v in {plain})
print("set dedup :", len({v, plain}))
print("!=        :", v != (1, 2))
print("sortable  :", sorted([v, (3, 11)])[0] == (3, 11))

print()
print("--- repr is name(field=value, ...), with the dotted type name ---")
r = repr(v)
print("starts    :", r.startswith("sys.version_info("))
print("ends      :", r.endswith(")"))
print("has names :", all(("%s=" % n) in r
                         for n in ("major", "minor", "micro",
                                   "releaselevel", "serial")))
print("quotes str:", "'final'" in r)
print("commas    :", r.count(", ") == 4)
print("str==repr :", str(v) == repr(v))

print()
print("--- float_info, which the numeric stdlib reads ---")
fi = sys.float_info
print("epsilon   :", fi.epsilon)
print("max       :", fi.max)
print("min       :", fi.min)
print("mant_dig  :", fi.mant_dig, fi.dig, fi.radix, fi.rounds)
print("exponents :", fi.max_exp, fi.min_exp, fi.max_10_exp, fi.min_10_exp)
print("positional:", fi[0] == fi.max, fi[8] == fi.epsilon, len(fi) == 11)
print("usable    :", 1.0 + fi.epsilon != 1.0, 1.0 + fi.epsilon / 2 == 1.0)
print("types     :", type(fi.max).__name__, type(fi.max_exp).__name__)

print()
print("--- int_info ---")
ii = sys.int_info
print("digits    :", ii.bits_per_digit, ii.sizeof_digit)
print("max str   :", ii.default_max_str_digits, ii.str_digits_check_threshold)
print("positional:", list(ii) == [ii[0], ii[1], ii[2], ii[3]])

print()
print("--- hash_info: the numbers that are ours, checked against ours ---")
hi = sys.hash_info
print("width     :", hi.width, hi.hash_bits)
print("modulus   :", hi.modulus == 2 ** 61 - 1)
print("inf/nan   :", hi.inf, hi.nan)
print("imag      :", hi.imag)
# The modulus is not decoration: it is what int hashing actually uses.
print("used      :", hash(2 ** 61 - 1) == 0)
print("algorithm :", type(hi.algorithm).__name__, len(hi.algorithm) > 0)
print("len       :", len(hi) == 9)

print()
print("--- an unknown name still raises, and getattr still defaults ---")
for o, n in ((v, "nope"), (fi, "nope"), (ii, "nope"), (hi, "nope")):
    try:
        getattr(o, n)
        print("found?!")
    except AttributeError:
        print("AttributeError")
print("default   :", getattr(v, "nope", "fallback"))
print("hasattr   :", hasattr(v, "major"), hasattr(v, "nope"))

print()
print("--- churn: the tail and the item array are separate allocations ---")
kept = [sys.version_info, sys.float_info, sys.int_info, sys.hash_info]
copies = [tuple(x) for x in kept]
print("churn     :", len([[i, i] for i in range(3000)]))
print("intact    :", [tuple(x) == c for x, c in zip(kept, copies)])
print("named     :", kept[0].major, kept[1].radix, kept[2].sizeof_digit)
print("reprs     :", [repr(x).split("(")[0] for x in kept])
