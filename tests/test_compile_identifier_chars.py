# An identifier is XID_Start XID_Continue*, not "any byte over 0x7F".
#
# cc_table marks every byte from 0x80 up as both an identifier start and an
# identifier continue -- a permissive UTF-8 lead and nothing more.  So an
# invisible NBSP or ZWSP compiled as a NAME, and so did a lone combining mark:
# `\xa0 = 1` bound a variable whose name cannot be seen, and `f'''{\xa0}'''`
# was a NameError rather than a SyntaxError.  Source that reads as one thing
# and means another is the hazard, and the tables for the real rule were
# already generated.
#
# The refusals live in tests/syntax_corpus.txt, where all five SyntaxError
# fields are compared against CPython's.  This file is the other direction:
# the non-ASCII names that must keep working.

# Letters: Greek, Cyrillic, CJK, accented Latin, and the micro sign, which is
# XID_Start even though it is a Symbol elsewhere in Unicode.
α = 1
Β = 2
да = 3
变量 = 4
été = 5
µ = 6
print(α, Β, да, 变量, été, µ)

# XID_Continue but not XID_Start: a combining mark and an Arabic-Indic digit
# are both legal after the first character.
x́ = 7
y٠ = 8
print(x́, y٠)

# In every position a name can appear.
def é(α, *Β, **да):
    return (α, Β, да)


print(é(1, 2, 3, k=4))


class Å:
    λ = 9

    def méthode(self):
        return self.λ


print(Å().méthode(), Å.__name__, Å.λ)

é.αβ = 10
print(getattr(é, "αβ"))

import sys as sÿ
print(sÿ.version_info[0])

for ι in range(2):
    pass
print(ι)

d = {"α": 1}
print(d["α"], [κ for κ in range(2)])
print((lambda μ: μ + 1)(1))

# A non-identifier character inside a string, a comment or an f-string field's
# text is not an identifier question at all.
print("\xa0" == " ", len("​"), "☃")
# \xa0 a non-breaking space in a comment
print(f"[\xa0]", f"{α}", f"{d['α']}")
