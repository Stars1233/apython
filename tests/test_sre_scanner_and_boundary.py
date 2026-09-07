"""Two regex bugs that a plain-ASCII test cannot see.

**A scanner stops at its first failure.**  `p.scanner(s).match()` returning
None ends the scan in CPython: every later `match()` and `search()` on that
scanner returns None too.  Here the scanner carried on from wherever the failed
attempt left the position, so `sc.match(); sc.search()` found a match where
CPython finds nothing.  Reproduces on pure ASCII -- it has nothing to do with
encoding.

**`\\b` and `\\B` are ASCII-only.**  The AT handlers hard-coded the ASCII word
category, so a non-ASCII letter was not a word character to them.
`re.search(r"\\b\\d+\\b", "ééé42")` finds `42` here and nothing in CPython,
because those letters ARE word characters and there is no boundary before the
digits.  `\\w` itself was already right, which is what makes this a dispatch
bug rather than a missing table: `sre_uni_isword` exists and answers correctly.

The two are together because both are invisible to the obvious test: the first
needs a scanner reused after a miss, the second needs a non-ASCII subject.
"""

import re


def span(m):
    """A match as (start, end), or None.  The repr of a Match differs here for
    an unrelated reason, and this file is not about that."""
    return None if m is None else m.span()


print("--- a scanner stops at its first failed match ---")
p = re.compile("b")
sc = p.scanner("ab")
print("match at 0:", span(sc.match()))
print("search after that:", span(sc.search()))

sc = p.scanner("ab")
print("search first:", span(sc.search()))
print("match after:", span(sc.match()))
print("search again:", span(sc.search()))

# A scanner that matches repeatedly still walks the whole subject.
p2 = re.compile("a")
sc = p2.scanner("aaa")
print("repeated match:", [span(sc.match()) for _ in range(4)])

sc = p2.scanner("aXaXa")
print("repeated search:", [span(sc.search()) for _ in range(4)])

# match() failing then match() again stays failed.
sc = re.compile("z").scanner("abc")
print("fail then fail:", span(sc.match()), span(sc.match()), span(sc.search()))

# finditer is the scanner's ordinary caller and must not change.
print("finditer:", [m.group() for m in re.finditer("a", "aXaXa")])
print("findall:", re.findall("a", "aXaXa"))

print("--- \\b and \\B over non-ASCII words ---")
acc = "ééé"           # three accented letters: word characters
print("digits after letters:", span(re.search(r"\b\d+\b", acc + "42")))
print("digits before letters:", span(re.search(r"\b\d+\b", "42" + acc)))
print("digits alone:", re.search(r"\b\d+\b", "42").group())
print("digits spaced:", re.search(r"\b\d+\b", acc + " 42").group())

X = "Ç"                   # a non-ASCII letter used as a word
print("word between spaces:", span(re.search(r"\bX\b".replace("X", X), "x " + X + " y")))
print("that word alone:", re.search(r"\bX\b".replace("X", X), X).group())
print("glued to a letter:", span(re.search(r"\bX\b".replace("X", X), "a" + X + "b")))

print("--- \\B is the complement ---")
print("B before digits:", span(re.search(r"\B\d", acc + "42")))
print("B in the middle:", span(re.search(r"\B\d", "a42")))
print("B at a real boundary:", span(re.search(r"\B\d", " 42")))

print("--- the ASCII flag keeps the old meaning ---")
print("ASCII \\b:", span(re.search(r"\b\d+\b", acc + "42", re.ASCII)))
print("ASCII \\B:", span(re.search(r"\B\d", acc + "42", re.ASCII)))

print("--- \\w was already right ---")
print("w matches:", re.findall(r"\w", acc))
print("w with ASCII:", re.findall(r"\w", acc, re.ASCII))
print("W complement:", re.findall(r"\W", acc))

print("--- boundaries at the ends of the subject ---")
for pat, subj in ((r"\bfoo", "foo"), (r"foo\b", "foo"),
                  (r"\bfoo", " foo"), (r"foo\b", "foo "),
                  (r"\B", ""), (r"\b", "")):
    print(repr(pat), repr(subj), "->", bool(re.search(pat, subj)))

print("--- a mixed subject ---")
subj = "café 42 naïve"
print("words:", re.findall(r"\b\w+\b", subj))
print("numbers:", re.findall(r"\b\d+\b", subj))

print("--- bytes patterns are ASCII by definition ---")
print("bytes \\b:", span(re.search(rb"\b\d+\b", b"abc42")))
print("bytes ok:", re.search(rb"\b\d+\b", b"abc 42").group())

print("done")
