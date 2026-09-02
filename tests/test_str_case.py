
# str.title()'s word boundary is the CASED property, not alphabetic:
# CPython's previous_is_cased is _PyUnicode_IsCased(c).  The two differ for
# every alphabetic-but-uncased character -- all of Lo, so Hebrew, Arabic,
# CJK, Thai, Devanagari -- and for every cased-but-not-alphabetic one, the
# Roman numerals and the circled letters among them.
for c in ["אa", "ⒶA", "中文a", "ⅠⅡx",
          "กขa", "अआb", "abcאdef", "ǅab",
          "hello world", "they're", "a1b2c3", "x²y", "1st place"]:
    print(repr(c), "->", repr(c.title()))

# A sweep wide enough to catch a rule that is right for ASCII and wrong
# elsewhere; this was the only failing column of a 20,000-string run.
digest = 0
count = 0
for i in range(0x20, 0x2200):
    ch = chr(i)
    for probe in (ch + "a", "a" + ch + "b", ch + ch + "z"):
        for out in probe.title():
            digest = (digest * 131 + ord(out)) % (2 ** 61 - 1)
        count += 1
print("title sweep", count, digest)
