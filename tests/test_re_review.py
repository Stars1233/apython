# The regex engine's answers to things a fuzzer finds and a test file does not.
#
# Every case here crashed, corrupted or silently answered wrong before the
# commit that added it: a lazy repeat leaving stale group marks, a bytes
# replacement taken on trust, a group name looked up as bytes, an escape
# UTF-8-encoded into a bytes template, and a match holding a raw pointer into
# a bytearray that had since moved.

import re


def t(name, fn):
    try:
        print(name, "->", fn())
    except Exception as e:
        print(name, type(e).__name__, str(e)[:70])


print("=== a lazy repeat restores its marks ===")
# The body attempt saves and restores last_pos; without the marks going with
# it a failed iteration left a group with start > end, and group() then asked
# for a slice of negative length.
t("span", lambda: re.compile("(a)(a?)+?a+?").match("aab").span(2))
t("group", lambda: re.compile("(a)(a?)+?a+?").match("aab").group(2))
t("groups", lambda: re.compile("(a)(a?)+?a+?").match("aab").groups())
t("findall", lambda: re.compile("(a)(a?)+?a+?").findall("aab"))
t("split", lambda: re.compile("(a)(a?)+?a+?").split("aab"))
t("lazy alt", lambda: re.compile("(?:ab)+?(a|b)??(?:a|)+").fullmatch("abab").groups())
t("lazy nested", lambda: re.compile("((a)*?)+b").match("aab").groups())
t("lazy bounded", lambda: re.compile("(a)(a?){1,3}?a+?").match("aaab").span(2))

print("=== a callable replacement is checked, not trusted ===")
t("bytes int", lambda: re.sub(b"a", lambda m: 5, b"aba"))
t("bytes str", lambda: re.sub(b"a", lambda m: "XY", b"aba"))
t("bytes bytearray", lambda: re.sub(b"a", lambda m: bytearray(b"XY"), b"aba"))
t("bytes bytes", lambda: re.sub(b"a", lambda m: b"XY", b"aba"))
t("bytes none", lambda: re.sub(b"a", lambda m: None, b"aba"))
t("str int", lambda: re.sub("a", lambda m: 5, "aba"))
t("str bytes", lambda: re.sub("a", lambda m: b"X", "aba"))
t("str str", lambda: re.sub("a", lambda m: "XY", "aba"))

print("=== \\g<name> in a bytes template ===")
t("named bytes", lambda: re.sub(b"(?P<a>a)", rb"[\g<a>]", b"a"))
t("named str", lambda: re.sub("(?P<a>a)", r"[\g<a>]", "a"))
t("expand bytes", lambda: re.match(b"(?P<a>a)", b"a").expand(rb"[\g<a>]"))
t("missing bytes", lambda: re.sub(b"(?P<a>a)", rb"[\g<b>]", b"a"))
t("non-ascii name", lambda: re.sub("(?P<é>a)", "[\\g<é>]", "a"))
t("bad name", lambda: re.sub("(?P<a>a)", "\\g<1a>", "a"))

print("=== an escape in a bytes template is one byte ===")
t("octal bytes", lambda: re.sub(b"(a)", b"\\377", b"a"))
t("octal str", lambda: re.sub("(a)", "\\377", "a"))
t("hex bytes", lambda: re.sub(b"(a)", b"\\x80", b"a"))
t("hex str", lambda: re.sub("(a)", "\\x80", "a"))
t("low octal", lambda: re.sub(b"(a)", b"\\101", b"a"))

print("=== a match reads its subject, not a stale pointer ===")
ba = bytearray(b"hello world")
m = re.search(b"w(or)ld", ba)
print("before", m.group(0), m.group(1), m.span())
ba.clear()
ba.extend(b"Z" * 5000)
print("after", m.group(0), m.group(1))
print("done")
