# A differential test for the regex engine: several hundred patterns run
# against both interpreters and diffed.
#
# This is the test that did not exist, and its absence is why the engine
# shipped with silently wrong answers in its commonest idioms.  The two
# existing sre tests feed HAND-WRITTEN integer arrays to _sre.compile(), and
# not one of those arrays contains REPEAT, REPEAT_ONE, IN or ASSERT --
# precisely the opcodes that were broken.  `.*x` matched "abc".
#
# It needs CPython's Lib/re on the path, so it is deliberately not named
# test_*.py -- run_tests.sh auto-discovers those.  `make check-re` runs it
# through tests/re_probe.sh, which ratchets against tests/re_floor.txt.
# tests/test_re_opcodes.py is the version that runs in the ordinary gate.
#
# Only span() and groups() are printed, never the Match object itself: our
# repr does not carry the span, which would swamp the signal.

import sys

try:
    import re
except ImportError:                                     # pragma: no cover
    print("re unavailable; set CPYTHON_LIB")
    sys.exit(0)


def show(pattern, subject, flags=0):
    try:
        rx = re.compile(pattern, flags)
    except BaseException as e:
        print("%-24r %-14r compile %s" % (pattern, subject, type(e).__name__))
        return
    out = []
    for name, fn in (("match", rx.match), ("search", rx.search)):
        try:
            m = fn(subject)
            out.append("%s=%s" % (name, m.span() if m else None))
        except BaseException as e:
            out.append("%s=%s" % (name, type(e).__name__))
    try:
        m = rx.match(subject)
        out.append("groups=%r" % (m.groups() if m else None))
    except BaseException as e:
        out.append("groups=%s" % type(e).__name__)
    try:
        out.append("findall=%r" % (rx.findall(subject),))
    except BaseException as e:
        out.append("findall=%s" % type(e).__name__)
    print("%-24r %-14r %s" % (pattern, subject, " ".join(out)))


print("--- literals and any ---")
for p in ("abc", "a.c", "...", "a", "", "abcd"):
    for s in ("abc", "", "abcd", "xbc"):
        show(p, s)

print()
print("--- greedy quantifiers, and the backtracking they need ---")
for p in (".*", ".*a", ".*b", ".*c", ".*x", ".+", ".+a", ".+c",
          "a*", "a+", "a?", "a*b", "a*a", "a.*b", "a.*c",
          "a{2}", "a{2,}", "a{2,3}", "a{0,1}", "a{,2}", "a{3,}"):
    for s in ("abc", "aaa", "aab", "", "a"):
        show(p, s)

print()
print("--- lazy and possessive ---")
for p in (".*?", ".*?c", "a*?b", "a+?b", "a??", "a??b", ".+?",
          "a*+", "a*+b", "(?>a*)b", "(?>a*)a"):
    for s in ("aab", "abc", "aaa", ""):
        show(p, s)

print()
print("--- character classes ---")
for p in ("[abc]", "[abc]+", "[^abc]", "[^abc]+", "[a-z]", "[a-z]+",
          "[0-9]", "[0-9]+", "[a-zA-Z0-9_]+", "[]]", "[^]]", "[-a]",
          r"[\d]+", r"[\w]+", r"[\s]", r"[\D]", r"[\W]", r"[\S]+",
          "[abc][def]", "[a-c]{2}"):
    for s in ("abc", "def", "123", "a-c", "]", " ", ""):
        show(p, s)

print()
print("--- escapes ---")
for p in (r"\d", r"\d+", r"\D+", r"\w+", r"\W", r"\s", r"\S+",
          r"\bfoo\b", r"\Bfoo", r"\A abc", r"\Aabc", r"abc\Z", r"a\Z",
          r"\.", r"\\", r"\n", r"\t"):
    for s in ("abc", "123", "foo", "a.b", "\\", "\n", " abc"):
        show(p, s)

print()
print("--- anchors ---")
for p in ("^abc", "abc$", "^abc$", "^$", "^", "$", "^a.*c$"):
    for s in ("abc", "", "abcd", "xabc"):
        show(p, s)

print()
print("--- groups, alternation, backreferences ---")
for p in ("(a)", "(a)(b)", "(abc)", "(?:abc)", "(?P<n>a)(?P=n)",
          "a|b", "a|b|c", "(a|b)c", "(a|b)*c", "(a|b)+", "(ab|cd)+",
          r"(a)\1", r"(a)(b)\2\1", "(a)?b", "(a)?", "((a)(b))",
          "(?:a|b)+", "x(?:yz)?", "(ab)+", "(ab)*", "()", "(){2}"):
    for s in ("abc", "aa", "abab", "cd", "b", "x", "xyz", ""):
        show(p, s)

print()
print("--- lookaround ---")
for p in ("(?=abc)a", "(?=a)a", "(?=b)a", "(?!b)a", "(?!a)a",
          "(?<=a)b", "(?<=x)b", "(?<!a)b", "(?<!x)b",
          "a(?=b)", "a(?!b)", "(?=.*c)ab", "(?<=ab)c"):
    for s in ("abc", "ab", "b", "xb", "a"):
        show(p, s)

print()
print("--- conditionals and named groups ---")
for p in ("(a)?b(?(1)c|d)", "(?P<x>a)(?P=x)", "(?P<y>a)b"):
    for s in ("abc", "bd", "aa", "ab"):
        show(p, s)

print()
print("--- flags ---")
show("ABC", "abc", re.IGNORECASE)
show("[a-z]+", "ABC", re.IGNORECASE)
show(".", "\n", re.DOTALL)
show(".+", "a\nb", re.DOTALL)
show("^b", "a\nb", re.MULTILINE)
show("a$", "a\nb", re.MULTILINE)
show(" a b ", "ab", re.VERBOSE)
show("(?i)ABC", "abc")
show("(?s).", "\n")
show("(?s:.*)", "a\nb")
show(r"(?s:.*\.txt)\Z", "f.txt")
show(r"(?s:.*\.txt)\Z", "f.doc")

print()
print("--- the shapes fnmatch and the stdlib build ---")
for p in (r"(?s:.*)\Z", r"(?s:f.*\.txt)\Z", r"(?s:[abc])\Z",
          r"(?s:a|b)\Z", r"(?s:.)\Z", r"(?s:)\Z"):
    for s in ("f.txt", "abc", "a", ""):
        show(p, s)

print()
print("--- nesting and pathological shapes, bounded ---")
for p in ("(a*)*b", "(a|a)*b", "((a)*)*", "(a+)+b", "(?:a{2}){2}",
          "a{1,2}{1,2}" if False else "(?:a{1,2}){1,2}"):
    for s in ("aaab", "aaa", "b"):
        show(p, s)

print()
print("--- sub, split and finditer, which drive the engine differently ---")
for p, repl, s in ((r"\d+", "#", "a1b22c333"), ("a", "X", "aaa"),
                   ("(a)(b)", r"\2\1", "abab"), ("", "-", "abc"),
                   (r"\s+", " ", "a  b   c")):
    try:
        print("sub  %-10r %-6r %-12r %r" % (p, repl, s, re.sub(p, repl, s)))
    except BaseException as e:
        print("sub  %-10r %-6r %-12r %s" % (p, repl, s, type(e).__name__))
for p, s in ((r",", "a,b,c"), (r"\s+", "a b  c"), (r"(,)", "a,b"),
             (r"x*", "abc"), (r"\d", "a1b2")):
    try:
        print("split %-10r %-12r %r" % (p, s, re.split(p, s)))
    except BaseException as e:
        print("split %-10r %-12r %s" % (p, s, type(e).__name__))
for p, s in ((r"\w+", "ab cd ef"), (r"a", "aaa"), (r"(a)(b)", "abab")):
    try:
        print("finditer %-8r %-10r %r" % (p, s, [m.span() for m in re.finditer(p, s)]))
    except BaseException as e:
        print("finditer %-8r %-10r %s" % (p, s, type(e).__name__))

print()
print("--- match object surface ---")
m = re.match(r"(?P<first>a+)(b*)(?P<last>c)", "aabbc")
print("span   :", m.span(), m.span(1), m.span(2), m.span(3))
print("group  :", m.group(), m.group(1), m.group("first"), m.group(2, 3))
print("groups :", m.groups())
print("dict   :", sorted(m.groupdict().items()))
print("start  :", m.start(), m.end(), m.pos, m.endpos)
print("lastidx:", m.lastindex, m.lastgroup)
print("expand :", m.expand(r"\1-\2-\3"))
mm = re.match(r"(a)?(b)", "b")
print("unmatch:", mm.groups(), mm.span(1), mm.group(1))
print("default:", mm.groups("Z"), mm.groupdict())

# --- sub/subn accepted only the exact str type for a template, so a str
# SUBCLASS fell through with the template and the literal both NULL and the
# expander dereferenced one.  Anything that is neither callable nor a str is
# CPython's TypeError, and the slot holds a PAYLOAD, so an int immediate of 5
# looks exactly like the address 5 unless the tag is what gets tested.

class S(str):
    pass


print(re.sub("a", S("b"), "aaa"))
print(re.sub("(a)", S(r"[\1]"), "aaa"))
print(re.subn("a", S("b"), "aaa"))
for bad in [5, None, 2.5, [1]]:
    try:
        re.sub("a", bad, "aaa")
        print(type(bad).__name__, "NO ERROR")
    except TypeError:
        print(type(bad).__name__, "TypeError")
print(re.sub("a", "b", "aaa"), re.sub("a", lambda m: "X", "aaa"))

# --- fullmatch reaches its end-of-string test only at the top level, and a
# continuation of the same match -- a branch, a repeat body, the tail after
# MAX_UNTIL -- is still that same match.  Hardcoding "toplevel" at those five
# sites made a lookahead's tail answer for the whole pattern.
#
# The repeat's last_pos has to be put BACK when a body attempt fails, as
# CPython's save_last_ptr is; discarding it left the zero-width guard
# comparing against a position already backtracked out of.
for p, s in [(r"(a*)*", "aab"), (r"(\d*)*", "12x"), (r"(?=a*)ab", "ab"),
             (r"(a*)*", "aaa"), (r"([a-z]*)+", "abc"), (r"a*", "aaa"),
             (r"(ab)*", "abab")]:
    m = re.fullmatch(p, s)
    print(repr(p), repr(s), m.span() if m else None)

# --- A nullable body under a repeat with a lower bound.  MAX_UNTIL and
# MIN_UNTIL both write rep->count only in the branch that is about to attempt
# the body, and put it back before the tail is tried; incrementing on the way
# in and decrementing on each way out instead spent one count twice, and the
# enclosing repeat iterated from a count it had already used.  `(a*)+` over
# 'a1' recursed until the depth limit rather than answering None, and every
# pattern here is one that used to.
for p, s in [(r"(a*)+", "a1"), (r"(a*)+", "aa1"), (r"(a*)+", "aa"),
             (r"([a-z]*)+", "abc1"), (r"(a*){1,3}", "a1"),
             (r"(a*)+$", "a1"), (r"(?:a*)+", "a1"), (r"(a*)+?", "a1"),
             (r"(a*?)+", "a1"), (r"(a*?)+b", "aab"), (r"(a*?)*b", "aab"),
             (r"(a??)+", "a1"), (r"(|a)+", "aa"), (r"(a|)+b", "aab"),
             (r"()+", "a"), (r"()*", "a"), (r"(a?)+", "aab"),
             (r"(a?)*b", "aab"), (r"((a*)*)*b", "aab"),
             (r"(a*)+(b*)+c", "aabbc"), (r"(x*)+y", "xxxxxxxxz"),
             (r"(a*)+", ""), (r"(a*)+", "b")]:
    for fn in ("fullmatch", "match", "search"):
        m = getattr(re, fn)(p, s)
        print(fn, repr(p), repr(s), m.span() if m else None,
              m.groups() if m else "")
