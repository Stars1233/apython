# Test _sre module basic functionality
import _sre

# Test that constants exist
assert _sre.MAGIC == 20221023
assert _sre.CODESIZE == 4
assert _sre.MAXREPEAT == 0xFFFFFFFF
assert _sre.MAXGROUPS == 0x3FFFFFFF

print("_sre constants OK")

# Test getcodesize
assert _sre.getcodesize() == 4
print("getcodesize OK")

# Test compile - create a simple pattern for literal "hello"
# SRE bytecode: MARK 0, LITERAL h, LITERAL e, LITERAL l, LITERAL l, LITERAL o, MARK 1, SUCCESS
code = [
    17, 0,    # MARK 0
    16, 104,  # LITERAL 'h'
    16, 101,  # LITERAL 'e'
    16, 108,  # LITERAL 'l'
    16, 108,  # LITERAL 'l'
    16, 111,  # LITERAL 'o'
    17, 1,    # MARK 1
    1,        # SUCCESS
]

pattern = _sre.compile("hello", 0, code, 0, {}, ())
print("compile OK")

# Test match
m = pattern.match("hello world")
assert m is not None, "match should succeed"
print("match OK")

# Test group
assert m.group() == "hello", f"expected 'hello', got '{m.group()}'"
assert m.group(0) == "hello"
print("group OK")

# Test start/end/span
assert m.start() == 0
assert m.end() == 5
sp = m.span()
assert sp[0] == 0 and sp[1] == 5, f"span failed: {sp}"
print("start/end/span OK")

# Test search
m2 = pattern.search("say hello world")
assert m2 is not None, "search should find 'hello'"
assert m2.group() == "hello"
assert m2.start() == 4
assert m2.end() == 9
print("search OK")

# Test no match
m3 = pattern.match("goodbye")
assert m3 is None, "match should fail"
print("no match OK")

# Test fullmatch
m4 = pattern.fullmatch("hello")
assert m4 is not None
m5 = pattern.fullmatch("hello world")
assert m5 is None
print("fullmatch OK")

# Test findall
results = pattern.findall("hello world hello")
assert results == ["hello", "hello"], f"findall failed: {results}"
print("findall OK")

# Test sub
result = pattern.sub("HI", "hello world hello")
assert result == "HI world HI", f"sub failed: {result}"
print("sub OK")

# Test split with a simple space pattern
space_code = [
    17, 0,    # MARK 0
    16, 32,   # LITERAL ' '
    17, 1,    # MARK 1
    1,        # SUCCESS
]
space_pat = _sre.compile(" ", 0, space_code, 0, {}, ())
parts = space_pat.split("hello world foo")
assert parts == ["hello", "world", "foo"], f"split failed: {parts}"
print("split OK")

# Test with groups
group_code = [
    17, 0,    # MARK 0 (group 0 start)
    17, 2,    # MARK 2 (group 1 start)
    16, 97,   # LITERAL 'a'
    16, 98,   # LITERAL 'b'
    16, 99,   # LITERAL 'c'
    17, 3,    # MARK 3 (group 1 end)
    17, 1,    # MARK 1 (group 0 end)
    1,        # SUCCESS
]
group_pat = _sre.compile("(abc)", 0, group_code, 1, {}, ())
gm = group_pat.match("abcdef")
assert gm is not None
assert gm.group(0) == "abc"
assert gm.group(1) == "abc"
g = gm.groups()
assert len(g) == 1 and g[0] == "abc", f"groups failed: {g}"
print("groups OK")

# Test __getitem__ (mp_subscript)
assert gm[0] == "abc", f"__getitem__(0) failed: {gm[0]}"
assert gm[1] == "abc", f"__getitem__(1) failed: {gm[1]}"
print("__getitem__ OK")

# Test regs attribute
regs = gm.regs
assert len(regs) == 2, f"regs length wrong: {len(regs)}"
assert regs[0][0] == 0 and regs[0][1] == 3, f"regs[0] wrong: {regs[0]}"
assert regs[1][0] == 0 and regs[1][1] == 3, f"regs[1] wrong: {regs[1]}"
print("regs OK")

# Test subn with count
result2 = pattern.subn("HI", "hello world hello")
assert result2[0] == "HI world HI", f"subn result wrong: {result2[0]}"
assert result2[1] == 2, f"subn count wrong: {result2[1]}"
print("subn OK")

# Test subn with count limit
result3 = pattern.subn("HI", "hello world hello", 1)
assert result3[0] == "HI world hello", f"subn count=1 result wrong: {result3[0]}"
assert result3[1] == 1, f"subn count=1 count wrong: {result3[1]}"
print("subn count limit OK")

# Test expand stub (returns template as-is)
em = pattern.match("hello world")
expanded = em.expand("replacement")
assert expanded == "replacement", f"expand failed: {expanded}"
print("expand OK")

# Test __copy__ / __deepcopy__
p_copy = pattern.__copy__()
assert p_copy is pattern, "__copy__ should return same object"
m_copy = em.__copy__()
assert m_copy is em, "match __copy__ should return same object"
# __deepcopy__ takes a memo arg but returns self
p_deep = pattern.__deepcopy__({})
assert p_deep is pattern, "__deepcopy__ should return same object"
print("copy/deepcopy OK")

# Test finditer
matches = list(pattern.finditer("hello world hello"))
assert len(matches) == 2, f"finditer count wrong: {len(matches)}"
assert matches[0].group() == "hello"
assert matches[1].group() == "hello"
assert matches[0].start() == 0
assert matches[1].start() == 12
print("finditer OK")

# Test scanner methods
scanner = pattern.scanner("hello world hello")
m1 = scanner.search()
assert m1 is not None and m1.group() == "hello"
m2 = scanner.search()
assert m2 is not None and m2.group() == "hello"
m3 = scanner.search()
assert m3 is None
print("scanner OK")

# Test named groups with __getitem__ and groupdict
named_code = [
    17, 0,    # MARK 0 (group 0 start)
    17, 2,    # MARK 2 (group 1 start = named 'word')
    16, 120,  # LITERAL 'x'
    17, 3,    # MARK 3 (group 1 end)
    17, 1,    # MARK 1 (group 0 end)
    1,        # SUCCESS
]
named_pat = _sre.compile("(?P<word>x)", 0, named_code, 1, {"word": 1}, (None, "word"))
nm = named_pat.match("xyz")
assert nm is not None

# Test __getitem__ with string key
assert nm["word"] == "x", f"named __getitem__ failed: {nm['word']}"
print("named __getitem__ OK")

# Test groupdict
gd = nm.groupdict()
assert len(gd) == 1, f"groupdict length wrong: {len(gd)}"
assert "word" in gd, f"'word' not in groupdict: {gd}"
assert gd["word"] == "x", f"groupdict['word'] wrong: {gd['word']}"
print("groupdict OK")

# Test match on no-match returns None for finditer (empty iteration)
empty_matches = list(pattern.finditer("goodbye"))
assert len(empty_matches) == 0, f"finditer on no-match should be empty: {len(empty_matches)}"
print("finditer empty OK")

# Test match attributes
assert em.re is pattern, "match.re should be the pattern"
assert em.string == "hello world", f"match.string wrong: {em.string}"
assert em.pos == 0, f"match.pos wrong: {em.pos}"
print("match attributes OK")

# Test pattern attributes
assert pattern.pattern == "hello", f"pattern.pattern wrong: {pattern.pattern}"
assert pattern.groups == 0, f"pattern.groups wrong: {pattern.groups}"
assert pattern.flags == 0, f"pattern.flags wrong: {pattern.flags}"
print("pattern attributes OK")

# =================================================================
# Gap 4: lastgroup attribute
# =================================================================
# Named group with lastgroup
nm2 = named_pat.match("xyz")
assert nm2.lastgroup == "word", f"lastgroup wrong: {nm2.lastgroup}"
print("lastgroup (named) OK")

# No group match → lastgroup is None
assert m.lastgroup is None, f"lastgroup should be None: {m.lastgroup}"
print("lastgroup (none) OK")

# =================================================================
# Gap 1: String group names in group/start/end/span
# =================================================================
assert nm2.group("word") == "x", f"group('word') failed: {nm2.group('word')}"
assert nm2.start("word") == 0, f"start('word') failed: {nm2.start('word')}"
assert nm2.end("word") == 1, f"end('word') failed: {nm2.end('word')}"
sp2 = nm2.span("word")
assert sp2[0] == 0 and sp2[1] == 1, f"span('word') failed: {sp2}"
print("string group names OK")

# =================================================================
# Gap 7: Pattern __hash__ / __eq__
# =================================================================
# Same pattern → equal
p1 = _sre.compile("hello", 0, code, 0, {}, ())
p2 = _sre.compile("hello", 0, code, 0, {}, ())
assert p1 == p2, "identical patterns should be equal"
assert not (p1 != p2), "identical patterns should not be unequal"

# Different pattern string → not equal
p3 = _sre.compile("world", 0, code, 0, {}, ())
assert p1 != p3, "different patterns should not be equal"

# Hash: same patterns have same hash
assert hash(p1) == hash(p2), "identical patterns should have same hash"
print("pattern hash/eq OK")

# =================================================================
# Gap 2: Callable repl in sub/subn
# =================================================================
# Use a callable that uppercases the match
def upper_repl(m):
    return m.group(0).upper()

sub_result = pattern.sub(upper_repl, "hello world hello")
assert sub_result == "HELLO world HELLO", f"callable sub failed: {sub_result}"
print("callable sub OK")

# subn with callable
subn_result = pattern.subn(upper_repl, "hello world hello")
assert subn_result[0] == "HELLO world HELLO", f"callable subn result wrong: {subn_result[0]}"
assert subn_result[1] == 2, f"callable subn count wrong: {subn_result[1]}"
print("callable subn OK")

# subn with callable and count limit
subn_result2 = pattern.subn(upper_repl, "hello world hello", 1)
assert subn_result2[0] == "HELLO world hello", f"callable subn count=1 wrong: {subn_result2[0]}"
assert subn_result2[1] == 1, f"callable subn count=1 count wrong: {subn_result2[1]}"
print("callable subn count limit OK")

print("All _sre tests passed!")
