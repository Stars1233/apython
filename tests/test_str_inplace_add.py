# `s += t` and `s = s + t` into a local, which resize the string where it
# stands instead of building a new one each time.
#
# What is being tested is not the fast path -- that is easy -- but every way of
# falling off it, because each one is silent when it goes wrong.  The string is
# mutated in place, so anything else that still holds it sees the mutation:
#
#   - a dict or a set holding it as a key would have bucketed it by bytes that
#     are about to change, so a string that has ever been HASHED must not be
#     resized;
#   - any second reference at all -- another name, a list, a tuple -- must
#     block it, which is what the refcount test is for;
#   - `s += s` puts the same object on the stack twice, and if that were not
#     caught the source would be moving underneath the copy;
#   - a str SUBCLASS must produce the subclass's ordinary result, not a
#     resized str.
#
# Every accumulation runs long enough that a quadratic implementation would
# still be correct, so a failure here is a wrong answer and not a slow one.
# The non-ASCII cases check len() at every step: the code-point count is added
# rather than recounted, and a byte count used in its place looks right until
# something asks.


def basic_iadd(n):
    s = ""
    for i in range(n):
        s += "ab"
    return len(s), s[:6], s[-4:]


def basic_add(n):
    s = ""
    for i in range(n):
        s = s + "xy"
    return len(s), s[:6], s[-4:]


def wide(n):
    s = ""
    for i in range(n):
        s += "é中"
    return len(s), len(s.encode()), s[:4], s[-2:]


def growing_right():
    # The right operand grows too, so the copy length changes every step.
    s = ""
    t = "x"
    for i in range(20):
        s += t
        t = t + "y"
    return len(s), s[:10], s[-6:]


def hashed_each_step(n):
    # The accumulator becomes a dict key every iteration, so from the next
    # iteration on it has a cached hash and must not be resized in place.
    s = ""
    d = {}
    for i in range(n):
        s += "z"
        d[s] = i
    return len(s), len(d), d["z" * n], sorted(d.values())[-1]


def hashed_in_set(n):
    s = ""
    seen = set()
    for i in range(n):
        s += "w"
        seen.add(s)
    return len(s), len(seen), ("w" * n) in seen, ("w" * (n // 2)) in seen


def aliased(n):
    # A second reference is kept every tenth step; the strings held must not
    # change afterwards.
    s = ""
    keep = []
    for i in range(n):
        s += "q"
        if i % 10 == 0:
            keep.append(s)
    return len(s), [len(k) for k in keep], keep[0], keep[1]


def aliased_tuple():
    s = "start"
    t = (s,)
    s += "-more"
    return s, t, t[0], len(t[0])


def aliased_name():
    s = "abc"
    other = s
    s += "def"
    return s, other, len(s), len(other)


def self_append():
    s = "ab"
    for i in range(5):
        s += s
    return len(s), s[:4], s[-4:]


def self_append_wide():
    s = "é中"
    for i in range(4):
        s += s
    return len(s), len(s.encode()), s[:2]


def subclass_left():
    class S(str):
        pass

    s = S("a")
    for i in range(5):
        s += "b"
    return s, type(s).__name__, len(s)


def subclass_right():
    class S(str):
        pass

    s = "a"
    for i in range(5):
        s += S("c")
    return s, type(s).__name__, len(s)


def empties():
    s = ""
    for i in range(3):
        s += ""
    t = ""
    t += "x"
    u = "y"
    u += ""
    return repr(s), repr(t), repr(u), len(s), len(t), len(u)


def type_errors():
    out = []
    s = "a"
    for bad in (1, None, b"b", 1.5, ["c"], ("d",)):
        try:
            s += bad
        except TypeError:
            out.append("TypeError")
        else:
            out.append("no error: " + repr(s))
    return out, s


def store_elsewhere(n):
    # The result is stored into a DIFFERENT local from the one the left
    # operand came from, so the local no longer holds the left operand and the
    # in-place path must not be taken.
    a = "A"
    b = ""
    for i in range(n):
        b = a + "z"
    return a, b, len(a), len(b)


def module_level_check():
    # A global accumulator emits STORE_NAME, not STORE_FAST, so it takes the
    # ordinary path; it still has to be right.
    return len(_G), _G[:4]


def per_step_lengths():
    s = ""
    out = []
    for c in ("a", "é", "中", "\U0001f600", "b"):
        s += c
        out.append((len(s), len(s.encode())))
    return out, s


def nul_bytes():
    s = ""
    for i in range(5):
        s += "a\0b"
    return len(s), len(s.encode()), s.count("\0"), repr(s)


def conditional_branch(n):
    # The BINARY_OP is reached from two different paths, only one of which the
    # specializer ever saw.
    s = ""
    for i in range(n):
        if i % 2:
            s += "o"
        else:
            s += "e"
    return len(s), s[:8]


def in_a_loop_with_slicing(n):
    s = ""
    for i in range(n):
        s += "abc"
        if len(s) > 30:
            s = s[-6:]
    return len(s), s


_G = ""
for _i in range(10):
    _G += "g"

print(basic_iadd(500))
print(basic_add(500))
print(wide(300))
print(growing_right())
print(hashed_each_step(40))
print(hashed_in_set(40))
print(aliased(30))
print(aliased_tuple())
print(aliased_name())
print(self_append())
print(self_append_wide())
print(subclass_left())
print(subclass_right())
print(empties())
print(type_errors())
print(store_elsewhere(5))
print(module_level_check())
print(per_step_lengths())
print(nul_bytes())
print(conditional_branch(20))
print(in_a_loop_with_slicing(30))
