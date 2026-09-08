# Two places where bytes and str disagreed with each other about the same call.
#
#   b"ab".replace(b"", b"Z")  was b"ab" where str gives "ZaZbZ": an empty
#     needle is not "no match", it is an insertion before every byte and once
#     more at the end.  bytes.replace also refused the count argument that
#     methods/init.asm has always registered it as taking, and that
#     str.replace has always honoured.
#
#   "ab".center(3, "*")  was "ab*" where bytes.center gives b"*ab".  CPython's
#     rule is left = marg//2 + (marg & width & 1), so the odd character goes
#     on the LEFT, and only when the width is odd as well.


def replace_sweeps():
    out = []
    for s in (b"", b"a", b"ab", b"abc", b"aaa", b"abab"):
        for old in (b"", b"a", b"ab", b"z"):
            for new in (b"", b"Z", b"YZ"):
                out.append(s.replace(old, new))
                out.append(bytearray(s).replace(old, new))
                for c in (0, 1, 2, 5, -1):
                    out.append(s.replace(old, new, c))
                    out.append(bytearray(s).replace(old, new, c))
    return len(out), out[:6], out[-6:]


def replace_agrees_with_str():
    bad = []
    for s in ("", "a", "ab", "abc", "aaa", "abab"):
        for old in ("", "a", "ab", "z"):
            for new in ("", "Z", "YZ"):
                for c in (None, 0, 1, 2, 5, -1):
                    if c is None:
                        a, b = s.replace(old, new), s.encode().replace(
                            old.encode(), new.encode())
                    else:
                        a, b = s.replace(old, new, c), s.encode().replace(
                            old.encode(), new.encode(), c)
                    if a.encode() != b:
                        bad.append((s, old, new, c, a, b))
    return bad


def center_agrees_with_bytes():
    bad = []
    for s in ("", "a", "ab", "abc", "abcd"):
        for w in range(0, 10):
            a, b = s.center(w, "*"), s.encode().center(w, b"*")
            if a.encode() != b:
                bad.append((s, w, a, b))
    return bad


def center_values():
    return [("ab".center(w, "*"), b"ab".center(w, b"*")) for w in range(0, 8)]


def ljust_and_rjust_unmoved():
    return "ab".ljust(5, "*"), "ab".rjust(5, "*"), b"ab".ljust(5, b"*")


print(replace_sweeps())
print(replace_agrees_with_str())
print(center_agrees_with_bytes())
print(center_values())
print(ljust_and_rjust_unmoved())
