# str search: the optional start/end arguments, the replace count, and the
# fact that a str may contain NUL.  find/rfind/index/rindex/count/replace all
# ignored their optional arguments and stopped at the first NUL byte, because
# they went through the C-string ap_strstr and never read args[2]/args[3].

s = "abcabc"

# --- find/rfind with start, and with start+end ---
print(s.find("b"), s.find("b", 3), s.find("b", 2, 4), s.find("b", 5))
print(s.rfind("b"), s.rfind("b", 0, 3), s.rfind("b", 2), s.rfind("b", 5))
print(s.find("abc", 1), s.rfind("abc", 0, 5))
print(s.find("", 3), s.find("", 99), s.rfind("", 2))

# --- negative indices ---
print(s.find("b", -4), s.rfind("b", -6, -3), s.find("c", -1), s.find("a", -100))

# --- index/rindex raise where find returns -1 ---
print(s.index("b"), s.index("b", 3), s.rindex("b"), s.rindex("b", 0, 3))
for args in [("z",), ("b", 5), ("a", 1, 2)]:
    try:
        s.index(*args)
        print("no raise", args)
    except ValueError as e:
        print("ValueError", e)
    try:
        s.rindex(*args)
        print("no raise", args)
    except ValueError as e:
        print("ValueError", e)

# --- count with start/end ---
print(s.count("b"), s.count("b", 3), s.count("b", 0, 3), s.count("b", 5))
print(s.count("abc"), s.count("abc", 1), s.count(""), s.count("", 2, 4))

# --- replace with a count ---
print("aXbXcXd".replace("X", "-"))
print("aXbXcXd".replace("X", "-", 1))
print("aXbXcXd".replace("X", "-", 2))
print("aXbXcXd".replace("X", "-", 0))
print("aXbXcXd".replace("X", "-", -1))
print("aaaa".replace("aa", "b"), "aaaa".replace("aa", "b", 1))
print("abc".replace("", "-"), "abc".replace("", "-", 2))

# --- embedded NUL: the string does not end there ---
n = "a\x00b"
print(len(n), n.find("b"), n.find("\x00"), n.rfind("b"), n.count("b"))
print("b" in n, "\x00" in n, n.index("b"))
print(n.replace("\x00", "-"), len(n.replace("b", "")))
print(n.split("\x00"), n.partition("\x00"))

# --- startswith/endswith are the neighbours of the same family ---
print(s.startswith("bc", 1), s.endswith("ab", 0, 2), s.startswith("abc", 3))

# --- non-ASCII: indices are code points, not bytes ---
u = "αβαβ"     # alpha beta alpha beta
print(len(u), u.find("β"), u.find("β", 2), u.rfind("α"))
print(u.count("α"), u.count("α", 1), u.replace("α", "x", 1))
