# str(bytes, encoding[, errors]) -- the decoding form of str().
#
# It was missing outright: str(b, "utf-8") answered "str() takes at most 1
# argument".  CPython's re/_parser.py uses it, which is what kept glob and
# fnmatch from importing.  It is the one builtin whose second argument changes
# what the first one means: with an encoding, str() is a decode and takes a
# bytes-like object only, so str("a", "utf-8") is an error and not a copy.
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
t("str()", lambda: str())
t("str(1)", lambda: str(1))
t("str([1])", lambda: str([1]))
t("str(b,utf-8)", lambda: str(b"abc","utf-8"))
t("str(b,ascii)", lambda: str(b"abc","ascii"))
t("str(b,latin-1)", lambda: str(b"\xe9","latin-1"))
t("str(b,utf-8,strict)", lambda: str(b"abc","utf-8","strict"))
t("str(b,ascii,replace)", lambda: str(b"\xff","ascii","replace"))
t("str(b,ascii,ignore)", lambda: str(b"a\xffb","ascii","ignore"))
t("str(ba,utf-8)", lambda: str(bytearray(b"abc"),"utf-8"))
t("str(mv,utf-8)", lambda: str(memoryview(b"abc"),"utf-8"))
t("str(b) no enc", lambda: str(b"abc"))
t("kw encoding", lambda: str(b"abc", encoding="utf-8"))
t("kw errors only", lambda: str(b"\xff", errors="replace"))
t("kw both", lambda: str(b"\xff", encoding="ascii", errors="replace"))
t("kw object", lambda: str(object=b"abc", encoding="utf-8"))
t("kw object only", lambda: str(object=5))
t("kw encoding only", lambda: str(encoding="utf-8"))
t("kw errors only noobj", lambda: str(errors="x"))
t("str(1,'utf-8')", lambda: str(1,"utf-8"))
t("str([],'utf-8')", lambda: str([],"utf-8"))
t("str('a','utf-8')", lambda: str("a","utf-8"))
t("str(b,'nosuch')", lambda: str(b"a","nosuch"))
t("str(b,5)", lambda: str(b"a",5))
t("str(b,'utf-8',5)", lambda: str(b"a","utf-8",5))
t("too many", lambda: str(1,2,3,4))
t("bad kw", lambda: str(b"a", foo=1))
t("bad utf8", lambda: str(b"\xff","utf-8"))
class S(str): pass
t("subclass decode", lambda: (lambda r: (r, type(r).__name__))(S(b"abc","utf-8")))
t("subclass plain", lambda: (lambda r: (r, type(r).__name__))(S("abc")))
class B(bytes): pass
t("bytes subclass", lambda: str(B(b"abc"),"utf-8"))
