# str.maketrans, and its one-argument dict form.
#
# str.maketrans({...}) did not exist -- one argument was "maketrans requires 2
# or 3 string arguments" -- and pathlib builds its table that way, so pathlib
# could not import.  The two- and three-argument forms did not check that they
# had been given strings either, so str.maketrans("ab", 1) read the int as a
# PyStrObject.
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
t("2 args", lambda: sorted(str.maketrans("ab","xy").items()))
t("3 args", lambda: sorted(str.maketrans("ab","xy","cd").items()))
t("dict mixed", lambda: sorted(str.maketrans({'a':1,'b':None,99:'z',100:5}).items(), key=lambda kv: kv[0]))
t("dict empty", lambda: str.maketrans({}))
t("dict subclass", lambda: (lambda D: sorted(str.maketrans(D({'a':'X'})).items()))(type('D',(dict,),{})))
t("dict bool key", lambda: sorted(str.maketrans({True:'x'}).items()))
t("dict big key", lambda: sorted(str.maketrans({0x1F600:'x'}).items()))
t("dict unicode key", lambda: sorted(str.maketrans({'é':'x'}).items()))
t("no args", lambda: str.maketrans())
t("one list", lambda: str.maketrans([1]))
t("one str", lambda: str.maketrans("ab"))
t("4 args", lambda: str.maketrans("a","b","c","d"))
t("bad key type", lambda: str.maketrans({1.5:'x'}))
t("long key", lambda: str.maketrans({"ab":"x"}))
t("(1,2)", lambda: str.maketrans(1,2))
t("('ab',1)", lambda: str.maketrans("ab",1))
t("('ab','cd',1)", lambda: str.maketrans("ab","cd",1))
t("({},'a')", lambda: str.maketrans({},'a'))
t("unequal", lambda: str.maketrans("ab","xyz"))
t("translate from dict", lambda: "abcd".translate(str.maketrans({'a':'X','b':None})))
t("unicode roundtrip", lambda: "éx".translate(str.maketrans({'é':'E'})))
t("emoji", lambda: "a\U0001F600".translate(str.maketrans({'\U0001F600':'!'})))
t("str subclass key", lambda: (lambda S: sorted(str.maketrans({S('a'):'X'}).items()))(type('S',(str,),{})))
