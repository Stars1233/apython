# A replacement function that RAISES.
#
# re.sub(pattern, repl, string) with a callable repl calls it once per match.
# The call's result was used without ever being tested for NULL, so when the
# callable raised, the 0 it returned was handed to sre_concat as a string --
# and the exception the caller was owed came out as
#
#     TypeError: sequence item 0: expected str instance,  found
#
# with the type name of the NULL missing from the middle of the message.  That
# is memory `null-means-two-things`: "declined" and "it raised" arrive as the
# same 0, and here the second was read as the first.
#
# string.Template.substitute is what found it.  CPython's string.py is
#
#     def convert(mo):
#         named = mo.group('named') or mo.group('braced')
#         if named is not None:
#             return str(mapping[named])
#         ...
#     return self.pattern.sub(convert, self.template)
#
# so a missing key is a KeyError raised INSIDE a sub callable, and every
# Template with a name the mapping does not carry reported the wrong error.
import re

# --- sub and subn, first match ----------------------------------------
def boom(m):
    raise KeyError("k")


for name, call in (("sub", lambda: re.sub("a", boom, "aaa")),
                   ("subn", lambda: re.subn("a", boom, "aaa")),
                   ("Pattern.sub", lambda: re.compile("a").sub(boom, "aaa")),
                   ("Pattern.subn", lambda: re.compile("a").subn(boom, "aaa"))):
    try:
        call()
        print("%-14s NOT RAISED" % name)
    except KeyError as exc:
        print("%-14s KeyError %s" % (name, exc))


# --- and on a LATER match, after a replacement already succeeded -------
def once(m):
    if m.start() == 0:
        return "X"
    raise ValueError("second")


for name, call in (("sub", lambda: re.sub("a", once, "aa")),
                   ("subn", lambda: re.subn("a", once, "aa"))):
    try:
        call()
        print("%-14s NOT RAISED" % name)
    except ValueError as exc:
        print("%-14s ValueError %s" % (name, exc))

# --- every exception type reaches the caller unchanged ----------------
for exc_type in (KeyError, ValueError, TypeError, ZeroDivisionError,
                 StopIteration, RuntimeError, AttributeError):
    def raiser(m, t=exc_type):
        raise t("from the callable")
    try:
        re.sub("x", raiser, "x")
        print("%-20s NOT RAISED" % exc_type.__name__)
    except exc_type as exc:
        print("%-20s %s" % (exc_type.__name__, exc))

# --- on bytes as well as str ------------------------------------------
try:
    re.sub(b"a", boom, b"a")
    print("bytes: NOT RAISED")
except KeyError:
    print("bytes: KeyError")

# --- and what must still work -----------------------------------------
print("callable:", re.sub("a", lambda m: m.group(0).upper(), "abca"))
print("callable subn:", re.subn("a", lambda m: "-", "aba"))
print("None replaces with nothing:", re.sub("a", lambda m: None, "aba"))
print("a non-str return is still a TypeError:", end=" ")
try:
    re.sub("a", lambda m: 1, "a")
    print("NOT RAISED")
except TypeError as exc:
    print("yes")
print("count honoured:", re.sub("a", lambda m: "-", "aaa", count=2))
print("no match, never called:", re.sub("z", boom, "abc"))
print("template repl untouched:", re.sub(r"(a)(b)", r"\2\1", "ab"))

# The Template case that found this is in tests/test_string_operator.py,
# because it needs CPython's string.py rather than the eleven-line stand-in
# this tree carried when the bug was found.
print("survived")
