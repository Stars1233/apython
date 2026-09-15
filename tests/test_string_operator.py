# `string` and `operator`, CPython's own.
#
# lib/string.py was eleven lines: the five character constants and nothing
# else.  lib/operator.py had the operator functions but none of the three
# objects a program actually reaches for -- itemgetter, attrgetter and
# methodcaller -- which is what `sorted(key=...)` and functools and the whole
# of `dataclasses` are written on.
#
# Both of CPython's files run here unmodified.  operator.py ends with a
# guarded `from _operator import *`, and lib/_operator.py supplies exactly one
# of the names -- `index` -- so the rest are the Python definitions, which is
# the arrangement CPython has when its C module is missing.
import operator
import string

# --- string: the constants, which is all that was here -----------------
print("ascii_lowercase:", string.ascii_lowercase)
print("digits/hexdigits:", string.digits, string.hexdigits)
print("punctuation:", string.punctuation)
print("whitespace:", repr(string.whitespace))
print("printable is a superset:", set(string.digits) <= set(string.printable))

# --- string: capwords --------------------------------------------------
for text, sep in ((" hello  world ", None), ("a-b-c", "-"), ("", None),
                  ("ONE two", None)):
    print("capwords(%r, %r) = %r" % (text, sep, string.capwords(text, sep)))

# --- string: Template --------------------------------------------------
t = string.Template("$who likes ${what} for $$1")
print("substitute:", t.substitute(who="she", what="it"))
print("substitute(mapping):", t.substitute({"who": "he", "what": "that"}))
print("safe_substitute:", string.Template("$a $b").safe_substitute(a=1))
print("get_identifiers:", sorted(t.get_identifiers()))
print("is_valid:", string.Template("$ok").is_valid(),
      string.Template("${}").is_valid())
try:
    string.Template("$missing").substitute()
    print("missing key: NOT REFUSED")
except KeyError as exc:
    print("missing key: KeyError", exc)
try:
    string.Template("${}").substitute()
    print("bad placeholder: NOT REFUSED")
except ValueError as exc:
    print("bad placeholder:", exc)


class Dollar(string.Template):
    delimiter = "%"


print("subclassed delimiter:", Dollar("%x").substitute(x=1))

# --- string: Formatter -------------------------------------------------
f = string.Formatter()
print("format:", f.format("{0}-{k}-{0!r}-{1:>4}", "a", 7, k=2))
print("parse:", list(f.parse("lit{0}mid{k!r:>3}end")))
print("get_field:", f.get_field("0.real", (3,), {}))
print("get_value:", f.get_value(0, ("z",), {}), f.get_value("k", (), {"k": 1}))
print("convert_field:", f.convert_field("a", "r"), f.convert_field(1, "s"))
print("format_field:", f.format_field(3.5, ".2f"))
print("vformat:", f.vformat("{x}{y}", (), {"x": 1, "y": 2}))
print("auto numbering:", f.format("{}{}{}", 1, 2, 3))
try:
    f.format("{}{0}", 1, 2)
    print("mixed numbering: NOT REFUSED")
except ValueError as exc:
    print("mixed numbering:", exc)
print("nested spec:", f.format("{0:{1}}", 7, ">4"))

# --- operator: the three objects that were missing ---------------------
print("itemgetter one:", operator.itemgetter(1)("abc"))
print("itemgetter many:", operator.itemgetter(2, 0)("abc"))
print("itemgetter slice:", operator.itemgetter(slice(1, 3))("abcd"))
print("itemgetter on a dict:", operator.itemgetter("k")({"k": 9}))
print("attrgetter one:", operator.attrgetter("imag")(3j))
print("attrgetter dotted:", operator.attrgetter("real.imag")(3j))
print("attrgetter many:", operator.attrgetter("real", "imag")(1 + 2j))
print("methodcaller:", operator.methodcaller("replace", "a", "b")("aa"))
print("methodcaller kwargs:",
      operator.methodcaller("center", 5, "-")("x"))
print("reprs:", repr(operator.itemgetter(1)),
      repr(operator.attrgetter("x")),
      repr(operator.methodcaller("m", 1)))
print("as a sort key:", sorted([(2, "b"), (1, "a")], key=operator.itemgetter(0)))
for bad, what in ((lambda: operator.itemgetter(), "itemgetter()"),
                  (lambda: operator.attrgetter(), "attrgetter()"),
                  (lambda: operator.methodcaller(), "methodcaller()"),
                  (lambda: operator.attrgetter(1), "attrgetter(1)")):
    try:
        bad()
        print("%-16s NOT REFUSED" % what)
    except TypeError:
        print("%-16s TypeError" % what)

# --- operator: the rest of the surface ---------------------------------
print("countOf:", operator.countOf([1, 1, 2], 1), operator.countOf("aba", "a"))
print("indexOf:", operator.indexOf([1, 2, 3], 2))
print("length_hint:", operator.length_hint(iter([1, 2, 3])),
      operator.length_hint(object(), 5))
print("index:", operator.index(True), operator.index(7))
print("call:", operator.call(len, "abc"))
print("truth/not_:", operator.truth([]), operator.not_([]))
print("is_/is_not:", operator.is_(None, None), operator.is_not(1, 2))
print("concat/iconcat:", operator.concat([1], [2]), operator.iconcat([1], [2]))
print("getitem/setitem/delitem:", end=" ")
d = {}
operator.setitem(d, "a", 1)
got = operator.getitem(d, "a")
operator.delitem(d, "a")
print(got, d)
print("in-place on a list:", operator.iadd([1], [2]), operator.imul([1], 2))
print("arithmetic:", operator.add(1, 2), operator.floordiv(7, 2),
      operator.mod(7, 2), operator.pow(2, 8), operator.matmul.__name__)
print("comparisons:", operator.lt(1, 2), operator.ge(2, 2), operator.eq(1, 1))
print("bitwise:", operator.and_(6, 3), operator.or_(6, 3), operator.xor(6, 3),
      operator.inv(0), operator.lshift(1, 4))
print("unary:", operator.neg(3), operator.pos(-3), operator.abs(-3))
print("dunder aliases agree:",
      operator.__add__ is operator.add, operator.__getitem__ is operator.getitem)
print("__all__ complete:",
      [n for n in operator.__all__ if not hasattr(operator, n)],
      [n for n in string.__all__ if not hasattr(string, n)])

# --- the case that found the sre bug this commit follows ---------------
# Template.substitute is a re.sub() over a callable that raises KeyError for
# a name the mapping does not carry, and re.sub never tested the call's
# result for NULL -- so every missing key reported
# "TypeError: sequence item 0: expected str instance,  found".
# tests/test_re_sub_raises.py is the reduction; this is the shape it came in.
import re

try:
    string.Template("$missing").substitute()
    print("missing name: NOT RAISED")
except KeyError as exc:
    print("missing name: KeyError", exc)
try:
    string.Template("${missing}").substitute(other=1)
    print("missing braced name: NOT RAISED")
except KeyError as exc:
    print("missing braced name: KeyError", exc)


def raising_repl(m):
    raise RuntimeError("from the callable")


try:
    re.sub("x", raising_repl, "x")
    print("a raising sub callable: NOT RAISED")
except RuntimeError as exc:
    print("a raising sub callable:", exc)
print("survived")
