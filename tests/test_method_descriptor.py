# A method descriptor knows the class it was found on.
#
# One PyBuiltinObject stands for CPython's three descriptor types here --
# builtin_function_or_method, method_descriptor and wrapper_descriptor -- and
# which one it is lives in a field, func_kind.  Two things read that field
# wrongly:
#
#   * __objclass__ was not answered at all, so str.count.__objclass__ was an
#     AttributeError.  inspect reads it, and so does pickle.
#
# __self__ is NOT fixed, and is a recorded divergence.  CPython's
# method_descriptor and wrapper_descriptor have none -- only a bound method
# does, and it is the receiver -- but __self__ is published as a getset on the
# ONE type that stands for all three kinds here, so "absent" would have to be
# a per-object answer from a per-type descriptor.  Nothing depends on it any
# more: builtin_func_reduce reads func_kind directly rather than inferring
# the kind from __self__.
#
# The visible cost was pickling.  __reduce__ on an unbound builtin answered
# its bare name, which is CPython's meth_reduce for a MODULE-level builtin;
# a descriptor uses descr_reduce and answers (getattr, (objclass, name)).
# Without it every `pickle.dumps(str.count)` failed with "it's not found as
# __main__.count", and test_pickle, test_descr and test_functools carried
# about a hundred and eighty of them between them.
#
# What is NOT fixed, and stays a recorded divergence: type(str.count) is
# still builtin_function_or_method rather than method_descriptor, because
# there is one type here where CPython has three.
import copyreg
import sys

# --- __objclass__ ------------------------------------------------------
print("str.count:", str.count.__objclass__)
print("list.append:", list.append.__objclass__)
print("list.__len__:", list.__len__.__objclass__)
print("dict.get:", dict.get.__objclass__)
print("str.maketrans:", getattr(str.maketrans, "__objclass__", "<absent>"))
print("len:", getattr(len, "__objclass__", "<absent>"))

# --- __self__ ----------------------------------------------------------
# A module-level builtin's __self__ is its module, and a bound one's is its
# receiver.  The descriptor case is the recorded divergence above and is not
# compared.
print("len.__self__:", len.__self__)
print("bound:", "x".upper.__self__)

# int.__new__ keeps its __self__: copyreg._reduce_ex walks the MRO comparing
# `base.__new__.__self__ is base` to find the last non-heap base, and without
# it every protocol-0 and protocol-1 reduction is an AttributeError.
print("int.__new__.__self__:", int.__new__.__self__)
print("str.__new__.__self__:", str.__new__.__self__)


class Q(int):
    pass


print("copyreg base:", copyreg._reconstructor is not None,
      Q(5).__reduce_ex__(1)[1][1])

# --- __reduce__, which is what reads them ------------------------------
for f in (str.count, tuple.count, list.append, list.__len__, str.maketrans,
          dict.get):
    r = f.__reduce__()
    print("%-18s %s" % (getattr(f, "__name__", "?"),
                        (r[0].__name__, r[1][0].__name__, r[1][1])
                        if isinstance(r, tuple) else r))

# A module-level builtin still reduces to its bare name.
print("len reduce:", len.__reduce__())

# And the round trip those reductions exist for.
for f in (str.count, list.append, list.__len__, tuple.count, dict.get):
    g = f.__reduce__()
    back = g[0](*g[1])
    print("roundtrip %-14s %s" % (getattr(f, "__name__", "?"), back is f))

# --- the repr, which reads func_kind too -------------------------------
# Addresses are stripped: they are not the same between two interpreters, and
# which WORDING each one gets is the thing func_kind decides.
import re

for o in (str.count, list.__len__, str.maketrans, len, "x".upper):
    print(re.sub(r"0x[0-9a-f]+", "0xX", repr(o)))
print("survived")
