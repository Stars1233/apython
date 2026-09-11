# The attributes a type's tp_getattr answers are on the TYPE as well.
#
# dir() sees a type's tp_dict over the MRO plus the instance dict, and nothing
# else -- so an attribute answered only by a tp_getattr could never appear in
# one, and could never be reached through the type either.  A great deal of the
# standard library asks exactly that way: inspect.getmembers walks dir(), pydoc
# walks dir(), unittest.mock's autospec walks dir(), and types.py takes
# GetSetDescriptorType from `type(FunctionType.__code__)`.
#
# An exception did not list `args`; a function did not list `__name__`; a frame
# did not list `f_back`; and a code object listed none of its sixteen co_* at
# all, because code_type had no tp_dict whatsoever.
#
# Three of these were more than a listing.  An unbound builtin's __self__ was
# an AttributeError where CPython answers the module it belongs to;
# reversed(d.keys()) was "not reversible" although the dict itself has had a
# __reversed__ all along; and a class answered neither __annotations__ nor
# __type_params__.

import sys


def has(obj, *names):
    return [n for n in names if n not in dir(obj)]


# --- exceptions -------------------------------------------------------------
e = ValueError(1, 2)
# __notes__ is not among them: PEP 678 creates it on the first add_note(), and
# CPython does not list it before then either.
print(has(e, "args", "__traceback__", "__cause__", "__context__",
          "__suppress_context__"), "exception names missing")
print(e.args, "and args still reads")

# --- functions --------------------------------------------------------------
def sample(a, b=2, *args, **kw):
    """doc"""
    return a


print(has(sample, "__name__", "__qualname__", "__defaults__", "__kwdefaults__",
          "__closure__", "__annotations__", "__module__", "__doc__",
          "__dict__", "__code__", "__globals__"), "function names missing")
print(sample.__name__, sample.__defaults__, "and they still read")

# --- frames -----------------------------------------------------------------
frame = sys._getframe()
print(has(frame, "f_back", "f_code", "f_globals", "f_builtins", "f_locals",
          "f_lineno", "f_lasti", "f_trace", "f_trace_lines",
          "f_trace_opcodes"), "frame names missing")
print(type(frame.f_lineno).__name__, frame.f_code.co_name, "and they still read")

# --- code objects -----------------------------------------------------------
code = sample.__code__
print(has(code, "co_name", "co_qualname", "co_filename", "co_consts",
          "co_names", "co_varnames", "co_argcount", "co_posonlyargcount",
          "co_kwonlyargcount", "co_flags", "co_nlocals", "co_stacksize",
          "co_firstlineno", "co_code", "co_linetable",
          "co_exceptiontable"), "code names missing")
print(code.co_name, code.co_argcount, "and they still read")

# --- builtin functions ------------------------------------------------------
print(has(len, "__name__", "__qualname__", "__module__", "__self__"),
      "builtin names missing")
print(len.__self__ is sys.modules["builtins"], "an unbound builtin belongs to builtins")
print([].append.__self__, "a bound one belongs to its receiver")
print(int.from_bytes.__self__ is int, "and a classmethod-ish one to its type")

# --- the dict views ---------------------------------------------------------
d = {1: "a", 3: "b"}
print(list(reversed(d.keys())), "reversed keys")
print(list(reversed(d.values())), "reversed values")
print(list(reversed(d.items())), "reversed items")
print(list(reversed(d)), "reversed dict itself")
print(has(d.keys(), "__reversed__"), "view names missing")

# --- classes ----------------------------------------------------------------
class Plain:
    pass


class Annotated:
    y: int


print(Plain.__annotations__, "a class with none answers an empty dict")
Plain.__annotations__["x"] = int
print(Plain.__annotations__, "and it persists, as CPython's does")
print(Annotated.__annotations__, "one with annotations answers its own")
print(Plain.__type_params__, int.__type_params__, "type params default to ()")
try:
    int.__annotations__
    print(False, "a static type must not grow one")
except AttributeError:
    print(True, "a static type has no __annotations__")

# inspect.getmembers is the reader this is for, and it walks dir(); it is not
# exercised here because this tree's lib/ does not ship inspect and the suite
# runs without CPython's on the path.  Checked by hand: getmembers(e) came back
# with 26 entries against CPython's 33, and matches now.
