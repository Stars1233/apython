# A bound method whose __func__ is not a function.
#
# method_getattr answered `__self__` and `__func__` itself and delegated
# everything else, choosing between two getattrs by a single comparison:
#
#     cmp [im_func.ob_type], builtin_func_type
#     je  .mg_builtin
#     call func_getattr            <-- everything else, unchecked
#
# So anything that was not a builtin was treated as a PyFunctionObject.
# func_getattr then reads func_dict at offset +64 and hands it to dict_get --
# and +64 on a PyTypeObject is tp_call, a code pointer.  `types.MethodType`
# will bind any callable, and `classmethod(SomeType)` is a shape the stdlib
# actually uses: CPython spells __class_getitem__ as
# `classmethod(types.GenericAlias)` on asyncio.Future and on dozens of other
# classes.  Reading any attribute other than __self__/__func__ off such a
# bound method jumped through a garbage dict pointer.
#
# The path that found it: inspect.unwrap probes __wrapped__ on everything it
# is handed, and unittest.mock.Mock(spec=cls) walks dir(cls) doing exactly
# that -- so `mock.Mock(spec=asyncio.Future)` was a SIGSEGV, and with it four
# of CPython's test_asyncio submodules.
import types

# The reduction: a method over a TYPE, asked for a name it does not have.
m = types.MethodType(types.GenericAlias, list)
print("func:", m.__func__)
print("self:", m.__self__)
print("missing:", getattr(m, "__nope__", "<absent>"))
try:
    m.__nope__
except AttributeError:
    print("bare access raises AttributeError")

# It still answers what it should: calling it builds the alias.
print("called:", m(int))

# The shape the stdlib writes, and the lookup mock makes over it.
class C:
    __class_getitem__ = classmethod(types.GenericAlias)


cg = C.__class_getitem__
print("classmethod func:", cg.__func__)
print("wrapped:", getattr(cg, "__wrapped__", "<absent>"))
print("doc is str or None:", isinstance(getattr(cg, "__doc__", None), (str, type(None))))
print("subscript:", C[int])

# Every other non-function callable bound the same way: a type, a builtin, a
# class with __call__, and a plain function for the control.
class Callable:
    def __call__(self):
        return "called"


for f in (int, dict, len, Callable(), (lambda self: self)):
    b = types.MethodType(f, "recv")
    print(type(f).__name__, "->", getattr(b, "__nope__", "<absent>"))

# __self__ and __func__ keep working over all of them -- they are the method's
# own, and delegating them is what broke `"x".upper.__self__` once before.
b = types.MethodType(int, "5")
print("alien self:", b.__self__, "alien func:", b.__func__)
print("alien call:", b())

# A real function still reaches func_getattr, dict and all.
def plain(self):
    return self


plain.marker = "on the function"
pm = types.MethodType(plain, "recv")
print("function attr through method:", pm.marker)
print("function name through method:", pm.__name__)

# And a builtin still reaches builtin_func_getattr.
print("builtin name through method:", "x".upper.__name__)
print("builtin self through method:", "x".upper.__self__)

import gc

gc.collect()
print("survived")
