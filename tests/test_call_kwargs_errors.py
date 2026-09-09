# CALL_FUNCTION_EX names the callable in every refusal it makes -- CPython's
# _PyObject_FunctionStr: the qualified name, prefixed by the module unless
# that is builtins, then "()".  The duplicate-keyword message names the KEY
# as well, through str(), which is what CPython's %S does; a plain
# "got multiple values for keyword argument" says neither.


def f(**kw):
    return kw


class C:
    def m(self, **kw):
        return kw


lam = lambda **kw: kw

for call in (
    lambda: f(**{"a": 1}, **{"a": 2}),
    lambda: C().m(**{"a": 1}, **{"a": 2}),
    lambda: f(**{1: 1}),
    lambda: f(**{1: 1}, **{1: 2}),
    lambda: dict(**{"a": 1}, **{"a": 2}),
    lambda: len(**{"a": 1}, **{"a": 2}),
    lambda: lam(**{"a": 1}, **{"a": 2}),
    lambda: f(**5),
    lambda: f(**[1]),
    lambda: f(**"ab"),
):
    try:
        call()
    except TypeError as e:
        print(type(e).__name__, e)
    else:
        print("no error")

# a key whose str() is not its repr
class K(str):
    def __str__(self):
        return "shown"


try:
    f(**{K("real"): 1}, **{K("real"): 2})
except TypeError as e:
    print(type(e).__name__, e)

# and the shapes that must still work
print(f(**{"a": 1}, **{"b": 2}))
print(f(**{}, **{"c": 3}))
