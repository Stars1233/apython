# A method descriptor reached unbound with no arguments at all.
#
# builtin_func_call checks that the receiver is of the right type, and skipped
# that check when there were no arguments -- with the comment "no arguments at
# all: the arity check ruled".  The arity check did not rule: min_args is 0
# for everything registered with ADD_FN rather than ADD_FN_N, and 0 means "no
# check".  So the method body was entered with a NULL args array and
# dereferenced args[0].
#
#     def g(): yield 1
#     type(g()).close(*())
#
# is five lines and a SIGSEGV.  Seven more had the same hole: coroutine.close,
# __reduce__ on BaseException, OSError and StopIteration, and __get__ on
# property, classmethod and staticmethod.
#
# It cannot be reached through a BOUND method, which always prepends its
# receiver, so nargs is never 0 there -- which is exactly why
# tests/arity_probe.sh could not see it.  That probe calls every method
# through an instance.  The empty argument sequence has to come from
# CALL_FUNCTION_EX, i.e. `f(*())`.
#
# Found in CPython's test_asyncio.test_base_events, which calls
# gen.close() on an async generator during loop shutdown.
import sys


def g():
    yield 1


async def coro():
    pass


c = coro()
c.close()

cases = [
    ("generator.close", type(g()).close),
    ("coroutine.close", type(c).close),
    ("BaseException.__reduce__", BaseException.__reduce__),
    ("StopIteration.__reduce__", StopIteration.__reduce__),
    ("classmethod.__get__", classmethod.__get__),
    ("staticmethod.__get__", staticmethod.__get__),
    ("property.__get__", property.__get__),
]
for name, f in cases:
    try:
        f(*())
        print("%-26s NO ERROR" % name)
    except TypeError as e:
        print("%-26s %s" % (name, e))

# OSError.__reduce__ raises the same way, but the TYPE its message names
# differs: CPython's OSError defines __reduce__ of its own, while here one
# body is shared down the exception MRO and func_owner records the first type
# it was stamped on.  So only the shape is compared.
try:
    OSError.__reduce__(*())
    print("OSError.__reduce__          NO ERROR")
except TypeError as e:
    print("OSError.__reduce__          %s" % str(e).endswith("needs an argument"))

# Which of CPython's two wordings you get says which kind of descriptor was
# reached, and that distinction is the same one the repr reports.
print(repr(property.__get__))
print(repr(classmethod.__get__))
print(repr(str.upper))
print(repr(list.append))

# The bound forms still work, and still take their own arguments.
print("bound close:", g().close())
print("bound reduce:", ValueError("x").__reduce__())
print("bound get:", property(lambda s: 7).__get__(object(), object))

# And unbound WITH a receiver still works, which is what the check must not
# have broken.
gen = g()
print("unbound with receiver:", type(gen).close(gen))
print("unbound reduce:", BaseException.__reduce__(ValueError("y")))

# A wrong receiver is still refused, by the check one line further on.
try:
    type(g()).close("not a generator")
except TypeError as e:
    print("wrong receiver:", e)

# A plain module-level builtin takes no receiver and must be unaffected: its
# func_owner is 0, so it never reaches the new check.
print("len of nothing:", end=" ")
try:
    len(*())
except TypeError as e:
    print(e)
print("survived")
