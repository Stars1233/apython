# A module whose PEP 562 hooks delete themselves while they are running, for
# test_module_getattr_del.py.
#
# Kept beside the test rather than inside it for the same reason
# pep562helper.py is: PEP 562 is about a MODULE's own namespace, and a dict
# exec()'d into is not a module.
#
# `del globals()["__getattr__"]` inside __getattr__ drops the last reference
# to the function that is currently executing.  The lookup that found it in
# the module dict must therefore be holding one of its own, or the callable is
# freed underneath its own frame.  CPython's own
# Lib/test/test_module/bad_getattr3.py is this file's ancestor; its comment
# reads "these lookups should not crash".

calls = []


def __getattr__(name):
    calls.append(name)
    if name != "delgetattr":
        raise AttributeError(name)
    # The hook removes ITSELF from the module namespace, then raises.  Nothing
    # else references the function at this point except the interpreter's own
    # record of what it is calling.
    del globals()["__getattr__"]
    raise AttributeError(name)


def __dir__():
    # The same shape one door over: __dir__ is looked up in the module dict
    # and called, so it can unbind itself too.
    del globals()["__dir__"]
    return ["gone"]
