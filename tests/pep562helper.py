# A module with PEP 562's __getattr__ and __dir__, for test_module_getattr.py.
#
# Kept beside the test rather than inside it because PEP 562 is about a
# MODULE's own namespace: the hook has to be a global of a real module, and
# exec()ing one into a dict does not make a module.

visible = "found in the dict"

_lazy = {
    "lazy": 42,
    "built": "made on demand",
}

# Every name __getattr__ was asked for, in order, so a test can show that the
# hook runs only on a miss.
asked = []


def __getattr__(name):
    asked.append(name)
    if name in _lazy:
        return _lazy[name]
    if name == "boom":
        raise RuntimeError("deliberate, and not an AttributeError")
    if name == "recurse":
        # Asking the module for another missing name from inside the hook.
        return getattr(__import__(__name__), "lazy")
    raise AttributeError("module %r has no attribute %r" % (__name__, name))


def __dir__():
    return ["from", "__dir__", "only"]
