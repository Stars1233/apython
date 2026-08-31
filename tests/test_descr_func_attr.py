# classmethod.__func__ and staticmethod.__func__ -- the wrapped function.
#
# collections.namedtuple reaches for it: after building _make as a classmethod
# it does `_make.__func__.__doc__ = ...`, which is the only way to reach the
# function through the wrapper.  Neither type had a tp_getattr, so the
# attribute did not exist and namedtuple raised.
class C:
    @classmethod
    def cm(cls, a):
        return ("cm", cls.__name__, a)

    @staticmethod
    def sm(a):
        return ("sm", a)


cm = C.__dict__["cm"]
sm = C.__dict__["sm"]
print(type(cm).__name__, type(sm).__name__)
print(cm.__func__.__name__, sm.__func__.__name__)
print(cm.__func__(C, 1), sm.__func__(2))

# Setting an attribute through __func__ reaches the function itself.
cm.__func__.__doc__ = "a docstring"
print(C.cm.__doc__)

# The wrappers still work as descriptors.
print(C.cm(3), C.sm(4), C().cm(5), C().sm(6))
