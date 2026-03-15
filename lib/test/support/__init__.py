import sys
import unittest

verbose = False

def cpython_only(func):
    return unittest.skip("CPython only")(func)

class adjust_int_max_str_digits:
    def __init__(self, n):
        self._new = n
    def __enter__(self):
        self._old = sys.get_int_max_str_digits()
        sys.set_int_max_str_digits(self._new)
        return self
    def __exit__(self, *args):
        sys.set_int_max_str_digits(self._old)
        return False

def requires_resource(name):
    return unittest.skip("resource %s not enabled" % name)

def run_in_subinterp(code):
    raise unittest.SkipTest("no subinterp support")

# Sentinel objects for testing comparison behavior
class _ALWAYS_EQ:
    """Object that is equal to everything."""
    def __eq__(self, other):
        return True
    def __ne__(self, other):
        return False
    def __lt__(self, other):
        return False
    def __le__(self, other):
        return True
    def __gt__(self, other):
        return False
    def __ge__(self, other):
        return True
    def __hash__(self):
        return 0

ALWAYS_EQ = _ALWAYS_EQ()

class _NEVER_EQ:
    """Object that is not equal to anything."""
    def __eq__(self, other):
        return False
    def __ne__(self, other):
        return True
    def __lt__(self, other):
        return False
    def __le__(self, other):
        return False
    def __gt__(self, other):
        return False
    def __ge__(self, other):
        return False
    def __hash__(self):
        return 0

NEVER_EQ = _NEVER_EQ()

C_RECURSION_LIMIT = 50

def check_free_after_iterating(test, func, cls):
    """Check that an iterator doesn't hold references after exhaustion."""
    obj = cls([0, 1, 2, 3, 4])
    it = func(obj)
    for _ in it:
        pass
