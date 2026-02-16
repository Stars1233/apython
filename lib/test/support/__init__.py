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
