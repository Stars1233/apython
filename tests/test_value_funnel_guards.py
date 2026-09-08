# dunder_lookup, dunder_call_1 and dict_get all answer with a VALUE now, and
# the guards that used to unpack them into a (payload, tag) pair were swept to
# `test rax, rax`.  Where the sweep stopped at the guard, the instruction below
# it still read rdx -- a register nothing had written since the call.  This is
# the "removed load whose guard stayed" pattern, and it produces wrong ANSWERS,
# not crashes: an int comes back 2^50 out, or an INCREF is skipped and a
# borrowed reference goes on the stack.
#
# Five sites had it.  Three are below, each the smallest program that reaches
# one.  The fourth wants a PEP 695 generic class and lives in
# test_load_from_dict_or_globals.py, which our own compiler cannot build.  The
# fifth is iobase_exit_fn, which hands the corrupted word to a DECREF_V for a
# value it is discarding anyway -- nothing observable, so nothing to assert; it
# is fixed in src/modules/io.asm all the same.


def a_builtin_borrowed_as_next():
    """slot_tp_iternext packed a Value that was already one."""
    class A(list):
        __next__ = list.pop

    class B(int):
        __next__ = int.bit_length

    class C(set):
        __next__ = set.pop

    return next(A([1, 2, 3])), next(B(5)), next(C({7}))


def round_through_a_builtin_dunder():
    """The same missed pack in builtin_round_fn's __round__ arm."""
    class M(list):
        __round__ = list.__len__

    class N:
        def __round__(self, *a):
            return 11

    return round(M([1, 2, 3])), round(N())


def next_through_a_python_dunder():
    """builtin_next_fn shared one exit between its tp_iternext arm, where the
    tag is real, and its __next__ arm, where it is not."""
    class It:
        def __init__(self):
            self.n = 0

        def __iter__(self):
            return self

        def __next__(self):
            self.n += 1
            if self.n > 3:
                raise StopIteration
            return self.n * 1000

    return list(It()), next(It()), next(It(), "d")


print(a_builtin_borrowed_as_next())
print(round_through_a_builtin_dunder())
print(next_through_a_python_dunder())
