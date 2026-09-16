"""The interpreter's lazy module-attribute caches must own what they cache.

Three places look a helper up once and keep it for the life of the process,
because builtins are built long before the import system can run:

    object_reduce_impl   caches _reduce.object_reduce_ex   (copy, pickle)
    co_ast_builder       caches _ast._from_raw             (compile(..., PyCF_ONLY_AST))
    builtin_open_fn      caches _io.open                   (open)

Each one fetched its function BORROWED from the module's dict and relied on
sys.modules to keep the module alive.  That is not a promise anyone keeps:
test.support.import_fresh_module drops entries from sys.modules as a matter of
course, and the next call then ran freed memory -- a SIGSEGV inside
obj_call_n for the first of the three, reached from copy.copy().

Each test below drops the module, drops the last reference, churns the heap so
the freed dict's memory is handed back out, and then uses the feature again.
The cached callable has to still be the right object.
"""

import gc
import sys


def churn():
    """Hand the allocator enough traffic to reuse a just-freed block."""
    junk = [bytes(200) for _ in range(20000)]
    junk.extend([x] for x in range(2000))
    return len(junk)


def drop(name):
    """Forget a module as completely as import_fresh_module does."""
    sys.modules.pop(name, None)
    gc.collect()


def test_reduce_cache_survives_module_drop():
    import copy

    class C:
        def __init__(self):
            self.x = 1

    first = copy.copy(C())          # warms omr_cached
    assert first.x == 1

    drop('_reduce')                 # CPython has no such module; dropping is a no-op there
    churn()

    second = copy.copy(C())         # the cached object must still be callable
    assert second.x == 1
    assert type(second) is C

    third = copy.deepcopy(C())
    assert third.x == 1


def test_ast_cache_survives_module_drop():
    import ast

    tree = compile('1 + 2', '<s>', 'eval', ast.PyCF_ONLY_AST)
    assert isinstance(tree, ast.Expression)

    drop('_ast')
    churn()

    again = compile('3 * 4', '<s>', 'eval', ast.PyCF_ONLY_AST)
    assert isinstance(again, ast.Expression)
    assert isinstance(again.body, ast.BinOp)


def test_open_cache_survives_module_drop():
    import os

    path = '/tmp/apython-cache-lifetime-%d' % os.getpid()
    try:
        with open(path, 'wb') as f:                  # warms builtin_open_impl
            f.write(b'cache lifetime')
        with open(path, 'rb') as f:
            head = f.read()
        assert head == b'cache lifetime'

        drop('_io')
        drop('io')
        churn()

        with open(path, 'rb') as f:
            again = f.read()
        assert again == head
    finally:
        os.unlink(path)


def run(fn):
    fn()
    print(fn.__name__, 'ok')


run(test_reduce_cache_survives_module_drop)
run(test_ast_cache_survives_module_drop)
run(test_open_cache_survives_module_drop)
print('OK')
