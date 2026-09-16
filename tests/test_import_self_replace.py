"""A module that replaces itself in sys.modules, and what `import` then binds.

`import x` must bind whatever sys.modules['x'] holds when the body FINISHES,
not the module object the import system put there before running it.
CPython's _bootstrap._load re-reads it for exactly this reason:

    module = sys.modules.pop(spec.name)
    sys.modules[spec.name] = module
    return module

Returning the created object instead is not cosmetic.  The replacement idiom
is how an alias module names something else -- it is what lib/_frozen_importlib
does to make `_frozen_importlib` and `importlib._bootstrap` one module reached
two ways -- and it is how a lazy-loading module hands back a proxy.  Worse, the
object created first has already been released by the dict_set that overwrote
it, so returning it is a read of freed memory.

Everything here is built at runtime under a directory put on sys.path, so the
test needs no fixture files.
"""

import os
import sys

DIR = '/tmp/apython-import-self-replace-%d' % os.getpid()


def write(name, text):
    with open(os.path.join(DIR, name + '.py'), 'w') as handle:
        handle.write(text)


def setup():
    os.mkdir(DIR)
    sys.path.insert(0, DIR)

    # Replaced by something that is not a module at all.
    write('selfrep_class', 'import sys\n'
                           'class Stand_In:\n'
                           '    value = 42\n'
                           'sys.modules[__name__] = Stand_In\n')

    # Replaced by another module -- the alias shape.
    write('selfrep_target', 'MARK = "target"\n')
    write('selfrep_alias', 'import sys\n'
                           'import selfrep_target\n'
                           'sys.modules[__name__] = selfrep_target\n')


def teardown():
    sys.path.remove(DIR)
    # Both interpreters write a __pycache__ beside the sources they compile.
    for root, dirs, files in os.walk(DIR, topdown=False):
        for name in files:
            os.unlink(os.path.join(root, name))
        for name in dirs:
            os.rmdir(os.path.join(root, name))
    os.rmdir(DIR)


def test_replaced_by_a_class():
    import selfrep_class
    assert selfrep_class.value == 42, selfrep_class
    assert not isinstance(selfrep_class, type(sys)), type(selfrep_class)
    assert sys.modules['selfrep_class'] is selfrep_class


def test_replaced_by_another_module():
    import selfrep_alias
    import selfrep_target
    assert selfrep_alias is selfrep_target
    assert selfrep_alias.MARK == 'target'
    assert sys.modules['selfrep_alias'] is selfrep_target


def test_from_import_sees_the_replacement():
    from selfrep_class import value
    assert value == 42


setup()
try:
    for fn in (test_replaced_by_a_class,
               test_replaced_by_another_module,
               test_from_import_sees_the_replacement):
        fn()
        print(fn.__name__, 'ok')
finally:
    teardown()
print('OK')
