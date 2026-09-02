# The pieces the stdlib reaches for that had no implementation here.
import sys
import itertools
import _thread

# sys.builtin_module_names: os.py reads it to pick a platform module, gating
# the whole import on `if 'posix' in sys.builtin_module_names`.  It used to be
# a hand-written array separate from the one import_init registers from, and
# the two had drifted: errno and asyncio were in sys.modules and absent here.
# Both are now built from builtin_module_table, so every registered module
# appears -- which is what this checks, rather than a fixed list.
print(type(sys.builtin_module_names).__name__, "sys" in sys.builtin_module_names)
# The list is compared as a set of invariants, not as a fixed roster: CPython
# ships some sixty of these and apython a handful.  What must hold in both is
# that every name apython registers is here, and that the tuple is sorted --
# which is what a table-driven list gives and a hand-written one had lost.
print("named:", all(n in sys.builtin_module_names
                    for n in ("builtins", "sys", "errno", "_sre", "_abc",
                              "_weakref", "time")))
print("sorted:", list(sys.builtin_module_names) == sorted(sys.builtin_module_names))
print("no dups:", len(set(sys.builtin_module_names)) == len(sys.builtin_module_names))
print("all str:", all(type(n).__name__ == "str" for n in sys.builtin_module_names))

# os._createenviron decodes posix.environ with this.
print("fsencoding:", sys.getfilesystemencoding())

# itertools is found relative to the interpreter, not the working directory.
print(list(itertools.islice(itertools.count(3), 4)))
print(list(itertools.repeat("x", 3)))
print(list(itertools.chain([1, 2], [3])))

# __contains__ as a method, not only as the `in` operator.  keyword.py does
# `iskeyword = frozenset(kwlist).__contains__`.
f = frozenset(["a", "b"])
inf = f.__contains__
print(inf("a"), inf("z"))
print({1, 2}.__contains__(1), [1, 2].__contains__(3), (1,).__contains__(1))
print({"k": 1}.__contains__("k"))

# frozenset now has the non-mutating half of set's methods.
print(sorted(f.union({"c"})), len(f), f.copy() == f)

# _thread: one thread, uncontended locks.
print(_thread.get_ident() != 0)
lock = _thread.allocate_lock()
with lock:
    print(lock.locked())
print(lock.locked())
r = _thread.RLock()
with r:
    with r:
        print(r._is_owned())
print(r._is_owned())
