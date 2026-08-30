# The pieces the stdlib reaches for that had no implementation here.
import sys
import itertools
import _thread

# sys.builtin_module_names: os.py reads it to pick a platform module.
print(type(sys.builtin_module_names).__name__, "sys" in sys.builtin_module_names)

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
