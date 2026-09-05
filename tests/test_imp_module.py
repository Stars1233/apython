# _imp, and the source_hash it has to agree with CPython about.
#
# importlib's bootstrap is written against _imp: acquire_lock and
# release_lock around the machinery, is_builtin and create_builtin for the
# modules the interpreter carries, is_frozen and get_frozen_object for the
# ones it has as marshalled code, create_dynamic for extension modules, and
# source_hash for a hash-based .pyc.  None of it needs to be C here.
#
# source_hash is the one that has to be a number rather than a behaviour: it
# goes into a .pyc that another interpreter reads, so it must be CPython's
# siphash13 exactly -- whose round is written differently from the published
# algorithm and whose result folds as (v0 ^ v1) ^ (v2 ^ v3).
import _imp
import sys

print("-- the lock")
print(_imp.lock_held())
_imp.acquire_lock()
print(_imp.lock_held())
_imp.acquire_lock()
_imp.release_lock()
print(_imp.lock_held())
_imp.release_lock()
print(_imp.lock_held())
try:
    _imp.release_lock()
    print("released an unheld lock")
except RuntimeError as e:
    print("RuntimeError:", e)

print()
print("-- builtins")
print("sys:", _imp.is_builtin("sys"))
print("builtins:", _imp.is_builtin("builtins"))
print("__main__:", _imp.is_builtin("__main__"))
print("nosuchmodule:", _imp.is_builtin("nosuchmodule"))
print("every name in sys.builtin_module_names is one:",
      all(_imp.is_builtin(n) != 0 for n in sys.builtin_module_names))

print()
print("-- frozen and dynamic: neither exists here")
print(_imp.is_frozen("importlib"), _imp.find_frozen("importlib"))
# CPython lists its .so suffixes; there is no dynamic loader here, so the
# list being EMPTY is the answer, and its contents are not comparable.
print("extension suffixes:", type(_imp.extension_suffixes()).__name__)
try:
    _imp.get_frozen_object("x")
except ImportError as e:
    print("ImportError:", e)

print()
print("-- source_hash, which has to be CPython's number")
for key in (0, 1, 0x1234, 0xDEADBEEF):
    for data in (b"", b"a", b"hello world", b"z" * 7, b"y" * 63, b"x" * 64):
        print("%08x %2d %s" % (key, len(data), _imp.source_hash(key, data).hex()))
print("check_hash_based_pycs:", _imp.check_hash_based_pycs)
