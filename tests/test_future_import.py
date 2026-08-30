# `from __future__ import ...`
#
# A future statement is settled by the compiler in CPython; the module exists so
# the import succeeds and so the feature objects can be inspected.  Every
# feature is mandatory in 3.12 except `annotations`, which apython is always in
# effect for: it does not evaluate annotations at all.
from __future__ import annotations
from __future__ import division, print_function
import __future__


def f(x: int, y: "Undefined") -> str:
    return str(x) + str(y)


print(f(1, 2))
print(__future__.annotations.compiler_flag == 0x1000000)
print("annotations" in __future__.all_feature_names)
print(sorted(__future__.all_feature_names)[:3])
print(type(__future__.division).__name__)
