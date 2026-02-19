# Test dict bug fixes and new features

# Bug 1: dict_repr with payload-0 keys (SmallInt 0, None)
# Note: 0 and False are same key in Python
d = {0: 'false_val', None: 'none'}
print(repr(d))

# Bug 2: hash of unhashable types
for obj in [{}, [], set()]:
    try:
        hash(obj)
        print("BUG: no error")
    except TypeError:
        print("OK")

# Bug 3: DICT_MERGE duplicate keys
def f(**kw):
    return kw
try:
    f(**{'a': 1}, **{'a': 2})
    print("BUG: no error")
except TypeError:
    print("OK")

# Bug 4: mutation during iteration
d = {1: 'a', 2: 'b', 3: 'c'}
try:
    for k in d:
        d[4] = 'd'
    print("BUG: no error")
except RuntimeError:
    print("OK")

# Feature 1: key in d.keys()
d = {1: 'a', 2: 'b'}
print(1 in d.keys())
print(99 in d.keys())

# Feature 2: reversed(d)
d = {1: 'a', 2: 'b', 3: 'c'}
print(list(reversed(d)))
