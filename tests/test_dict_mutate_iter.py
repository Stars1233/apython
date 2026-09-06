# Rebinding an existing key during iteration is not a size change.
#
# `for k in d: d[k] = f(d[k])` is one of the most ordinary things a Python
# program does, and it raised RuntimeError here.  dict_set bumps dk_version on
# every write -- including the update-in-place branch, which changes nothing
# structural -- and the three iterators read dk_version to answer "changed
# size".  Two different questions asked of one field.
#
# CPython's dictiter_iternextkey compares ma_used, the SIZE, and that is what
# the iterators compare now.  dk_version keeps its meaning for the LOAD_GLOBAL
# and LOAD_ATTR inline caches, which guard a dense index with it.


def show(label, fn):
    try:
        print(label, "->", fn())
    except BaseException as e:
        print(label, "->", type(e).__name__ + ":", e)


def over_keys():
    d = {"a": 1, "b": 2, "c": 3}
    for k in d:
        d[k] = d[k] * 10
    return d


def over_items():
    d = {"a": 1, "b": 2}
    for k, v in d.items():
        d[k] = v + 1
    return d


def over_values_then_write():
    d = {"a": 1, "b": 2}
    out = []
    for v in d.values():
        out.append(v)
        d["a"] = 99
    return out, d


def over_reversed():
    d = {"a": 1, "b": 2}
    for k in reversed(d):
        d[k] = -d[k]
    return d


def with_update():
    d = {"a": 1}
    for k in d:
        d.update({"a": 5})
    return d


def with_setdefault():
    d = {"a": 1}
    for k in d:
        d.setdefault("a", 2)
    return d


def del_via_list():
    d = {"a": 1, "b": 2}
    for k in list(d):
        del d[k]
    return d


show("rebind over keys  ", over_keys)
show("rebind over items ", over_items)
show("rebind over values", over_values_then_write)
show("rebind reversed   ", over_reversed)
show("update existing   ", with_update)
show("setdefault exists ", with_setdefault)
show("delete via a copy ", del_via_list)


# --- and the errors that MUST still be raised ---------------------------
def grow():
    d = {"a": 1}
    for k in d:
        d["b"] = 2
    return d


def shrink():
    d = {"a": 1, "b": 2}
    for k in d:
        del d[k]
    return d


def grow_items():
    d = {"a": 1}
    for k, v in d.items():
        d["b"] = 2
    return d


def shrink_values():
    d = {"a": 1, "b": 2}
    for v in d.values():
        d.popitem()
    return d


def grow_reversed():
    d = {"a": 1}
    for k in reversed(d):
        d["b"] = 2
    return d


def clear_during():
    d = {"a": 1, "b": 2}
    for k in d:
        d.clear()
    return d


show("grow              ", grow)
show("shrink            ", shrink)
show("grow over items   ", grow_items)
show("shrink over values", shrink_values)
show("grow reversed     ", grow_reversed)
show("clear             ", clear_during)


# --- a set behaves the same way -----------------------------------------
def set_readd():
    s = {1, 2, 3}
    for x in s:
        s.add(1)
    return sorted(s)


def set_grow():
    s = {1}
    for x in s:
        s.add(2)
    return s


show("set re-add        ", set_readd)
show("set grow          ", set_grow)


# --- a resize inside iteration is still caught --------------------------
def many():
    d = {i: i for i in range(8)}
    n = 0
    for k in d:
        n += 1
        d[k] = k + 1
    return n, len(d), d[3]


show("rebind over eight ", many)
