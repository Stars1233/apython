# An unbound method descriptor has to be handed one of its own type's
# instances.
#
# Nothing checked.  `list.append((1, 2), 9)` read a tuple's header as a list's
# and tried to grow it -- "Fatal: out of memory", from a two-element tuple --
# and `set.__contains__(frozenset({1}), 1)` quietly answered True.  Every
# builtin method funnels through builtin_func_call, so one check covers all of
# them; func_owner was already recorded there for the repr.
#
# CPython has two wordings, and which one you get says what kind of descriptor
# you reached: a method descriptor "doesn't apply to", a slot wrapper
# "requires ... but received".


def t(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


# --- method descriptors given the wrong receiver
t("list.append tuple", lambda: list.append((1, 2), 9))
t("list.append str", lambda: list.append("ab", 9))
t("list.append int", lambda: list.append(5, 9))
t("list.append None", lambda: list.append(None, 9))
t("list.sort tuple", lambda: list.sort((1, 2)))
t("list.extend dict", lambda: list.extend({}, [1]))
t("dict.keys list", lambda: dict.keys([]))
t("dict.items str", lambda: dict.items("ab"))
t("str.upper int", lambda: str.upper(5))
t("str.split list", lambda: str.split([], ","))
t("bytes.hex str", lambda: bytes.hex("ab"))
t("set.add frozen", lambda: set.add(frozenset({1}), 2))
t("set.__contains__ frozen", lambda: set.__contains__(frozenset({1}), 1))
t("frozenset.__contains__ set", lambda: frozenset.__contains__({1}, 1))
t("tuple.count list", lambda: tuple.count([1], 1))

# --- slot wrappers given the wrong receiver
t("int.__neg__ float", lambda: int.__neg__(2.5))
t("int.__neg__ str", lambda: int.__neg__("x"))
t("str.__len__ int", lambda: str.__len__(5))
t("dict.__len__ list", lambda: dict.__len__([]))
t("list.__iter__ tuple", lambda: list.__iter__(()))
t("int.__hash__ str", lambda: int.__hash__("a"))

# --- the receiver is checked BEFORE the arity, as CPython checks it
t("wrong type and arity", lambda: int.__neg__(2.5, 1))

# --- the right receiver still works, subclasses included
t("list.append list", lambda: (lambda x: (list.append(x, 9), x))([1]))
t("str.upper str", lambda: str.upper("ab"))
t("int.__neg__ int", lambda: int.__neg__(5))
t("int.__neg__ bool", lambda: int.__neg__(True))
t("dict.keys dict", lambda: sorted(dict.keys({1: 2})))


class L(list):
    pass


class S(str):
    pass


class D(dict):
    pass


t("subclass of list", lambda: (lambda x: (list.append(x, 9), list(x)))(L([1])))
t("subclass of str", lambda: str.upper(S("ab")))
t("subclass of dict", lambda: sorted(dict.keys(D({1: 2}))))
t("bound still works", lambda: (lambda x: (x.append(9), x))([1]))

# --- and the class/static methods, whose first argument IS a class
t("dict.fromkeys", lambda: sorted(dict.fromkeys([1, 2])))
t("int.__new__", lambda: int.__new__(int))
t("str.__new__", lambda: str.__new__(str))
t("float.__new__", lambda: float.__new__(float))

# --- arity, which now reports both counts
t("append 2 args", lambda: list.append([], 1, 2))
t("append 0 args", lambda: list.append([]))
t("count 0 args", lambda: list.count([]))
t("bound append 2", lambda: [].append(1, 2))
t("wrapper extra", lambda: list.__len__([], 1))
t("unary extra", lambda: int.__neg__(2, 1))
t("hash extra", lambda: int.__hash__(1, 2))
t("iter extra", lambda: list.__iter__([], 1))
t("len extra", lambda: str.__len__("a", 1))
t("binary missing", lambda: int.__add__(1))
t("eq missing", lambda: dict.__eq__({}))

print("done")
