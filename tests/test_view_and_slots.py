"""The names a slot has but a tp_dict entry did not: views, re, and three
dunders that only the protocol could reach."""


def main():
    import re

    d = {1: "a", 2: "b"}

    # .mapping is on all three views; .isdisjoint only on the set-like two.
    for view in (d.keys(), d.values(), d.items()):
        name = type(view).__name__
        print(name, "mapping:", view.mapping, type(view.mapping).__name__)
        print(name, "isdisjoint:", hasattr(view, "isdisjoint"))

    print("keys disjoint:", d.keys().isdisjoint({3, 4}),
          d.keys().isdisjoint({2}))
    print("keys empty:", d.keys().isdisjoint([]),
          d.keys().isdisjoint(iter([2])))
    print("items:", d.items().isdisjoint({(1, "a")}),
          d.items().isdisjoint({(1, "z")}))
    print("self:", d.keys().isdisjoint(d.keys()))
    print("empty self:", {}.keys().isdisjoint({}.keys()))

    try:
        d.keys().isdisjoint([[1]])
    except TypeError as e:
        print("unhashable:", e)
    try:
        d.keys().isdisjoint(5)
    except TypeError as e:
        print("not iterable:", e)

    # The proxy is read-only and tracks the dict it came from.
    proxy = d.keys().mapping
    try:
        proxy[3] = 1
    except TypeError as e:
        print("proxy readonly:", type(e).__name__)
    d[3] = "c"
    print("proxy live:", sorted(proxy), len(proxy))
    del d[3]

    # PEP 585 on the regex types.
    print("re.Match:", re.Match[str])
    print("re.Pattern:", re.Pattern[bytes])
    print("alias origin:", re.Match[str].__origin__ is re.Match)

    # Three slots that had no name.
    print("bytes.__bytes__:", bytes.__bytes__(b"ab"))
    print("range.__bool__:", range.__bool__(range(0)),
          range.__bool__(range(3)))
    print("None.__bool__:", type(None).__bool__(None))

    class B(bytes):
        pass

    print("subclass __bytes__:", type(B(b"xy").__bytes__()).__name__,
          B(b"xy").__bytes__())

    for call, arg in ((bytes.__bytes__, 1), (range.__bool__, 1),
                      (type(None).__bool__, 1)):
        try:
            call(arg)
        except TypeError as e:
            print("wrong self:", type(e).__name__)

    # And they are all in dir(), which is how the stdlib finds them.
    print("in dir:", "__bytes__" in dir(bytes), "__bool__" in dir(range),
          "__bool__" in dir(type(None)),
          "isdisjoint" in dir(d.keys()), "mapping" in dir(d.values()))


main()
