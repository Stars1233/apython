# A module that leans on most of the compiler at once, so importing it from
# source is a real test of the pipeline rather than of the file lookup.


class Base:
    kind = "base"

    def describe(self):
        return f"{self.kind}:{self.tag()}"

    def tag(self):
        return "?"


class Child(Base):
    kind = "child"

    def __init__(self, n):
        self.n = n

    def tag(self):
        return str(self.n * 2)


def gen(n):
    for i in range(n):
        if i % 2:
            continue
        yield i


def run():
    out = []
    out.append(Child(3).describe())
    out.append([x * x for x in range(4) if x])
    out.append({k: v for k, v in zip("abc", range(3))})
    out.append(list(gen(6)))
    try:
        raise ValueError("v")
    except ValueError as e:
        out.append("caught " + str(e))
    finally:
        out.append("finally")
    with open("tests/srcpkg/mixed.py") as f:
        out.append(f.readline().startswith("#"))
    out.append(sorted({1, 2, 3} | {3, 4}))
    return out
