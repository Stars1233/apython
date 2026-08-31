# sym_visit's generic walk follows a node's a/b/c, and for three pattern kinds
# .b is not a node index at all: a sequence pattern's star position, a class
# pattern's positional count, a mapping pattern's **rest object index.  Each
# was walked as a node, visiting whatever sat at that index in the *node*
# arena and adding bindings from it -- the two arenas overlap freely.
x = 1


def seq(v):
    match v:
        case [*rest]:
            return (x, rest)
    return None


print(seq([1, 2]))
print(seq([]))


def seq2(v):
    match v:
        case [a, *rest, b]:
            return (a, rest, b, x)
    return None


print(seq2([1, 2, 3, 4]))


class P:
    __match_args__ = ("a", "b")

    def __init__(self, a, b):
        self.a = a
        self.b = b


def cls(v):
    match v:
        case P(p, q):
            return (p, q, x)
    return None


print(cls(P(5, 6)))


def cls_kw(v):
    match v:
        case P(a=p, b=q):
            return (p, q, x)
    return None


print(cls_kw(P(7, 8)))


def mapping(v):
    match v:
        case {"k": q, **rest}:
            return (q, rest, x)
    return None


print(mapping({"k": 1, "z": 2}))


def mapping2(v):
    match v:
        case {"k": q}:
            return (q, x)
    return None


print(mapping2({"k": 3, "z": 4}))


# Nested, and combined with as/or patterns.
def nested(v):
    match v:
        case [P(a, b) as whole, {"m": mm, **more}]:
            return (a, b, whole.a, mm, more, x)
    return None


print(nested([P(1, 2), {"m": 3, "n": 4}]))


def alt(v):
    match v:
        case [1, *r] | {"k": r}:
            return (r, x)
    return None


print(alt([1, 2, 3]), alt({"k": 9}))
