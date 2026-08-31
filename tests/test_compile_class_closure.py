# A class body is a nested block and needs a closure like any other.
#
# A method's free variable resolves PAST the class scope to the enclosing
# function -- class scopes are invisible to the functions defined in them --
# and the class body carries the variable through in its own co_freevars so it
# can hand it on.  Two things were missing:
#
#   * the symbol table only let a free variable travel through a block that was
#     function-like, and a class body is not, so a method that referenced the
#     class it was being defined in had nowhere to put the name;
#
#   * cg_class_value never built a closure tuple, so the class body's
#     COPY_FREE_VARS had nothing to copy.
#
# The second was hidden by a third bug: cg_class_value restored the enclosing
# scope through eax on its way out, overwriting its own return value with a
# scope index that is never zero -- so every failure was reported as a success
# and the caller emitted a store for a class it had not built.
SRC = '''
def factory():
    class M:
        def whoami(self):
            return M

        def sibling(self):
            return M()

    return M().whoami() is M, isinstance(M().sibling(), M)


print(factory())


def mutating():
    class Node:
        def __init__(self, v):
            self.v = v

        def __lt__(self, other):
            seen.append(Node(-1))
            return self.v < other.v

    seen = []
    items = [Node(i) for i in (2, 1, 3)]
    items.sort()
    return [n.v for n in items], len(seen) > 0


print(mutating())


def two_levels():
    tag = "outer"

    class Outer:
        class Inner:
            pass

        def get(self):
            return tag, Outer, Outer.Inner

    return Outer().get()[0], Outer().get()[1] is Outer


print(two_levels())


# A class at module level, whose methods reach a module global, still works.
value = 7


class Plain:
    def read(self):
        return value

    def make(self):
        return Plain()


print(Plain().read(), isinstance(Plain().make(), Plain))
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)
