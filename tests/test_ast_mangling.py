# What the AST says about a private name.
#
# CPython's tree keeps `__x` and mangles in the compiler, where the enclosing
# class is still in hand.  This parser mangles as it interns -- it has to, the
# class is only known while the parser is inside its body -- so the tree read
# back `_C__x` for every identifier in a class body: attributes, parameters,
# names, global declarations, import aliases, except-as, match captures.
# Seven files in this repository's own corpus differed from CPython's tree for
# that reason and no other.
#
# The mangled name is still what the compiler uses; the source name is
# recorded beside it, and that is the one the AST reports.

import ast

SRC = '''
class C:
    __slot = 1
    def __init__(self, __arg, *, __kw=2):
        self.__kwseen = __kw
        self.__x = __arg
        self.__w = 3
        self.__v = 4
        del self.__v
        __local = self.__x
        return __local

    def uses(self):
        global __g
        __g = 1
        for __i in self.__items:
            print(__i, self.__x[0], self.__x.__y)
        with open("f") as __fh:
            pass
        try:
            pass
        except ValueError as __e:
            print(__e)
        [__c for __c in self.__x]
        lambda __p: __p
        f(__kwarg=1)
        match self.__x:
            case {"k": __cap}:
                print(__cap)

    def imports(self):
        import __mod
        import __mod.__sub as __alias
        from __pkg import __thing as __other

    class __Inner:
        def m(self):
            return self.__deep


def outside(__notmangled):
    x = __notmangled
    return x.__attr


class D:
    __dunder__ = 1          # a trailing __ is not mangled
    _single = 2             # nor is one leading underscore
    __ = 3                  # nor is a name that is all underscores
    def m(self):
        return self.__dunder__, self._single, self.__


class ___:                  # a class whose name is all underscores
    def m(self):
        return self.__x
'''

print("=== every identifier, as written ===")
tree = ast.parse(SRC)
seen = set()
for node in ast.walk(tree):
    if isinstance(node, ast.Name):
        seen.add(("name", node.id))
    elif isinstance(node, ast.Attribute):
        seen.add(("attr", node.attr))
    elif isinstance(node, ast.arg):
        seen.add(("arg", node.arg))
    elif isinstance(node, ast.alias):
        seen.add(("alias", node.name, node.asname))
    elif isinstance(node, ast.Global):
        seen.add(("global", tuple(node.names)))
    elif isinstance(node, (ast.FunctionDef, ast.ClassDef)):
        seen.add(("def", node.name))
    elif isinstance(node, ast.ExceptHandler):
        seen.add(("except", node.name))
    elif isinstance(node, ast.keyword):
        seen.add(("keyword", node.arg))
    elif isinstance(node, ast.MatchMapping):
        seen.add(("match", tuple(node.patterns[0].name for _ in [0])))
for item in sorted(seen, key=repr):
    print(item)

print("=== and the round trip says the same ===")
print(ast.unparse(tree))

print("=== the compiler still mangles, which is what makes it work ===")
ns = {}
exec(compile(SRC, "<mangling>", "exec"), ns)
C = ns["C"]
obj = C.__new__(C)
C.__init__(obj, 7)
print(obj._C__x, obj._C__w, C._C__slot)
print(sorted(n for n in dir(obj) if "__" in n and not n.startswith("__")))
D = ns["D"]
print(D.m(D()))
print("done")
