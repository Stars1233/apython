# ast.parse's last two arguments: mode="func_type" and type_comments=True.
#
# Both were absent.  func_type was a ValueError, because there was no
# `(int, str) -> bool` start symbol; type_comments collected nothing, because
# the tokenizer discards comments and had nowhere to put a `# type:` one.
#
# The comment is a token now, and only when PyCF_TYPE_COMMENTS asks for it --
# which is why every other parse in this tree is unchanged.  It rides in a
# side table beside the type parameters, for the same reason: AstNode is
# thirty-two bytes and has no field left.
import ast


def dump(src, **kw):
    try:
        return ast.dump(ast.parse(src, **kw))
    except Exception as e:
        return "%s: %s" % (type(e).__name__, e)


print("-- mode='func_type'")
for src in ("(int, str) -> bool", "() -> None", "(...) -> int", "(int) -> int",
            "(int, str, bytes) -> None"):
    print("%-28s %s" % (src, dump(src, mode="func_type")))
print("bad:", dump("int -> bool", mode="func_type"))
print("bad:", dump("(int, str)", mode="func_type"))

print()
print("-- type_comments=True")
SRC = '''x = 1  # type: int
a, b = 1, 2  # type: (int, int)
for i in y:  # type: int
    pass
with open(f) as g:  # type: IO
    pass
def one(a, b):  # type: (int, str) -> bool
    return True
def two(a, b):
    # type: (int, str) -> bool
    return True
def three(a,  # type: int
          b,  # type: str
          ):
    pass
async def four(a):  # type: (int) -> None
    pass
async for j in z:  # type: int
    pass
'''

tree = ast.parse("async def _wrap():\n" + "".join("    " + L + "\n"
                 for L in SRC.splitlines()), type_comments=True)
found = []
for n in ast.walk(tree):
    tc = getattr(n, "type_comment", None)
    if tc is not None:
        found.append((type(n).__name__, tc))
for row in sorted(found):
    print("%-16s %r" % row)

print()
print("-- the flag is what asks for them")
plain = ast.parse("async def _wrap():\n" + "".join("    " + L + "\n"
                  for L in SRC.splitlines()))
print("without:", [type(n).__name__ for n in ast.walk(plain)
                   if getattr(n, "type_comment", None) is not None])

print()
print("-- # type: ignore is the module's, not a statement's")
IG = '''x = 1  # type: ignore
y = 2  # type: ignore[misc]
z = 3  # type: ignoreme
w = 4  # type: int
'''
t = ast.parse(IG, type_comments=True)
print("ignores:", [(i.lineno, i.tag) for i in t.type_ignores])
print("comments:", [(type(n).__name__, n.type_comment) for n in ast.walk(t)
                    if getattr(n, "type_comment", None) is not None])
print("without the flag:", ast.parse(IG).type_ignores)

print()
print("-- a comment that is not a type comment is still a comment")
print(dump("x = 1  # ordinary\n", type_comments=True))
print(dump("x = 1  # typeof: int\n", type_comments=True))
