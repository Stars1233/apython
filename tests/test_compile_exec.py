# The source compiler, phase 3: statements and exec().
#
# exec() compiles at module scope, where nothing is function-like -- so every
# name goes through STORE_NAME and LOAD_NAME against the frame's locals
# mapping, which is exactly the dict passed in here.  That is the whole reason
# exec(src, d) can hand results back through d.

d = {}
exec("x = 1", d)
print(d['x'])

exec("a = 2\nb = a * 3\nc = [a, b]", d)
print(d['a'], d['b'], d['c'])

# Chained and multiple-target assignment: the value is computed once.
exec("m = n = 5", d); print(d['m'], d['n'])
exec("p, q = 10, 20", d); print(d['p'], d['q'])
exec("(r, s), t = (1, 2), 3", d); print(d['r'], d['s'], d['t'])
exec("h, *tl = [1,2,3,4]", d); print(d['h'], d['tl'])
exec("*ini, las = [1,2,3]", d); print(d['ini'], d['las'])
exec("f1, *mid, l1 = [1,2,3,4,5]", d); print(d['f1'], d['mid'], d['l1'])

# Augmented assignment, against each target shape.
exec("n2 = 5\nn2 += 3\nn2 *= 2\nn2 //= 2\nn2 -= 1", d); print(d['n2'])
exec("lst = [1,2,3]\nlst[0] += 10", d); print(d['lst'])

# Stores into subscripts and attributes.
exec("o = {}\no['k'] = 7\no['k'] += 1", d); print(d['o'])

# Several statements on one line, and a trailing semicolon.
exec("z = 1; y = 2;", d); print(d['z'], d['y'])

# del, in each of its forms.
exec("del z", d); print('z' in d)
exec("dl = [1,2,3]\ndel dl[1]", d); print(d['dl'])
exec("dd = {'a':1,'b':2}\ndel dd['a']", d); print(d['dd'])

# Imports.
exec("import sys", d); print(type(d['sys']).__name__)
exec("from sys import argv", d); print(type(d['argv']).__name__)
exec("import sys as system", d); print(type(d['system']).__name__)

# assert and raise.
exec("assert 1 == 1", d)
try:
    exec("assert 1 == 2, 'boom'", d)
except AssertionError as e:
    print("AssertionError:", e)
try:
    exec("raise ValueError('v')", d)
except ValueError as e:
    print("ValueError:", e)
try:
    exec("raise TypeError('t') from ValueError('c')", d)
except TypeError as e:
    print("TypeError:", e, "cause:", type(e.__cause__).__name__)

# pass, an annotation, and exec's return value.
exec("pass", d)
exec("ann: int = 3", d); print(d['ann'])
print(exec("q2 = 9", d))

# Expression statements are evaluated and discarded, side effects and all.
exec("acc = []\nacc.append(1)\nacc.append(2)", d); print(d['acc'])

# Comments, blank lines and continuations inside an exec'd source.
exec("""
# leading comment
w1 = 1

w2 = (w1 +
      1)
w3 = w1 + \\
     2
""", d)
print(d['w1'], d['w2'], d['w3'])

# With globals given but no locals, the two are the same mapping.
g = {'seed': 4}
exec("out = seed * 2", g)
print(g['out'])

# Syntax errors are reported rather than crashed on.
for bad in ["x =", "= 1", "del", "import", "1 +", "x ==== 2", "  indented"]:
    try:
        exec(bad, {})
        print("no error for", repr(bad))
    except SyntaxError:
        print("SyntaxError for", repr(bad))
    except IndentationError:
        print("IndentationError for", repr(bad))
