# The source compiler, phase 5: functions, lambdas, and closures.
#
# This is where the symbol table starts mattering.  Nothing in the syntax says
# whether `x` is a fast local, a cell, a free variable or a global -- `x` in a
# function is a local if the function assigns it ANYWHERE, including after the
# use -- so every name goes through one classification pass and then one
# emitter that reads its answer.

def run(src, **kw):
    d = dict(kw)
    exec(src, d)
    return {k: v for k, v in d.items()
            if not k.startswith('__') and k not in kw and not callable(v)}

# Parameters, in every shape.
print(run("def f():\n  return 1\nr = f()\n"))
print(run("def f(a, b):\n  return a + b\nr = f(2, 3)\n"))
print(run("def f(a, b=10):\n  return a * b\nr = (f(2), f(2, 3))\n"))
print(run("def f(*a):\n  return a\nr = (f(), f(1,2))\n"))
print(run("def f(**k):\n  return sorted(k.items())\nr = f(x=1, y=2)\n"))
print(run("def f(a, *rest, k=9, **kw):\n  return (a, rest, k, sorted(kw))\nr = f(1,2,3,k=4,z=5)\n"))
print(run("def f(a, b, /, c, *, d):\n  return (a,b,c,d)\nr = f(1,2,3,d=4)\n"))
print(run("def f(a, b=2, c=3):\n  return (a,b,c)\nr = (f(1), f(1,9), f(1,9,9), f(1,c=7))\n"))

# Lambdas.
print(run("f = lambda x: x * 2\nr = f(21)\n"))
print(run("r = (lambda a, b: a + b)(1, 2)\n"))
print(run("r = (lambda a, b=5: a * b)(3)\n"))
print(run("r = (lambda *a: sum(a))(1,2,3)\n"))
print(run("r = sorted([3,1,2], key=lambda x: -x)\n"))

# Return, in all its forms.
print(run("def f():\n  return\nr = f()\n"))
print(run("def f(n):\n  if n:\n    return 'yes'\n  return 'no'\nr = (f(1), f(0))\n"))
print(run("def f():\n  for i in range(10):\n    if i == 3:\n      return i\nr = f()\n"))
print(run("def f():\n  return 1, 2\nr = f()\n"))

# Locals are fast slots and do not escape.
print(run("x = 'outer'\ndef f():\n  x = 'inner'\n  return x\nr = (f(), x)\n"))
print(run("def f():\n  a = 1\n  b = 2\n  return a + b\nr = f()\n"))

# Globals: read implicitly, written only with a declaration.
print(run("g = 5\ndef f():\n  return g\nr = f()\n"))
print(run("g = 5\ndef f():\n  global g\n  g = 7\nf()\nr = g\n"))

# Closures: a local a nested block reads becomes a cell.
print(run("def outer(n):\n  def inner(x):\n    return x + n\n  return inner\nr = outer(10)(5)\n"))
print(run("def f():\n  x = 1\n  def g():\n    return x\n  return g()\nr = f()\n"))
print(run("def mk():\n  t = 0\n  def add(x):\n    nonlocal t\n    t += x\n    return t\n  return add\na = mk()\nr = (a(1), a(2), a(3))\n"))
print(run("def f():\n  a = 1\n  def g():\n    def h():\n      return a\n    return h()\n  return g()\nr = f()\n"))
print(run("def mk(n):\n  return lambda: n\nr = []\nfor i in range(3):\n  r.append(mk(i)())\n"))

# A parameter that is also a cell keeps its parameter slot and is boxed there.
print(run("def outer(a):\n  def inner():\n    return a\n  a = a + 1\n  return inner()\nr = outer(1)\n"))

# Recursion.
print(run("def fact(n):\n  return 1 if n < 2 else n * fact(n-1)\nr = fact(6)\n"))
print(run("def fib(n):\n  if n < 2:\n    return n\n  return fib(n-1) + fib(n-2)\nr = fib(12)\n"))

# The construct this whole compiler was built for: collections.namedtuple
# builds its __new__ with exactly this eval().
arg_list = "x, y"
ns = {'_tuple_new': tuple.__new__, '__builtins__': {}, '__name__': 'namedtuple_P'}
new = eval(f'lambda _cls, {arg_list}: _tuple_new(_cls, ({arg_list}))', ns)
print(new(tuple, 1, 2))

# Errors the symbol table is responsible for catching.
for bad in ["def f():\n  nonlocal q\n", "nonlocal z\n",
            "def f():\n  global q\n  nonlocal q\n"]:
    try:
        exec(bad, {})
        print("no error for", repr(bad))
    except SyntaxError:
        print("SyntaxError for", repr(bad))
