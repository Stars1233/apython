# The source compiler, phase 7: classes, decorators, comprehensions and
# generators.
#
# All four comprehension forms compile to a nested function taking the
# outermost iterable as its one argument -- the pre-3.12 shape.  CPython 3.12
# inlines the list, set and dict forms, but a generator expression cannot be
# inlined at all, so that machinery would have to exist alongside this rather
# than instead of it.  One mechanism gets the scoping right for free: the loop
# variable does not leak because it lives in another frame.

def run(src, **kw):
    d = dict(kw)
    exec(src, d)
    return {k: repr(v) for k, v in d.items()
            if not k.startswith('__') and k not in kw
            and not callable(v) and not isinstance(v, type)}

# Classes.
print(run("class C:\n  x = 1\nr = C.x\n"))
print(run("class C:\n  def m(self):\n    return 7\nr = C().m()\n"))
print(run("class C:\n  def __init__(self, v):\n    self.v = v\n  def get(self):\n    return self.v\nr = C(5).get()\n"))
print(run("class C:\n  pass\nr = (C.__name__, C().__class__.__name__)\n"))
print(run("class C:\n  x = 1\n  y = x + 1\nr = C.y\n"))

# Inheritance, and zero-argument super() -- which needs the implicit __class__
# cell the class body leaves behind in __classcell__.
print(run("class B:\n  def m(self):\n    return 'base'\nclass D(B):\n  pass\nr = D().m()\n"))
print(run("class B:\n  def m(self):\n    return 'base'\nclass D(B):\n  def m(self):\n    return 'derived+' + super().m()\nr = D().m()\n"))
print(run("class B:\n  def m(self):\n    return 'b'\nclass D(B):\n  def m(self):\n    return 'd+' + super(D, self).m()\nr = D().m()\n"))
print(run("class A:\n  def w(self):\n    return 'a'\nclass B(A):\n  def w(self):\n    return 'b'+super().w()\nclass C(B):\n  def w(self):\n    return 'c'+super().w()\nr = C().w()\n"))

# Decorators, applied bottom to top.
print(run("def deco(f):\n  def w(*a):\n    return ('wrapped', f(*a))\n  return w\n@deco\ndef g(x):\n  return x * 2\nr = g(3)\n"))
print(run("def d1(f):\n  return lambda: 'd1(' + f() + ')'\ndef d2(f):\n  return lambda: 'd2(' + f() + ')'\n@d1\n@d2\ndef h():\n  return 'h'\nr = h()\n"))
print(run("def d(c):\n  c.tag = 'decorated'\n  return c\n@d\nclass C:\n  pass\nr = C.tag\n"))

# Comprehensions, in all four forms.
print(run("r = [x*2 for x in range(5)]\n"))
print(run("r = [x for x in range(10) if x % 3 == 0]\n"))
print(run("r = [x for x in range(20) if x % 2 == 0 if x % 3 == 0]\n"))
print(run("r = [(a,b) for a in range(2) for b in range(2)]\n"))
print(run("r = [y for x in [[1,2],[3]] for y in x]\n"))
print(run("r = sorted({x for x in [1,2,2,3]})\n"))
print(run("r = {x: x*x for x in range(4)}\n"))
print(run("r = list(x+1 for x in range(4))\n"))
print(run("r = sum(x for x in range(5))\n"))

# The loop variable does not leak, and the comprehension can still close over
# what surrounds it.
print(run("x = 'outer'\nr = ([x for x in range(2)], x)\n"))
print(run("n = 10\nr = [x+n for x in range(3)]\n"))
print(run("def f(n):\n  return [x*n for x in range(3)]\nr = f(4)\n"))

# Generators.
print(run("def g():\n  yield 1\n  yield 2\nr = list(g())\n"))
print(run("def g(n):\n  for i in range(n):\n    yield i*i\nr = list(g(4))\n"))
print(run("def g():\n  yield from [1,2]\n  yield from (3,4)\nr = list(g())\n"))
# send() feeds the value back to the yield expression.
print(run("def g():\n  x = yield 1\n  yield x*2\nit = g()\na = next(it)\nb = it.send(5)\ndel it\nr = (a,b)\n"))
print(run("def g():\n  yield 1\n  return\n  yield 2\nr = list(g())\n"))
print(run("def g():\n  try:\n    yield 1\n  finally:\n    pass\nr = list(g())\n"))
print(run("def outer():\n  def inner():\n    yield 1\n  return list(inner())\nr = outer()\n"))
