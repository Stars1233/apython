# The source compiler, phase 4: if / elif / else, while, for, break, continue.
#
# The long-body cases at the end are not padding: they push a jump past 255
# code units, which is the only way to exercise the EXTENDED_ARG fixpoint in
# the assembler.  A jump's oparg depends on the offsets, the offsets depend on
# the sizes, and the sizes depend on the opargs; the loop that settles that has
# to be run against something that actually needs it.

def run(src, **kw):
    d = dict(kw)
    exec(src, d)
    return {k: v for k, v in d.items() if not k.startswith('__') and k not in kw}

print(run("if 1:\n  r = 'yes'\nelse:\n  r = 'no'\n"))
print(run("if 0:\n  r = 'yes'\nelse:\n  r = 'no'\n"))
print(run("if 1: r = 'oneline'\n"))
print(run("if 0:\n  r = 1\n"))

for x in (5, 2, 0):
    print(x, run("if x > 3:\n  r = 'big'\nelif x > 1:\n  r = 'mid'\nelse:\n  r = 'small'\n", x=x))

print(run("r = 0\nfor i in [1,2,3]:\n  r += i\n"))
print(run("r = []\nfor i in range(5):\n  if i % 2:\n    continue\n  r.append(i)\n"))
print(run("r = 0\nn = 5\nwhile n:\n  r += n\n  n -= 1\n"))
print(run("r = []\nfor i in range(10):\n  if i == 3:\n    break\n  r.append(i)\n"))
print(run("r = []\nfor a, b in [(1,2),(3,4)]:\n  r.append(a+b)\n"))
print(run("r = []\nfor i in 'abc':\n  r.append(i)\n"))
print(run("r = []\nfor k in {'a':1,'b':2}:\n  r.append(k)\n"))

# The else clause runs when the loop ends on its own, not when it is broken.
print(run("r = 'no'\nfor i in [1]:\n  pass\nelse:\n  r = 'else ran'\n"))
print(run("r = 'else ran'\nfor i in [1]:\n  break\nelse:\n  r = 'no'\n"))
print(run("r = 'no'\nn = 0\nwhile n < 1:\n  n += 1\nelse:\n  r = 'else ran'\n"))
print(run("r = 'else ran'\nwhile 1:\n  break\nelse:\n  r = 'no'\n"))

# break and continue bind to the innermost loop.
print(run("r = []\nfor i in range(3):\n  for j in range(3):\n    if j == 1:\n      break\n    r.append((i,j))\n"))
print(run("r = []\nfor i in range(3):\n  for j in range(3):\n    if j == 1:\n      continue\n    r.append((i,j))\n"))

# Deep nesting, and a loop whose body reassigns its own target.
print(run("r = 0\nfor i in range(3):\n  for j in range(3):\n    for k in range(3):\n      r += 1\n"))
print(run("r = []\nfor i in range(3):\n  i = i * 10\n  r.append(i)\n"))

# A jump long enough to need EXTENDED_ARG, in each construct.
BIG = "\n".join("  r += %d" % i for i in range(300))
print(run("r = 0\nif 1:\n" + BIG + "\n")['r'])
print(run("r = 0\nif 0:\n" + BIG + "\nelse:\n  r = -1\n")['r'])
print(run("r = 0\nfor i in range(2):\n" + BIG + "\n")['r'])
print(run("r = 0\nn = 2\nwhile n:\n  n -= 1\n" + BIG + "\n")['r'])

# break and continue outside a loop are syntax errors, not silent jumps.
for bad in ["break", "continue", "if 1:\n  break\n"]:
    try:
        exec(bad, {})
        print("no error for", repr(bad))
    except SyntaxError:
        print("SyntaxError for", repr(bad))
