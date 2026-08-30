# The source compiler, phase 6: try / except / else / finally, and with.
#
# This is where the exception table gets built.  Every instruction carries the
# innermost handler covering it, and the ranges are recovered afterwards by
# run-length encoding that stamp -- which is what makes them come out disjoint
# and sorted, and that matters: exc_table_find_handler scans linearly and takes
# the FIRST entry containing an offset, so an overlap would silently select the
# wrong handler.

class CM:
    def __init__(self, log, name, suppress=False):
        self.log, self.name, self.suppress = log, name, suppress
    def __enter__(self):
        self.log.append('enter ' + self.name)
        return self.name
    def __exit__(self, *a):
        self.log.append('exit ' + self.name)
        return self.suppress

def run(src, **kw):
    d = dict(kw)
    exec(src, d)
    return {k: v for k, v in d.items()
            if not k.startswith('__') and k not in kw
            and not callable(v) and not isinstance(v, type)}

# except, in each of its forms.
print(run("r = 'no'\ntry:\n  r = 'ok'\nexcept ValueError:\n  r = 'caught'\n"))
print(run("try:\n  raise ValueError('x')\nexcept ValueError:\n  r = 'caught'\n"))
print(run("try:\n  raise ValueError('x')\nexcept ValueError as e:\n  r = str(e)\n"))
print(run("try:\n  raise TypeError('t')\nexcept ValueError:\n  r = 'v'\nexcept TypeError:\n  r = 't'\n"))
print(run("try:\n  raise KeyError('k')\nexcept:\n  r = 'bare'\n"))
print(run("try:\n  raise ValueError()\nexcept (TypeError, ValueError):\n  r = 'tuple of types'\n"))

# An unmatched exception propagates rather than being swallowed.
print(run("r = 'propagated'\ntry:\n  try:\n    raise KeyError()\n  except ValueError:\n    r = 'wrong'\nexcept KeyError:\n  pass\n"))

# The name bound by `as` does not outlive the clause.
print(run("try:\n  raise ValueError('v')\nexcept ValueError as e:\n  pass\ntry:\n  e\n  r = 'leaked'\nexcept NameError:\n  r = 'unbound'\n"))

# finally runs on both paths, and after a return, break or continue.
print(run("r = []\ntry:\n  r.append(1)\nfinally:\n  r.append(2)\n"))
print(run("r = []\ntry:\n  r.append(1)\n  raise ValueError()\nexcept ValueError:\n  r.append(2)\nfinally:\n  r.append(3)\n"))
print(run("r = []\ntry:\n  r.append(1)\nexcept ValueError:\n  r.append(9)\nelse:\n  r.append(2)\nfinally:\n  r.append(3)\n"))
print(run("def f(log):\n  try:\n    return 'from try'\n  finally:\n    log.append('finally ran')\nlog = []\nr = (f(log), log)\n"))
print(run("def f():\n  log = []\n  for i in range(5):\n    try:\n      if i == 2:\n        break\n    finally:\n      log.append(i)\n  return log\nr = f()\n"))
print(run("def f():\n  log = []\n  for i in range(4):\n    try:\n      if i % 2:\n        continue\n    finally:\n      log.append(i)\n  return log\nr = f()\n"))

# Nested try, and a finally that itself raises out of a handler.
print(run("r = []\ntry:\n  try:\n    raise ValueError()\n  finally:\n    r.append('inner finally')\nexcept ValueError:\n  r.append('outer caught')\n"))

# with, in each of its forms.
print(run("log = []\nwith CM(log,'a'):\n  log.append('body')\nr = log\n", CM=CM))
print(run("log = []\nwith CM(log,'a') as x:\n  log.append('body ' + x)\nr = log\n", CM=CM))
print(run("log = []\nwith CM(log,'a'), CM(log,'b'):\n  log.append('body')\nr = log\n", CM=CM))
print(run("log = []\ntry:\n  with CM(log,'a'):\n    raise ValueError()\nexcept ValueError:\n  log.append('caught')\nr = log\n", CM=CM))
print(run("log = []\nwith CM(log,'a',True):\n  raise ValueError()\nlog.append('suppressed')\nr = log\n", CM=CM))
print(run("log = []\ndef f(log):\n  with CM(log,'a'):\n    return 'returned'\nr = (f(log), log)\n", CM=CM))

# A try statement needs at least one of except or finally.
for bad in ["try:\n  pass\n", "except ValueError:\n  pass\n", "finally:\n  pass\n"]:
    try:
        exec(bad, {})
        print("no error for", repr(bad))
    except SyntaxError:
        print("SyntaxError for", repr(bad))
