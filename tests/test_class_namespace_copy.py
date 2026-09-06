# A class's tp_dict is a COPY of the namespace it was built from, never the
# namespace object itself.  type_from_parts used to adopt the caller's dict, so
# a retained namespace and the live class were one object and writing to the
# first edited the second.
#
# This also underwrites any future attribute cache keyed on a type version:
# a write that reaches the class dict without going through type_setattr has
# no hook to invalidate from, and lib/enum.py writes into a class body
# namespace after the class exists.

# --- type(name, bases, ns) -------------------------------------------------
ns = {'x': 1}
C = type('C', (), ns)

print(C.x)
print(C.__dict__ is ns)

ns['y'] = 2
print(hasattr(C, 'y'))
print('y' in C.__dict__)

# Deleting from the namespace must not remove a class attribute either.
del ns['x']
print(C.x)

# ...and the class is still writable in the ordinary way.
C.z = 3
print(C.z)
print('z' in ns)

# --- the class body form ---------------------------------------------------
class D:
    a = 1

d = D.__dict__
print(D.a)
print(type(d).__name__)

# --- a metaclass sees its own namespace, and it is still not the class's ----
captured = {}


class Meta(type):
    def __new__(mcls, name, bases, ns):
        captured['ns'] = ns
        return super().__new__(mcls, name, bases, ns)


class E(metaclass=Meta):
    b = 1

print(E.b)
print(E.__dict__ is captured['ns'])
captured['ns']['c'] = 9
print(hasattr(E, 'c'))

# --- inheritance is unaffected ---------------------------------------------
base_ns = {'v': 10}
Base = type('Base', (), base_ns)
Derived = type('Derived', (Base,), {})
print(Derived.v)
base_ns['w'] = 11
print(hasattr(Derived, 'w'))
Base.w = 12
print(Derived.w)
