# Test __init_subclass__ and __class_getitem__

# === __init_subclass__ ===
print("--- __init_subclass__ ---")

class Plugin:
    registry = []

    def __init_subclass__(cls):
        Plugin.registry.append(cls.__name__)

class AudioPlugin(Plugin):
    pass

class VideoPlugin(Plugin):
    pass

print(Plugin.registry)

# === __class_getitem__ ===
print("--- __class_getitem__ ---")

class MyGeneric:
    @classmethod
    def __class_getitem__(cls, item):
        return "MyGeneric[" + str(item) + "]"

print(MyGeneric["int"])
print(MyGeneric[42])

# === metaclass= and the type() protocol ===
class M(type):
    def __new__(mcls, name, bases, ns):
        cls = super().__new__(mcls, name, bases, ns)
        cls.tagged = True
        return cls
    def __instancecheck__(cls, obj):
        return getattr(obj, "quacks", False)
    def __subclasscheck__(cls, sub):
        return getattr(sub, "quacks_cls", False)

class Duck(metaclass=M):
    pass

class Quacker:
    quacks = True
    quacks_cls = True

print(type(Duck) is M, Duck.tagged)
print(isinstance(Quacker(), Duck), isinstance(1, Duck))
print(issubclass(Quacker, Duck), issubclass(int, Duck))
print(issubclass(Quacker, (int, Duck)), issubclass(bool, (int, Duck)))
print(isinstance(Quacker(), (int, str)), isinstance(3, (int, str)))
print(M("Dyn", (), {"x": 1}).x, type(M("Dyn", (), {})).__name__)
print(Duck.__name__, M.__name__, isinstance(Duck, M), issubclass(M, type))
print(isinstance(Quacker(), (int, Duck)), isinstance(3, (str, Duck)))

# __init__ on the metaclass runs after __new__
order = []
class M2(type):
    def __new__(mcls, name, bases, ns):
        order.append("new")
        return super().__new__(mcls, name, bases, ns)
    def __init__(cls, name, bases, ns):
        order.append("init")
        super().__init__(name, bases, ns)

class C(metaclass=M2):
    pass
print(order, type(C).__name__)

# A metaclass is inherited by subclasses
class D(C):
    pass
print(type(D) is M2, order)

# Methods defined on the metaclass are callable on the class, not instances
class M3(type):
    def shout(cls):
        return cls.__name__.upper()
class E(metaclass=M3):
    pass
print(E.shout())
try:
    E().shout()
except AttributeError as e:
    print("AttributeError")

# 3-arg type() still builds an ordinary class
T = type("T", (object,), {"v": 7})
print(T.v, type(T) is type, issubclass(T, object))

# Metaclass attributes are visible on the class but not its instances
class M4(type):
    counter = 99
class F(metaclass=M4):
    pass
print(F.counter)
