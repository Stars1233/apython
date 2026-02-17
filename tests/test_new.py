# Test user-defined __new__

# Basic __new__ that modifies instance before __init__
class Tracker:
    def __new__(cls, val):
        instance = object.__new__(cls)
        instance.created_by_new = True
        return instance

    def __init__(self, val):
        self.val = val

t = Tracker(42)
print(t.created_by_new)  # True
print(t.val)             # 42

# Singleton pattern via __new__
class Singleton:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = object.__new__(cls)
        return cls._instance

    def __init__(self):
        self.initialized = True

a = Singleton()
b = Singleton()
print(a is b)            # True
print(a.initialized)     # True

# __new__ without __init__
class NoInit:
    def __new__(cls, x):
        instance = object.__new__(cls)
        instance.x = x
        return instance

n = NoInit(99)
print(n.x)               # 99

# __new__ returning different type skips __init__
class Weird:
    def __new__(cls, val):
        return val  # Return the value directly, not an instance

    def __init__(self):
        pass  # Should NOT be called

w = Weird(123)
print(w)                 # 123
