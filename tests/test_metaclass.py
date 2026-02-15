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
