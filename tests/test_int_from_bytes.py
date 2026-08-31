# int.from_bytes' byteorder has defaulted to 'big' since 3.11.  The
# implementation read args[2] unconditionally, so the one-argument form walked
# off the end of the argument array; ipaddress calls it that way.
print(int.from_bytes(b"\x01\x02"))
print(int.from_bytes(b"\x01\x02", "big"))
print(int.from_bytes(b"\x01\x02", "little"))
print(int.from_bytes(b""))
print(int.from_bytes(b"\xff\xff\xff\xff"))
print(int.from_bytes(bytes([1, 0, 0])), int.from_bytes(bytes([1, 0, 0]), "little"))

# Through an instance of the class, and on a subclass.
class I(int):
    pass


print(I.from_bytes(b"\x02"), type(I.from_bytes(b"\x02")).__name__)

# to_bytes round-trips.
n = 258
print(int.from_bytes(n.to_bytes(2, "big"), "big"))
print(int.from_bytes(n.to_bytes(2, "little"), "little"))

# Any iterable of ints works, which is how ipaddress calls it.
print(int.from_bytes([1, 2], "big"), int.from_bytes(map(int, "12"), "big"))
print(int.from_bytes(iter([0, 255]), "big"), int.from_bytes(bytearray(b"\x01\x00")))

# A bad byteorder is still an error.
try:
    int.from_bytes(b"\x01", "sideways")
except ValueError:
    print("bad byteorder")
try:
    int.from_bytes()
except TypeError:
    print("no args")
