# Test int.to_bytes
print((256).to_bytes(2, "big"))
print((256).to_bytes(2, "little"))
print((0).to_bytes(1, "big"))
print((255).to_bytes(1, "big"))
print((65535).to_bytes(2, "big"))

# Test int.from_bytes
print(int.from_bytes(b'\x00\x01', "big"))
print(int.from_bytes(b'\x01\x00', "little"))
print(int.from_bytes(b'\xff', "big"))
print(int.from_bytes(b'\x00', "big"))
print(int.from_bytes(b'\x01\x02\x03', "big"))

# length and byteorder are positional-or-keyword, not positional-only.
# pickle spells byteorder= out, so taking only signed= as a keyword made
# every such call a TypeError.
print((255).to_bytes(2, byteorder="little"))
print((255).to_bytes(length=2, byteorder="little"))
print((255).to_bytes(byteorder="big", length=3))
print(int.from_bytes(b'\x01\x02', byteorder="little"))
print(int.from_bytes(b'\xff\xfe', byteorder="big", signed=True))
print(int.from_bytes(bytes=b'\x01\x02', byteorder="little"))
print((-2).to_bytes(2, "big", signed=True))

# A byteorder that is not a str is a TypeError; a str that is neither
# spelling is a ValueError.  Both used to come out as ValueError.
try:
    (255).to_bytes(2, 5)
except TypeError:
    print("to_bytes non-str byteorder: TypeError")
try:
    (255).to_bytes(2, "sideways")
except ValueError:
    print("to_bytes bad byteorder: ValueError")
try:
    int.from_bytes(b'\x01', byteorder=7)
except TypeError:
    print("from_bytes non-str byteorder: TypeError")
try:
    (255).to_bytes(2, "big", bogus=1)
except TypeError:
    print("to_bytes unknown keyword: TypeError")
