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
