# Test float.fromhex
print(float.fromhex('0x0.0p+0'))
print(float.fromhex('0x1.0000000000000p+0'))
print(float.fromhex('-0x1.0000000000000p+0'))
print(float.fromhex('0x1.0000000000000p-1'))
print(float.fromhex('0x1.0000000000000p+1'))
print(float.fromhex('0x1.999999999999ap-4'))
