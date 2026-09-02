# bytes.rsplit and bytearray.rsplit.
#
# split took its maxsplit, but the right-hand form was never written, so
# b'a,b,c'.rsplit(b',', 1) was an AttributeError.  str already had the shape
# to copy: one implementation with a direction flag and two bare trampolines.
# The right-hand arms scan backwards and insert each piece at the front, which
# is how str_split_impl does it too.

cases = [
 (b'a,b,c', b',', None), (b'a,b,c', b',', 1), (b'a,b,c', b',', 2), (b'a,b,c', b',', 0),
 (b'a,,b', b',', None), (b',a,', b',', None), (b'', b',', None), (b'abc', b',', None),
 (b'aXXbXXc', b'XX', 1), (b'aXXbXXc', b'XX', None),
]
for data, sep, n in cases:
    if n is None:
        print(data, sep, data.rsplit(sep), data.split(sep))
    else:
        print(data, sep, n, data.rsplit(sep, n), data.split(sep, n))
ws = [(b'  a  b  c  ', None), (b'a b', None), (b'', None), (b'   ', None), (b' a b ', None)]
for data, sep in ws:
    print(data, data.rsplit(), data.split())
    for n in (0, 1, 2):
        print(data, n, data.rsplit(sep, n), data.split(sep, n))
b = bytearray(b'a,b,c')
print(b.rsplit(b',', 1), b.split(b',', 1), bytearray(b' a b ').rsplit(None, 1))
print(type(b.rsplit(b',')[0]).__name__)
try:
    b'abc'.rsplit(b'')
except ValueError as e:
    print("ValueError")
