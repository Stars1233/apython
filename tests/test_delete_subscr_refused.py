# A refused `del obj[key]` must not release its operands twice.
#
# op_delete_subscr's error arm jumped into eval_exception_unwind without
# republishing eval_saved_r13 -- and DISPATCH saved the stack top from BEFORE
# the two operands came off it, so the unwinder released both a second time.
# `del a[99]` on an array printed `array('[', [1675724320, 32139, 3])`: the
# array's own type-name field had been freed and reused.
import array
import gc

a = array.array("i", [1, 2, 3])
for bad in (99, -99, 3):
    try:
        del a[bad]
    except IndexError:
        print("IndexError", bad)
print(a, a.typecode, len(a))

m = memoryview(bytearray(b"abc"))
try:
    del m[0]
except TypeError as e:
    print("TypeError", e)
print(bytes(m))

d = {"k": 1}
for bad in ("nope", 7):
    try:
        del d[bad]
    except KeyError:
        print("KeyError", bad)
print(d)

l = [1, 2, 3]
try:
    del l[99]
except IndexError:
    print("IndexError list")
print(l)


class Refuses:
    def __delitem__(self, k):
        raise ValueError("no")


r = Refuses()
for _ in range(3):
    try:
        del r[0]
    except ValueError:
        pass
print("survived", type(r).__name__)

gc.collect()
print(a, d, l)
