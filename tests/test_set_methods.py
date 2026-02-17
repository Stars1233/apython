# Test set mutation methods: add, remove, discard, pop, clear, copy

# add
s = set()
s.add(1)
s.add(2)
s.add(2)  # duplicate
print(len(s))  # 2

# remove (raises KeyError if missing)
s.remove(1)
print(len(s))  # 1
try:
    s.remove(99)
    print("ERROR: should have raised KeyError")
except KeyError:
    print("KeyError OK")

# discard (no error if missing)
s.add(5)
s.add(6)
s.discard(5)
s.discard(999)  # no error
print(len(s))   # 2 (has 2 and 6)

# pop
s2 = {10, 20, 30}
popped = s2.pop()
print(popped in (10, 20, 30))  # True
print(len(s2))  # 2

# clear
s3 = {1, 2, 3}
s3.clear()
print(len(s3))  # 0

# copy
s4 = {7, 8, 9}
s5 = s4.copy()
print(len(s5))       # 3
print(7 in s5)       # True
s5.add(100)
print(100 in s4)     # False (independent copy)
