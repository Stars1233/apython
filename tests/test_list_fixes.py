# 1. richcompare NotImplemented -> cross-type __eq__
class EqAll:
    def __eq__(self, other):
        return True
print([1] == EqAll())      # True

# 2. list contains with __eq__
class C:
    def __init__(self, v):
        self.v = v
    def __eq__(self, other):
        return isinstance(other, C) and self.v == other.v
lst2 = [C(1)]
print(C(1) in lst2)        # True
print(C(2) in lst2)        # False

# 3. extend with tuple, range
lst = [1]
lst.extend((2, 3))
lst.extend(range(4, 7))
print(lst)                  # [1, 2, 3, 4, 5, 6]

# 4. remove/count with __eq__
lst = [C(1), C(2), C(3), C(1)]
print(lst.count(C(1)))     # 2
lst.remove(C(2))
print(len(lst))            # 3
print(lst.index(C(3)))     # 1

# 5. += identity preservation
a = [1, 2]
b = a
a += [3]
print(a is b)              # True
print(a)                   # [1, 2, 3]

# 6. *= identity preservation
a = [1, 2]
b = a
a *= 3
print(a is b)              # True
print(a)                   # [1, 2, 1, 2, 1, 2]

# 7. *= edge cases
a = [1, 2]
b = a
a *= 0
print(a is b)              # True
print(a)                   # []

# 8. reversed
print(list(reversed([1, 2, 3])))  # [3, 2, 1]

# 9. sort with key=
lst = ['banana', 'apple', 'cherry']
lst.sort(key=len)
print(lst)                  # ['apple', 'banana', 'cherry']

# 10. sort with key= and reverse=
lst = [3, 1, 4, 1, 5, 9, 2, 6]
lst.sort()
print(lst)                  # [1, 1, 2, 3, 4, 5, 6, 9]

# 11. sort stability
pairs = [(1, 'b'), (2, 'a'), (1, 'a'), (2, 'b')]
pairs.sort(key=lambda x: x[0])
print(pairs)                # [(1, 'b'), (1, 'a'), (2, 'a'), (2, 'b')]

# 12. larger sort (regression: must not be O(n^2) slow)
lst = list(range(500, 0, -1))
lst.sort()
print(lst[0], lst[-1])     # 1 500

# 13. [*range()]
print([*range(5)])          # [0, 1, 2, 3, 4]

# 14. slice assignment with range
lst = [0, 1, 2, 3, 4]
lst[1:3] = range(10, 13)
print(lst)                  # [0, 10, 11, 12, 3, 4]

# 15. a, *rest = range()
a, *rest, z = range(5)
print(a, rest, z)           # 0 [1, 2, 3] 4
