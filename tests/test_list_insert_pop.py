# Test list insert, pop, and remove shift operations

# insert at various positions
lst = [1, 2, 3, 4, 5]
lst.insert(0, 0)       # insert at front (backward shift of all elements)
lst.insert(3, 99)      # insert in middle
lst.insert(len(lst), 100)  # insert at end (no shift)
lst.insert(-1, 88)     # negative index
print(lst)

# pop from various positions
lst = [10, 20, 30, 40, 50]
print(lst.pop(0))      # pop front (forward shift of all elements)
print(lst.pop(1))      # pop middle
print(lst.pop())       # pop last (no shift)
print(lst)

# combined insert+pop pattern (fannkuch's hot path)
lst = list(range(10))
for i in range(5):
    lst.insert(i, lst.pop(0))
print(lst)

# remove (also uses shift)
lst = [1, 2, 3, 4, 5]
lst.remove(1)           # remove first (shift all)
lst.remove(5)           # remove last (no shift)
lst.remove(3)           # remove middle
print(lst)

# edge cases
lst = [42]
print(lst.pop(0))       # single-element pop
print(lst)
lst.insert(0, 99)       # insert into empty list
print(lst)

# larger list shifts
lst = list(range(20))
lst.insert(0, lst.pop(0))  # pop front, insert front (identity)
print(lst)
lst.insert(10, lst.pop(0))  # pop front, insert middle
print(lst)
