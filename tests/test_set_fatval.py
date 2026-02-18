# SmallStr in sets
s = {"hi", "bye"}
print("hi" in s)        # True
print("nope" in s)      # False
s.discard("hi")
print("hi" in s)        # False
print(len(s))           # 1

# SmallInt in sets
s2 = {1, 2, 3}
print(2 in s2)          # True
s2.remove(2)
print(2 in s2)          # False — tests tombstone probing
print(3 in s2)          # True — must find past tombstone

# Non-pointer values
s3 = {True, False, None}
print(True in s3)       # True
print(None in s3)       # True
