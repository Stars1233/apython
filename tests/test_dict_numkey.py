# Test dict cross-type numeric key equality
d = {}
d[1] = "int"
print(d[1.0])             # int
print(d[True])            # int
d[0] = "zero"
print(d[0.0])             # zero
print(d[False])           # zero

# Verify overwrite behavior
d2 = {1: "one", True: "true_val"}
print(d2[1])              # true_val (True overwrites 1)
print(len(d2))            # 1
