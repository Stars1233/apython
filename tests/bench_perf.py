# Benchmark: tight loop with arithmetic + comparison
x = 0
while x < 5000000:
    x = x + 1
# Benchmark: many print calls
for i in range(50000):
    print(i)
