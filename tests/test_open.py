# Test open() builtin - write and read back

# Write to temp file
f = open("/tmp/apython_test_open.txt", "w")
f.write("hello world\n")
f.write("line two\n")
f.close()

# Read back entire file
f2 = open("/tmp/apython_test_open.txt", "r")
content = f2.read()
f2.close()
print(repr(content))

# Read line by line
f3 = open("/tmp/apython_test_open.txt", "r")
line1 = f3.readline()
line2 = f3.readline()
f3.close()
print(repr(line1))
print(repr(line2))
print("done")
