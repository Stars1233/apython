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

# open(..., opener=...) hands the path and the computed flags to a callable and
# uses the descriptor it returns.  tempfile.NamedTemporaryFile is written on
# it: it passes the DIRECTORY as `file` and an opener that ignores it and
# returns a descriptor from mkstemp, so ignoring the opener meant opening the
# directory -- IsADirectoryError, from a function that had nothing to do with
# directories.
import os

seen = []


def opener(path, flags):
    seen.append((path, bool(flags & os.O_CREAT), bool(flags & os.O_WRONLY)))
    return os.open("/tmp/apython_test_opener.txt", flags, 0o600)


f = open("/tmp/ignored-by-the-opener", "w", opener=opener)
f.write("through the opener\n")
f.close()
print(seen)
print(open("/tmp/apython_test_opener.txt").read().strip())

# The name is the argument as given, not what the opener actually opened.
seen.clear()
f = open("/tmp/ignored-by-the-opener", "r", opener=opener)
print(f.buffer.raw.name)
print(f.read().strip())
f.close()
print(seen)

# A descriptor is what an opener has to return.
def bad_opener(path, flags):
    return "not a descriptor"


try:
    open("/tmp/whatever", "r", opener=bad_opener)
except TypeError as e:
    print("TypeError:", e)

os.unlink("/tmp/apython_test_opener.txt")
print("opener done")
