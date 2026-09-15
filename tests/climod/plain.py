import sys

print("plain module")
print("name:", __name__)
print("package:", repr(__package__))
print("argv0 ends with:", sys.argv[0].replace("\\", "/").endswith("plain.py"))
print("argv tail:", sys.argv[1:])
