import sys

print("in __main__ of a package")
print("name:", __name__)
print("package:", repr(__package__))
print("file ends with:", __file__.replace("\\", "/").endswith("climod/__main__.py"))
print("argv tail:", sys.argv[1:])
