# Test str.format_map
d = {"name": "World", "count": 3}
print("{name}!".format_map(d))
print("{name} {count}".format_map(d))
print("no braces".format_map(d))
print("{{literal}}".format_map(d))
print("{name} says {{hi}}".format_map(d))
