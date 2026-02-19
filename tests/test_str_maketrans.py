# Test str.maketrans and translate together
t = str.maketrans("abc", "xyz")
print("abcdef".translate(t))
print("banana".translate(t))
print("hello".translate(t))
