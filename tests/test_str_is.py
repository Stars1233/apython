# Test str is*() methods

# isalnum
print("abc123".isalnum())    # True
print("abc 123".isalnum())   # False (space)
print("".isalnum())          # False (empty)
print("ABC".isalnum())       # True

# isspace
print("   ".isspace())       # True
print(" \t\n".isspace())     # True
print("".isspace())          # False (empty)
print(" a ".isspace())       # False

# isupper
print("HELLO".isupper())     # True
print("Hello".isupper())     # False
print("".isupper())          # False (empty)
print("HELLO123".isupper())  # True (digits are not cased)
print("123".isupper())       # False (no cased chars)

# islower
print("hello".islower())     # True
print("Hello".islower())     # False
print("".islower())          # False (empty)
print("hello123".islower())  # True (digits are not cased)
print("123".islower())       # False (no cased chars)
