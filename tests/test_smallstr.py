# === Creation & identity ===
s0 = ""               # empty SmallStr
s1 = "a"              # 1-byte
s7 = "abcdefg"        # 7-byte (fits in payload only)
s8 = "abcdefgh"       # 8-byte (spills into tag)
s14 = "12345678901234" # 14-byte (max SmallStr)
s15 = "123456789012345" # 15-byte (heap, NOT SmallStr)
print(s0, s1, s7, s8, s14, s15)

# === len ===
print(len(""))         # 0
print(len("a"))        # 1
print(len("abcdefg"))  # 7
print(len("abcdefgh")) # 8
print(len("12345678901234"))  # 14
print(len("123456789012345")) # 15

# === Equality ===
print("abc" == "abc")     # True (SmallStr == SmallStr)
print("abc" != "def")     # True
print("abc" == "abcd")    # False (diff length)
print("" == "")           # True
print("12345678901234" == "12345678901234")  # True (14B boundary)
print("123456789012345" == "123456789012345") # True (heap == heap)

# === Comparison operators ===
print("abc" < "abd")      # True
print("abc" > "abb")      # True
print("abc" <= "abc")     # True
print("abc" >= "abc")     # True
print("a" < "b")          # True
print("b" > "a")          # True
print("abc" < "abcd")     # True (prefix shorter)

# === Concatenation ===
print("ab" + "cd")        # "abcd" (SmallStr + SmallStr)
print("1234567" + "8901234")  # "12345678901234" (14B, still SmallStr)
print("12345678" + "9012345") # "123456789012345" (15B, → heap)
print("" + "abc")         # "abc"
print("abc" + "")         # "abc"

# === Repetition ===
print("ab" * 3)           # "ababab" (SmallStr result)
print("ab" * 0)           # "" (empty)
print("abc" * 4)          # "abcabcabcabc" (12B, SmallStr)
print("abc" * 5)          # "abcabcabcabcabc" (15B, heap)

# === Bool / truthiness ===
print(bool(""))            # False
print(bool("x"))           # True
if "hello":
    print("truthy")
if not "":
    print("empty is falsy")

# === isinstance / type ===
print(isinstance("hi", str))  # True
print(isinstance("", str))    # True
print(type("hi"))              # <class 'str'>
print(type("") == str)         # True
print(type("hi") == str)       # True
print(type("hi") is str)       # True

# === repr ===
print(repr(""))            # ''
print(repr("hello"))       # 'hello'

# === str() constructor ===
print(str())               # "" (empty)
print(str(42))             # "42"
print(str(True))           # "True"
print(str(None))           # "None"
print(str(3.14))           # "3.14"

# === in / contains ===
print("bc" in "abcd")     # True
print("xy" in "abcd")     # False
print("" in "abc")        # True
print("a" in "a")         # True

# === Indexing ===
s = "hello"
print(s[0])                # h
print(s[4])                # o
print(s[-1])               # o
print(s[-5])               # h

# === Slicing ===
s = "hello world"
print(s[0:5])              # "hello"
print(s[6:])               # "world"
print(s[:5])               # "hello"
print(s[::2])              # "hlowrd"
print(s[::-1])             # "dlrow olleh"

# === Iteration ===
for c in "abc":
    print(c)               # a, b, c

print(list("hello"))       # ['h', 'e', 'l', 'l', 'o']

# === Dict with SmallStr keys ===
d = {"a": 1, "bb": 2, "ccc": 3}
print(d["a"])              # 1
print(d["bb"])             # 2
print(d["ccc"])            # 3
d["dd"] = 4
print(d["dd"])             # 4
print("a" in d)            # True
print("z" in d)            # False

# === String formatting ===
print("hello %s" % "world")
print("%d items" % 5)
print("pi=%s" % 3.14159)

# === f-string (BUILD_STRING) ===
name = "world"
print(f"hello {name}")
n = 42
print(f"n={n}")

# === Hashing consistency ===
d = {}
key = "test"
d[key] = 1
print(d["test"])           # 1

# === Mixed operations ===
x = "abc"
y = "def"
z = x + y                  # "abcdef" (SmallStr concat)
print(z)
print(len(z))              # 6
print(z == "abcdef")       # True
print(z[2:4])              # "cd"
print(z * 2)               # "abcdefabcdef" (12B, SmallStr)

# === Edge: 14-byte boundary ===
a = "1234567"              # 7B
b = "8901234"              # 7B
c = a + b                  # 14B (max SmallStr)
print(len(c))              # 14
print(c)                   # "12345678901234"
print(c == "12345678901234")  # True
d_dict = {c: "found"}
print(d_dict["12345678901234"])  # "found"

# === Edge: 0-byte string ===
e = ""
print(repr(e))             # ''
print(len(e))              # 0
print(e == "")             # True
print(bool(e))             # False
print(e + "abc")           # "abc"
print("abc" + e)           # "abc"

# === String methods ===
print("HELLO".lower())     # "hello"
print("hello".upper())     # "HELLO"
print("  hi  ".strip())    # "hi"
print("hello world".split()) # ['hello', 'world']
print(",".join(["a","b","c"])) # "a,b,c"
print("hello".startswith("he")) # True
print("hello".endswith("lo"))   # True
print("hello".find("ll"))       # 2
print("hello".replace("l","r")) # "herro"
print("hello".count("l"))       # 2
print("hello".index("e"))       # 1
