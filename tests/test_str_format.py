# str.format: the whole replacement-field grammar, not just {} and {N}.
print("{} {} {}".format(1, "a", 2.5))
print("{0} {1} {0}".format("x", "y"))
print("{name}-{other}".format(name="n", other="o"))
print("{0}{name}".format("p", name="q"))
print("{:>8}|{:<8}|{:^8}|".format("a", "b", "c"))
print("{:08.3f} {:+d} {:x} {:#o} {:,}".format(3.14159, 42, 255, 8, 1234567))
print("{!r} {!s}".format("q", "q"))
print("{{literal}} {} {{{}}}".format(1, 2))
print("{:*^11}".format("hello"))
print("{:s}".format("plain"), "{:6}".format("ab") + "|")
print("{}".format({"k": [1, 2]}), "{}".format(None), "{}".format(True))
print("{:.3}".format("abcdefg"), "{:>6.3}".format("abcdefg"))
try:
    "{5}".format(1)
except IndexError:
    print("IndexError")
try:
    "{zz}".format(a=1)
except KeyError:
    print("KeyError")
try:
    "{".format()
except ValueError:
    print("ValueError")
