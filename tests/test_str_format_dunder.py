# str.format goes through __format__, as f-strings and format() do.
#
# It went straight to the spec machinery instead, so a class with a __format__
# of its own was never asked: "{}".format(obj) printed the default repr, and
# "{:x}".format(obj) reported that the object could not be interpreted as an
# integer.  The three spellings are one operation and now share one funnel.


class F:
    def __format__(self, spec):
        return "F(" + spec + ")"


print("{}".format(F()), "{:x}".format(F()), format(F(), "y"), f"{F():z}")
print("{0} {0:a} {0}".format(F()))
print("{:{}}".format(F(), "w"))


# A __format__ that returns a non-str is refused here too.
class Bad:
    def __format__(self, spec):
        return 5


for call in (lambda: "{}".format(Bad()), lambda: "{:x}".format(Bad())):
    try:
        call()
    except TypeError as e:
        print(e)


# Everything str.format already did must still work.
print("{0} {1:>4} {k}".format(1, 22, k=3))
print("{:>6.2f} {:d} {!r} {!s}".format(3.14159, 42, "a", "b"))
print("{:{}}".format(7, ">5"))
print("{0.real} {0.imag}".format(1 + 2j))
print("{[0]}".format([9]))
print("{:%}".format(0.25), "{:,}".format(1234567), "{:+.3e}".format(-1.5))
print("{:08.3f}".format(3.14159), "{:#x} {:#o} {:#b}".format(255, 8, 5))
print("{}".format(None), "{}".format([1, 2]), "{}".format({1: 2}))
print("{:s}".format("x"), "{:>3}".format("y"), "{:^5}".format("z"))


class G:
    def __str__(self):
        return "S"

    def __repr__(self):
        return "R"


print("{} {!r} {!s}".format(G(), G(), G()))

# An int and a float through both arms, which is where the two parities met.
print("{}".format(2 ** 70), "{:d}".format(2 ** 70))
print("{}".format(1.5), "{:.1f}".format(1.5))
print("".join("{}".format(i) for i in range(5)))

print("done")
