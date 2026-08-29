# The protocols a user class can implement, now that its dunders reach real
# type slots.  Before, whichever operation nobody had wired an ad-hoc
# fallback for was simply absent: hash(obj) raised, -obj crashed, abs(obj)
# raised, +obj returned the operand unchanged, __index__ and __format__ and
# __getattr__ were ignored entirely.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


class Num:
    def __init__(self, n):
        self.n = n

    def __repr__(self):
        return "Num(%d)" % self.n

    def __neg__(self):
        return Num(-self.n)

    def __pos__(self):
        return Num(abs(self.n))

    def __invert__(self):
        return Num(~self.n)

    def __abs__(self):
        return Num(abs(self.n))

    def __hash__(self):
        return self.n * 7

    def __bool__(self):
        return self.n != 0

    def __len__(self):
        return 3

    def __int__(self):
        return self.n

    def __float__(self):
        return float(self.n)

    def __index__(self):
        return self.n

    def __format__(self, spec):
        return "fmt[%s]" % spec


v = Num(-3)
print(-v, +v, ~v, abs(v))
print(hash(v), hash(Num(2)), len(v))

# __bool__ must win over __len__, which is the priority obj_is_true applies
# only if nb_bool is filled -- otherwise a length of 3 makes Num(0) truthy
print(bool(Num(0)), bool(Num(3)), not Num(0))

print(int(Num(5)), float(Num(5)))

# __index__ makes an object usable wherever an integer is
print([10, 20, 30][Num(1)], (1, 2, 3)[Num(2)], [10, 20, 30][Num(-1)])
print(list(range(Num(3))), hex(Num(255)), oct(Num(8)), bin(Num(5)))
print("abcdef"[Num(2)], b"abcdef"[Num(1)])

# __format__ through both format() and an f-string
print(format(v, ">5"), format(v), f"{v:^7}", f"{v}")


class BadIndex:
    def __index__(self):
        return "no"


bad = BadIndex()
frac = 1.5
print(t(lambda: [1, 2, 3][bad]), t(lambda: [1, 2, 3][frac]))


# __getattr__ runs only when ordinary resolution fails
class Dyn:
    real = "declared"

    def __getattr__(self, name):
        return "dyn:" + name


d = Dyn()
d.stored = "instance"
print(d.real, d.stored, d.missing, getattr(d, "q"), getattr(d, "r", "unused"))
print(hasattr(d, "anything"))


class Plain:
    pass


print(t(lambda: Plain().nope), getattr(Plain(), "nope", "dflt"))


# A dunder that raises reaches the caller at the operation, not later
class Angry:
    def __hash__(self):
        raise ValueError("h")

    def __neg__(self):
        raise ValueError("n")

    def __len__(self):
        raise ValueError("l")

    def __getattr__(self, name):
        raise ValueError("g")


a = Angry()
print(t(lambda: hash(a)), t(lambda: -a), t(lambda: len(a)), t(lambda: a.zzz))


# __len__ must return a non-negative int
class NegLen:
    def __len__(self):
        return -1


class StrLen:
    def __len__(self):
        return "x"


print(t(lambda: len(NegLen())), t(lambda: len(StrLen())))


# __bool__ must return a bool
class BadBool:
    def __bool__(self):
        return 3


print(t(lambda: bool(BadBool())))
