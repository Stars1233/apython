# A getset descriptor reached unbound.
#
# The getters behind slice.start, range.stop, int.real and the rest are C
# functions that dereference their argument as an instance of the type the
# descriptor belongs to.  Nothing checked, so
# `slice.__dict__['start'].__get__(5)` handed an int immediate to a load off
# PySliceObject and segfaulted.  builtin_func_call has had the same check for
# its own descriptors since they started carrying an owner; a getset carries
# one too, and this is that check, with CPython's wording.

def t(name, fn):
    try:
        print(name, "->", fn())
    except TypeError as e:
        print(name, "TypeError:", e)
    except AttributeError as e:
        print(name, "AttributeError:", e)


s = slice(1, 9, 2)
print("=== bound, which is the ordinary way in ===")
print(s.start, s.stop, s.step)
t("start", lambda: slice.__dict__["start"].__get__(s))
t("stop", lambda: slice.__dict__["stop"].__get__(s))
t("step", lambda: slice.__dict__["step"].__get__(s))
print(range(3).start, range(3).stop, range(3).step)
t("range start", lambda: range.__dict__["start"].__get__(range(2, 9)))
print((7).real, (7).imag, (7.5).real)
t("int real", lambda: int.__dict__["real"].__get__(7))

print("=== unbound, which is a TypeError and not a crash ===")
for wrong in (5, "x", [1], (), 2.5, True, b"y"):
    t("slice.start " + type(wrong).__name__,
      lambda w=wrong: slice.__dict__["start"].__get__(w))
t("slice.stop list", lambda: slice.__dict__["stop"].__get__([1]))
t("range.stop float", lambda: range.__dict__["stop"].__get__(1.5))
t("int.real str", lambda: int.__dict__["real"].__get__("s"))
t("set unbound", lambda: slice.__dict__["start"].__set__(5, 3))

print("=== a subclass of the owner is fine ===")
# slice itself cannot be subclassed in either interpreter, so the check goes
# through a type that can: bool is an int, and int.real applies to it.
t("bool through int.real", lambda: int.__dict__["real"].__get__(True))
t("bool through int.imag", lambda: int.__dict__["imag"].__get__(False))

print("alive")
