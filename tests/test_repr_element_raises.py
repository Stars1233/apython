# A container repr whose element repr raises must leave its caller's registers
# exactly as it found them.
#
# dict_repr and set_repr keep the entry index in r12 and PUSH it across the
# nested obj_repr, popping it once that repr comes back.  The failure exit did
# not pop it: it ran straight into `pop r14 / pop r13 / pop r12 / pop rbx`,
# which then took each saved register off by one slot.  r13 is the interpreter's
# VALUE STACK POINTER, so the caller resumed with a stack pointer that was
# really the saved r14 -- and the first thing that happens after a failed call
# is the exception unwinder, which compares that pointer against the frame's
# own stack and, finding it far below, ZEROED every word in between.  Tens of
# kilobytes of the heap, from one raising __repr__.
#
# It needed a CALL_FUNCTION_EX to show, because that is the handler that
# publishes r13 to the unwinder itself.

class Boom(Exception):
    pass


class BadRepr:
    def __repr__(self):
        raise Boom("no repr for you")

    def __hash__(self):
        return 7

    def __eq__(self, other):
        return self is other


def call_ex(args, kwargs):
    return args[0](*args[1:], **kwargs)


def check(label, thunk):
    try:
        thunk()
    except Boom as e:
        print(label, "raised", e.args[0])
    else:
        print(label, "DID NOT RAISE")


# --- a dict value, a dict key, and a set member --------------------------
check("dict value ex", lambda: call_ex((repr, {1: BadRepr()}), {}))
check("dict key ex", lambda: call_ex((repr, {BadRepr(): 1}), {}))
check("set ex", lambda: call_ex((repr, {BadRepr()}), {}))
check("frozenset ex", lambda: call_ex((repr, frozenset([BadRepr()])), {}))

check("dict value", lambda: repr({1: BadRepr()}))
check("dict key", lambda: repr({BadRepr(): 1}))
check("set", lambda: repr({BadRepr()}))

# The second entry, so the loop has already been round once.
check("dict later", lambda: call_ex((repr, {1: 1, 2: BadRepr()}), {}))
check("set later", lambda: call_ex((repr, {1, 2, 3, BadRepr()}), {}))

# Nested one level down: the inner container's failure has to travel out
# through the outer one's own failure exit.
check("nested list", lambda: call_ex((repr, [{1: BadRepr()}]), {}))
check("nested dict", lambda: call_ex((repr, {0: {1: BadRepr()}}), {}))
check("nested tuple", lambda: call_ex((repr, ({BadRepr()},)), {}))

# str() reaches the same code; so does the repr the interpreter itself takes
# when it builds a message.
check("str", lambda: call_ex((str, {1: BadRepr()}), {}))
check("str direct", lambda: str({BadRepr()}))
check("print", lambda: print({1: BadRepr()}))

# --- the interpreter's own stack survived ---------------------------------
# Values live on the value stack across the failing call; if the unwinder
# rewrote it, they come back as None or as garbage.
def survives():
    a, b, c = "alpha", ["beta"], (1, 2, 3)
    try:
        call_ex((repr, {1: BadRepr()}), {})
    except Boom:
        pass
    return a, b, c, len(a) + len(b) + len(c)


print(survives())

deep = [{"k": BadRepr()}]
for i in range(30):
    try:
        call_ex((repr, deep), {})
    except Boom:
        pass
print("loop ok", len(deep))

# And the containers themselves are unharmed.
d = {1: 2, 3: 4}
s = {5, 6}
for i in range(10):
    check("mix %d" % i, lambda: call_ex((repr, {1: BadRepr()}), {}))
print(sorted(d.items()), sorted(s))
print("done")
