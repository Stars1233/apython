# `%` formatting runs user code, and every conversion has to notice when that
# code raised.
#
# str_mod_impl called obj_str / obj_repr / builtin_ascii / fmt_percent_coerce /
# format_apply_spec and used what came back without ever testing it.  A
# __repr__ that raised handed the copier a NULL string, which it dereferenced
# at `[NULL + ob_size]`.
#
# Returning NULL is not enough either: op_binary_op reads a NULL from an nb_
# slot as "this pair is not mine" and moves on to the right operand and then
# to a TypeError -- so the real exception has to be re-raised from here, the
# way every other error exit in this function does.

class Boom(Exception):
    pass


class BadRepr:
    def __repr__(self):
        raise Boom("repr")


class BadStr:
    def __str__(self):
        raise Boom("str")

    def __repr__(self):
        return "<BadStr>"


class BadIndex:
    def __index__(self):
        raise Boom("index")


class BadFloat:
    def __float__(self):
        raise Boom("float")


def check(label, thunk):
    try:
        r = thunk()
    except Boom as e:
        print(label, "->", e.args[0])
    except TypeError as e:
        print(label, "-> TypeError", e)
    else:
        print(label, "=>", r)


# --- the plain conversions ------------------------------------------------
check("%r", lambda: "%r" % (BadRepr(),))
check("%s", lambda: "%s" % (BadStr(),))
check("%s of BadRepr", lambda: "%s" % (BadRepr(),))
check("%d", lambda: "%d" % (BadIndex(),))
check("%x", lambda: "%x" % (BadIndex(),))
check("%f", lambda: "%f" % (BadFloat(),))

# --- the same conversions with a spec, which takes the other code path ----
check("%5r", lambda: "%5r" % (BadRepr(),))
check("%-8s", lambda: "%-8s" % (BadStr(),))
check("%5a", lambda: "%5a" % (BadRepr(),))
check("%05d", lambda: "%05d" % (BadIndex(),))
check("%8.2f", lambda: "%8.2f" % (BadFloat(),))

# --- in a container, and after some output has already been written -------
check("prefix", lambda: "ok %r" % (BadRepr(),))
check("second", lambda: "%s/%r" % ("a", BadRepr()))
check("dict value", lambda: "%r" % ({1: BadRepr()},))
check("list value", lambda: "%r" % ([BadRepr()],))
check("set value", lambda: "%r" % ({BadRepr()},))
check("mapping", lambda: "%(k)r" % {"k": BadRepr()})

# --- bytes % takes the same route -----------------------------------------
check("b%r", lambda: b"%r" % (BadRepr(),))
check("b%5r", lambda: b"%5r" % (BadRepr(),))
check("b%d", lambda: b"%d" % (BadIndex(),))

# --- and none of it disturbed anything around it --------------------------
kept = ["alpha", {"b": 2}, (3, 4)]
for i in range(40):
    try:
        "%r %r" % (i, BadRepr())
    except Boom:
        pass
print(kept, "%r" % (kept,) == repr(kept))
print("%s %r %d %05.1f" % ("s", "r", 7, 2.5))
print("done")
