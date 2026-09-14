# co_lines() and co_positions() must not cost the square of a code object's
# length.
#
# Both walked every code unit and called code_addr2location for each, and that
# function rescanned co_linetable from byte zero every time.  So listing a
# function's lines was O(units * entries), and for the hundred-thousand-line
# function CPython's test_jump_extended_args_for_iter builds it was not slow
# but indefinite: unittest formats the failure's traceback through
# co_positions(), and test_sys_settrace timed out with 18 of its tests run.
# CPython walks the table once.
#
# The fix is a resume cursor the CALLER owns, so a walk over ascending offsets
# picks up where it left off.  What such a cursor can get wrong is exactly two
# things -- the answers must be unchanged, and a walk that goes backwards or
# switches code objects must not resume into another table -- and that is what
# is asserted here.
#
# What is NOT asserted is the tables themselves.  Two compilers may emit
# different bytecode for the same source and both be right, so the entries
# differ between this tree and CPython by design; comparing them would be
# measuring the compiler.  Everything below holds whichever compiler ran.
import sys
import time


def build(n, name="f"):
    src = "def %s():\n" % name
    src += "".join("    x%d = %d\n" % (i, i) for i in range(n))
    src += "    return 0\n"
    ns = {}
    exec(compile(src, "<generated>", "exec"), ns)
    return ns[name]


def shapes():
    def straight(a, b):
        c = a + b
        d = c * 2
        return d

    def looping(n):
        t = 0
        for i in range(n):
            t += i
            if t > 3:
                t -= 1
        while t > 0:
            t -= 2
        return t

    def handling(x):
        try:
            y = 1 / x
        except ZeroDivisionError:
            y = 0
        finally:
            z = 1
        return y + z

    def wide(a, bb, ccc, dddd):
        return (a
                + bb
                + ccc
                + dddd)

    def comprehending(xs):
        return [x * 2 for x in xs if x], {k: v for k, v in zip(xs, xs)}

    return [straight, looping, handling, wide, comprehending]


# --- the invariants, over shapes with different kinds of table entry --------

for fn in shapes():
    code = fn.__code__
    runs = list(code.co_lines())
    pos = list(code.co_positions())
    first = code.co_firstlineno
    last = first + len(fn.__doc__ or "") + 40      # generous upper bound

    # co_lines covers the bytecode exactly once, in order, with no gaps.
    starts = [s for s, _, _ in runs]
    ends = [e for _, e, _ in runs]
    print(fn.__name__, "runs ordered:", starts == sorted(starts))
    print(fn.__name__, "runs contiguous:", all(e == s for e, s in zip(ends, starts[1:])))
    print(fn.__name__, "covers the code:", starts[0] == 0 and ends[-1] == len(code.co_code))

    # Every line it reports belongs to the function.
    known = [l for _, _, l in runs if l is not None]
    print(fn.__name__, "lines in range:", all(first <= l <= last for l in known))

    # co_positions answers once per code unit, and its start lines are the
    # same sequence co_lines reports, expanded.
    print(fn.__name__, "positions per unit:", len(pos) == len(code.co_code) // 2)
    expanded = []
    for s, e, l in runs:
        expanded.extend([l] * ((e - s) // 2))
    print(fn.__name__, "positions agree with lines:",
          [p[0] for p in pos] == expanded)

    # A start column is never past its end column.
    print(fn.__name__, "columns ordered:",
          all(c is None or d is None or c <= d for _, _, c, d in pos))

# --- a large table, where the cursor is what makes the walk cheap ------------

big = build(300)
lines = [l for _, _, l in big.__code__.co_lines() if l is not None]
print("big first:", lines[0], "last:", lines[-1])
print("big non-decreasing:", all(b >= a for a, b in zip(lines, lines[1:])))
print("big distinct lines:", len(set(lines)) == 302)

# Two walks of the same code object must answer the same thing.  A cursor kept
# anywhere but the caller's frame is what could make them differ.
print("repeatable lines:", list(big.__code__.co_lines()) == list(big.__code__.co_lines()))
print("repeatable positions:",
      list(big.__code__.co_positions()) == list(big.__code__.co_positions()))

# Interleaving two code objects, so a SHARED cursor would be pointing into the
# wrong table on every other call.
a = build(40, "a")
b = build(70, "b")
al, bl, ap_, bp = (list(a.__code__.co_lines()), list(b.__code__.co_lines()),
                   list(a.__code__.co_positions()), list(b.__code__.co_positions()))
same = True
for _ in range(3):
    same = same and list(a.__code__.co_lines()) == al
    same = same and list(b.__code__.co_lines()) == bl
    same = same and list(a.__code__.co_positions()) == ap_
    same = same and list(b.__code__.co_positions()) == bp
print("interleaved stable:", same)
print("interleaved distinct:", len(al) != len(bl))

# A traceback out of a large function: the frame's line number comes from the
# same walk, and it must name the line that actually raised.
exec("def boom():\n" + "".join("    y%d = %d\n" % (i, i) for i in range(2000))
     + "    raise ValueError('here')\n", globals())
try:
    boom()
except ValueError:
    tb = sys.exc_info()[2]
    depths = []
    while tb is not None:
        depths.append(tb.tb_lineno)
        tb = tb.tb_next
print("traceback line numbers:", depths)

# --- the scaling ------------------------------------------------------------
#
# A ratio, not a wall-clock bound, so it says the same thing on a fast machine
# and a slow one: quadruple the lines and a single-pass walk costs four times
# as much, a rescan-per-unit sixteen.  Six is clear of both.

def timed(n):
    code = build(n, "t%d" % n).__code__
    list(code.co_positions())           # warm anything that warms
    best = None
    for _ in range(3):
        t0 = time.time()
        list(code.co_positions())
        d = time.time() - t0
        if best is None or d < best:
            best = d
    return best


small = timed(1500)
large = timed(6000)
ratio = (large / small) if small > 0.0005 else 0.0
print("sub-quadratic:", ratio < 6.0)

print("done")
