# What a crafted .pyc and a crafted _sre program are allowed to do.
#
# Marshal validated offsets and lengths and never types, so a stream could
# hand co_names a tuple of ints and eval_frame would dereference one as a
# PyStrObject; frame_new added two 32-bit .pyc fields in 32 bits, so a pair
# near 2^31 wrapped to a small total and the frame came out far too short;
# and the regex engine bounded its opcode but not its program counter, so a
# program whose last word is not SUCCESS walked off the end of the code array
# and kept dispatching on whatever followed.
#
# A .pyc is a file on disk, often written by something other than the program
# running it, and _sre.compile() is a private API that does not validate its
# input -- CPython segfaults on some of what this file feeds it.  Neither is
# a trusted format.
#
# The checks are the point; this file proves they hold by feeding the shapes
# they refuse and showing the interpreter still standing.

import _sre
import re


def check(label, fn):
    """Did it refuse, one way or another, without taking the process with it?

    CPython refuses most of these when _sre.compile() validates the program,
    and this engine refuses them when the match runs off the end -- so which
    exception, or whether there is one at all, is not the same on the two
    sides.  What IS the same is that a malformed program never matches and
    the interpreter is still there afterwards, which is the whole claim.
    """
    try:
        matched = fn()
    except Exception:
        matched = False
    print(label.ljust(30), "matched" if matched else "no match")


# A pattern whose program runs off the end: no SUCCESS, no FAILURE, just an
# instruction and then whatever the allocator left behind.
FAILURE, SUCCESS, ANY, LITERAL, MARK = 0, 1, 2, 19, 17

check("no terminator",
      lambda: _sre.compile("", 0, [ANY], 0, {}, ()).match("a") is not None)
check("literal, no terminator",
      lambda: _sre.compile("", 0, [LITERAL, 97], 0, {}, ()).match("a") is not None)
check("empty program",
      lambda: _sre.compile("", 0, [], 0, {}, ()).match("a") is not None)
check("terminated properly",
      lambda: _sre.compile("", 0, [ANY, SUCCESS], 0, {}, ()).match("a") is not None)


def survives(label, fn):
    """A MARK with an absurd group index.

    CPython refuses the program when _sre.compile() validates it, and this
    one runs it -- and would have grown the mark array to twice whatever the
    index said, which for 2^40 is an allocation nobody asked for.  It refuses
    the mark instead and carries on, so the two differ in whether the match
    happens at all.  What both do is survive, which is what this checks.
    """
    try:
        fn()
    except Exception:
        pass
    print(label.ljust(30), "survived")


survives("a mark with a wild index",
         lambda: _sre.compile("", 0, [MARK, 1 << 40, SUCCESS], 0, {}, ()).match("a"))
survives("a mark with a big index",
         lambda: _sre.compile("", 0, [MARK, 100000, SUCCESS], 0, {}, ()).match("a"))

# ...and the engine still answers ordinary questions afterwards.
print("still matching", re.match(r"(\w+)\s+(\d+)", "abc 123").groups())
print("still searching", re.search(r"b+", "aabbbcc").group())
print("still splitting", re.split(r"[,;]", "a,b;c"))
print("groups still named", re.match(r"(?P<x>a)(?P<y>b)", "ab").groupdict())
print("done")
