# print() writes to the object it is given, not to a file descriptor.
#
# builtin_print kept a bare fd: it defaulted to 1 and never looked at
# sys.stdout, and the file= arm read PyFileObject.file_fd off whatever pointer
# it was handed, with no type check.  So print(x, file=open(...)) was silently
# lost, print(x, file=io.StringIO()) was silently lost, reassigning sys.stdout
# did nothing, and print(x, file=[1, 2, 3]) read a list's ob_size as a file
# descriptor and did not even raise.
#
# tests/test_print_file.py only ever printed to sys.stderr, which is a real
# file object with fd 2, so it could not see any of this.
#
# Everything that captures output goes through one of these: doctest, pdb,
# unittest -b, contextlib.redirect_stdout, code.InteractiveConsole.

import io
import sys


class Recorder:
    """The minimum print() is allowed to require: a write method."""

    def __init__(self):
        self.chunks = []
        self.flushed = 0

    def write(self, text):
        self.chunks.append(text)
        return len(text)

    def flush(self):
        self.flushed += 1


# --- file= with a Python object -------------------------------------------
rec = Recorder()
print("to a recorder", file=rec)
print("".join(rec.chunks) == "to a recorder\n", "recorder got the text")

rec = Recorder()
print("a", "b", sep="-", end="!", file=rec)
print("".join(rec.chunks) == "a-b!", "sep and end reach the object")

rec = Recorder()
print("flushed", file=rec, flush=True)
print(rec.flushed == 1, "flush=True calls the object's flush")

rec = Recorder()
print(file=rec)
print("".join(rec.chunks) == "\n", "a bare print() reaches the object")

# --- file= with a StringIO -------------------------------------------------
buf = io.StringIO()
print("into a StringIO", file=buf)
print(buf.getvalue() == "into a StringIO\n", "StringIO got the text")

# --- file=None means stdout, as CPython documents --------------------------
out = io.StringIO()
saved = sys.stdout
sys.stdout = out
print("none means stdout", file=None)
sys.stdout = saved
print(out.getvalue() == "none means stdout\n", "file=None follows sys.stdout")

# --- reassigning sys.stdout ------------------------------------------------
rec = Recorder()
saved = sys.stdout
sys.stdout = rec
print("captured")
sys.stdout = saved
print("".join(rec.chunks) == "captured\n", "print follows a reassigned sys.stdout")

# --- an object with no write is a TypeError, not silence -------------------
try:
    print("nowhere", file=[1, 2, 3])
    print(False, "a list as file= must raise")
except AttributeError:
    print(True, "a list as file= raises AttributeError")

try:
    print("nowhere", file=42)
    print(False, "an int as file= must raise")
except AttributeError:
    print(True, "an int as file= raises AttributeError")


# --- a write that raises propagates ----------------------------------------
class Angry:
    def write(self, text):
        raise ValueError("no")


try:
    print("x", file=Angry())
    print(False, "a raising write must propagate")
except ValueError:
    print(True, "a raising write propagates")


# --- a real file on disk ---------------------------------------------------
import os

# A fixed directory rather than tempfile, which this tree's lib/ does not
# ship; the pid keeps two concurrent runs of the suite apart.
path = "/tmp/apython_print_probe_%d.txt" % os.getpid()
with open(path, "w") as fh:
    print("to a real file", file=fh)
with open(path) as fh:
    print(fh.read() == "to a real file\n", "a real file got the text")
os.unlink(path)

# --- redirect_stdout, which is the stdlib's own use of all of the above ----
import contextlib

cap = io.StringIO()
with contextlib.redirect_stdout(cap):
    print("redirected")
print(cap.getvalue() == "redirected\n", "redirect_stdout captures print")


# --- what print() requires of a sink, and when it says so ------------------
#
# CPython's print asks for `write` and, under flush=True, for `flush` -- and
# neither is optional once asked for.  A sink with no flush was treated here as
# "a flush that has already happened", so `print(x, file=w, flush=True)`
# against a write-only object produced no error where CPython raises
# AttributeError.  And a lookup that RAISES is not a lookup that came up empty:
# obj_getattr_opt answers 0 for both, so a `write` or `flush` property that
# raised was reported as a missing attribute -- or, worse, swallowed, leaving
# the real exception pending to surface at the next opcode.
class WriteOnly:
    def __init__(self):
        self.chunks = []

    def write(self, text):
        self.chunks.append(text)
        return len(text)


class RaisingFlush:
    def write(self, text):
        return len(text)

    @property
    def flush(self):
        raise ValueError("looking up flush raised")


class RaisingWrite:
    @property
    def write(self):
        raise ValueError("looking up write raised")


w = WriteOnly()
print(w.write("") == 0, "a write-only sink writes")
try:
    print("x", file=w, flush=True)
    print("NO ERROR for flush=True against a sink with no flush")
except AttributeError as e:
    print("AttributeError:", e)

# Without flush=True it is never asked for, so this one is fine.
w = WriteOnly()
print("y", file=w)
print("".join(w.chunks) == "y\n", "and no flush is asked for without it")

try:
    print("x", file=RaisingFlush(), flush=True)
    print("NO ERROR for a flush lookup that raises")
except ValueError as e:
    print("ValueError:", e)

try:
    print("x", file=RaisingWrite())
    print("NO ERROR for a write lookup that raises")
except ValueError as e:
    print("ValueError:", e)
