# sys.settrace, and the event stream it produces.
#
# The whole test is the stream: bdb, pdb, trace and profile are written
# against these events and nothing else, so an event that fires once too often
# or once too rarely is the bug.  Every line below is compared against
# CPython's own answer.
#
# What it costs when nothing is set is nothing.  DISPATCH already jumps
# through opcode_dispatch_table -- a pointer, not the table -- because `-t`
# needed somewhere to interpose, so turning tracing on is one store and
# turning it off is one store back.
import sys

events = []


def tracer(frame, event, arg):
    events.append((event, frame.f_code.co_name, frame.f_lineno))
    return tracer


def run(fn, *args):
    del events[:]
    sys.settrace(tracer)
    try:
        fn(*args)
    except BaseException as e:
        events.append(("!!", type(e).__name__, str(e)))
    finally:
        sys.settrace(None)
    for e in events:
        print("   ", e)


# --- a loop, which exercises the backward-jump rule ----------------------
def loop(n):
    total = 0
    for i in range(n):
        total += i
    return total


print("loop:")
run(loop, 2)


# --- a while on ONE line: every event here comes from the backward edge ---
def one_line():
    i = 0
    while i < 3: i += 1
    return i


print("one line while:")
run(one_line)


# --- nested calls --------------------------------------------------------
def inner(x):
    return x + 1


def outer(x):
    return inner(x) * 2


print("nested:")
run(outer, 3)


# --- an exception raised and caught inside the traced code ---------------
def caught():
    try:
        1 / 0
    except ZeroDivisionError:
        return "caught"


print("caught:")
run(caught)


# --- an exception that leaves the frame ----------------------------------
def escapes():
    raise ValueError("out")


print("escaping:")
run(escapes)


# --- a generator: a resume is a fresh 'call', a yield is a 'return' ------
def gen(n):
    for i in range(n):
        yield i


def drive():
    return list(gen(2))


print("generator:")
run(drive)


# --- a comprehension keeps a frame of its own, and so gets traced --------
def comp():
    return [x * 2 for x in range(2)]


print("comprehension:")
run(comp)


# --- returning None from the tracer turns local tracing off --------------
def call_only(frame, event, arg):
    events.append((event, frame.f_code.co_name))
    return None                 # no local hook: no line, no return


def quiet():
    a = 1
    b = 2
    return a + b


del events[:]
sys.settrace(call_only)
quiet()
sys.settrace(None)
print("call only:", events)


# --- f_trace_lines = False silences lines and keeps the rest -------------
def no_lines(frame, event, arg):
    events.append((event, frame.f_code.co_name))
    frame.f_trace_lines = False
    return no_lines


del events[:]
sys.settrace(no_lines)
quiet()
sys.settrace(None)
print("no lines:", events)


# --- f_trace_opcodes ------------------------------------------------------
#
# Not compared against CPython 3.12, and DIVERGENCES.md says why: 3.12 accepts
# f_trace_opcodes and then never delivers an 'opcode' event, because PEP 669
# took the mechanism out from under the legacy hook.  apython delivers them,
# which is what 3.11 did and what 3.13 does again.  What IS compared here is
# that the events which are not 'opcode' are the same either way.
def count_opcodes(frame, event, arg):
    if event == "call":
        frame.f_trace_opcodes = True
        frame.f_trace_lines = False
    events.append(event)
    return count_opcodes


def tiny():
    return 1


del events[:]
sys.settrace(count_opcodes)
tiny()
sys.settrace(None)
print("without opcodes:", [e for e in events if e != "opcode"])


# --- gettrace round trip -------------------------------------------------
print("gettrace when off:", sys.gettrace())
sys.settrace(tracer)
print("gettrace when on:", sys.gettrace() is tracer)
sys.settrace(None)
print("gettrace after off:", sys.gettrace())


# --- call_tracing runs with tracing suppressed ---------------------------
seen = []


def note(frame, event, arg):
    seen.append(event)
    return note


def untraced():
    return 42


sys.settrace(note)
before = len(seen)
result = sys.call_tracing(untraced, ())
after = len(seen)
sys.settrace(None)
print("call_tracing result:", result, "events added:", after - before)


# --- a tracer that raises turns tracing off, as CPython's does -----------
def angry(frame, event, arg):
    raise RuntimeError("no")


def victim():
    return 1


try:
    sys.settrace(angry)
    victim()
except RuntimeError as e:
    print("tracer raised:", e)
finally:
    sys.settrace(None)
print("tracing off after the raise:", sys.gettrace())


# --- the frame handed to the tracer is the frame ------------------------
frames = []


def collect(frame, event, arg):
    frames.append(frame)
    return collect


def two_lines():
    a = 1
    return a


sys.settrace(collect)
two_lines()
sys.settrace(None)
print("one frame object throughout:", len(set(map(id, frames))) == 1)
print("its code:", frames[0].f_code.co_name)
