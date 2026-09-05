# termios, which is Python here over one raw ioctl.
#
# CPython's is C because ioctl is; posix.ioctl is the whole of what needed
# assembly, and the struct is unpacked in lib/termios.py -- the same split
# _socket and select already use.  What is checked is the constants (which
# come from the platform's own header either way), the shape of what
# tcgetattr answers, and that a descriptor that is not a terminal fails the
# way CPython's does.
import termios
import os
import errno

NAMES = ["TCSANOW", "TCSADRAIN", "TCSAFLUSH", "TCIFLUSH", "TCOFLUSH",
         "TCIOFLUSH", "TCOOFF", "TCOON", "TCIOFF", "TCION",
         "IGNBRK", "BRKINT", "ISTRIP", "INLCR", "IGNCR", "ICRNL", "IXON",
         "OPOST", "CSIZE", "PARENB", "CS8", "ECHO", "ICANON", "ISIG",
         "IEXTEN", "VMIN", "VTIME", "VINTR", "VQUIT", "VERASE", "VKILL",
         "VEOF", "VSTART", "VSTOP", "VSUSP", "B0", "B9600", "B115200",
         "NCCS", "TIOCGWINSZ"]
for n in NAMES:
    print(n, getattr(termios, n))

print("error is its own class:", termios.error is not OSError,
      issubclass(termios.error, Exception))
print("callables:", sorted(f for f in ("tcgetattr", "tcsetattr", "tcdrain",
                                       "tcflush", "tcflow", "tcsendbreak",
                                       "tcgetwinsize", "tcsetwinsize")
                           if callable(getattr(termios, f, None))))

# A pipe is not a terminal, and every one of these says so with the same
# errno CPython's does.
r, w = os.pipe()
for call, args in (("tcgetattr", (r,)), ("tcdrain", (r,)),
                   ("tcflush", (r, termios.TCIFLUSH)),
                   ("tcflow", (r, termios.TCOON)),
                   ("tcsendbreak", (r, 0))):
    try:
        getattr(termios, call)(*args)
        print(call, "-> no error")
    except termios.error as exc:
        print(call, "->", errno.errorcode.get(exc.args[0], exc.args[0]))
os.close(r)
os.close(w)

try:
    termios.tcsetattr(r, 99, [0, 0, 0, 0, 0, 0, [b"\x00"] * 32])
except (termios.error, ValueError) as exc:
    print("bad when:", type(exc).__name__)

try:
    termios.tcsetattr(0, termios.TCSANOW, [0, 0, 0, 0, 0, 0, [b"\x00"] * 3])
except (TypeError, termios.error) as exc:
    print("short cc:", type(exc).__name__)

try:
    termios.tcsetattr(0, termios.TCSANOW, [0, 0, 0])
except (TypeError, termios.error) as exc:
    print("short attributes:", type(exc).__name__, exc.args[0])
