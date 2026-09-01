# OSError's CPython shape: its attributes, its str(), the .args truncation, and
# the errno-to-subclass remapping.
#
# Before this, OSError kept all three constructor arguments in .args, had no
# .errno / .strerror / .filename at all, printed as the bare args tuple, and
# never became a subclass -- so `except FileNotFoundError` could not work and
# neither could any stdlib code that reads e.errno.

CASES = [
    "OSError()",
    "OSError('boom')",
    "OSError(2, 'No such file or directory')",
    "OSError(2, 'No such file or directory', '/nope')",
    "OSError(17, 'File exists', '/tmp', None, '/tmp2')",
    "OSError(13, 'Permission denied')",
    "OSError(21, 'Is a directory', '/tmp')",
    "OSError(99, 'unknown errno')",
    "OSError(1, 'Operation not permitted')",
    "OSError(11, 'Resource temporarily unavailable')",
    "OSError(32, 'Broken pipe')",
    "OSError(4, 'Interrupted system call')",
    "OSError(110, 'Connection timed out')",
]

# eval() over string literals that live in this file; nothing is read from
# input, and evaluating the source keeps each label and its expression together.
for src in CASES:
    e = eval(src)
    print(src)
    print("  type   :", type(e).__name__)
    print("  str    :", repr(str(e)))
    print("  repr   :", repr(repr(e)))
    print("  args   :", e.args)
    print("  fields :", e.errno, repr(e.strerror), repr(e.filename), repr(e.filename2))

print("--- aliases ---")
print(IOError is OSError, EnvironmentError is OSError)
print(issubclass(FileNotFoundError, OSError), issubclass(FileExistsError, OSError))
print(issubclass(UnicodeTranslateError, UnicodeError))

print("--- catching by subclass ---")
for errno_, name in ((2, "FileNotFoundError"), (17, "FileExistsError"),
                     (13, "PermissionError"), (21, "IsADirectoryError"),
                     (20, "NotADirectoryError"), (3, "ProcessLookupError"),
                     (10, "ChildProcessError"), (110, "TimeoutError")):
    try:
        raise OSError(errno_, "msg", "/f")
    except OSError as e:
        print(errno_, type(e).__name__, type(e).__name__ == name, e.errno, e.filename)

print("--- a subclass constructed directly is not remapped ---")
e = FileNotFoundError(17, "File exists")
print(type(e).__name__, e.errno)

print("--- attributes are writable, as CPython's are ---")
e = OSError(2, "x", "/f")
e.filename = "/other"
print(e.filename, str(e))
