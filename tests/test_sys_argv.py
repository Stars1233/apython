# sys.argv[0] is the script, not the interpreter.  It used to be the
# interpreter, so every script reading sys.argv[1] for its first argument got
# its own path instead.  (Run from a .pyc, argv[0] is that .pyc; either way it
# names this test and not the binary running it.)
import sys

print("test_sys_argv" in sys.argv[0])
print(not sys.argv[0].endswith("apython"))
print(sys.argv[1:])
print(all(isinstance(a, str) for a in sys.argv))
