import sys
# Test print file= kwarg with stderr
# Only use stderr to avoid stdout/stderr interleaving issues
print("hello stderr", file=sys.stderr)
print("another stderr line", file=sys.stderr)
