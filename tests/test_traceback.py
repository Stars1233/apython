# Test exception __traceback__

# Basic: __traceback__ is not None after catch
try:
    raise ValueError("test")
except ValueError as e:
    tb = e.__traceback__
    print("has tb:", tb is not None)
    print("type:", type(tb).__name__)

# tb_lineno and tb_next attributes exist
try:
    raise TypeError("oops")
except TypeError as e:
    tb = e.__traceback__
    print("tb_next:", tb.tb_next)
    # tb_lineno is an int
    print("lineno type:", type(tb.tb_lineno).__name__)

# Traceback from function call
def throws():
    raise RuntimeError("inner")

try:
    throws()
except RuntimeError as e:
    tb = e.__traceback__
    print("func tb:", tb is not None)

# No traceback on fresh exception (not raised)
e2 = ValueError("not raised")
print("fresh tb:", e2.__traceback__)

print("done")
