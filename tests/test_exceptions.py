# Test basic exception handling
try:
    raise ValueError("test error")
except ValueError:
    print("caught ValueError")

# Test exception with no message
try:
    raise TypeError()
except TypeError:
    print("caught TypeError")

# Test nested try/except
try:
    try:
        raise KeyError("inner")
    except KeyError:
        print("caught inner KeyError")
    print("after inner try")
except ValueError:
    print("should not reach here")

# Test bare raise (reraise)
try:
    try:
        raise RuntimeError("original")
    except RuntimeError:
        print("caught, reraising")
        raise
except RuntimeError:
    print("caught reraised RuntimeError")

# Test that code after try/except runs normally
print("done")
