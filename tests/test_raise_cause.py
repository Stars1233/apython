# Test raise X from Y (__cause__ storage)

# Basic raise from
try:
    try:
        raise ValueError("original")
    except ValueError as e:
        raise RuntimeError("chained") from e
except RuntimeError as e:
    print(type(e).__name__)           # RuntimeError
    print(str(e))                      # chained
    print(type(e.__cause__).__name__) # ValueError
    print(str(e.__cause__))           # original

# raise from None
try:
    try:
        raise ValueError("suppressed")
    except ValueError:
        raise RuntimeError("clean") from None
except RuntimeError as e:
    print(type(e).__name__)           # RuntimeError
    print(e.__cause__)                # None
