# Test except* (PEP 654) - ExceptionGroup support

# Test 1: basic match, non-group exception
try:
    raise ValueError("test")
except* ValueError:
    print("caught ValueError")

# Test 2: no match, reraised
try:
    try:
        raise TypeError("wrong")
    except* ValueError:
        print("should not reach")
except TypeError:
    print("reraised TypeError caught")

# Test 3: ExceptionGroup construction + match
try:
    raise ExceptionGroup("eg", [ValueError(1), TypeError(2)])
except* ValueError:
    print("caught ValueError from group")
except* TypeError:
    print("caught TypeError from group")

# Test 4: except* with as binding (non-group)
try:
    raise ValueError("hello")
except* ValueError as e:
    print(type(e).__name__)
    print(len(e.exceptions))

# Test 5: except* with as binding (group)
try:
    raise ExceptionGroup("grp", [ValueError("a"), ValueError("b")])
except* ValueError as e:
    print(type(e).__name__)
    print(len(e.exceptions))

# Test 6: hierarchy (KeyError is-a LookupError)
try:
    raise KeyError("key")
except* LookupError:
    print("caught KeyError via LookupError")

# Test 7: partial group match
try:
    raise ExceptionGroup("mix", [ValueError("v"), TypeError("t"), ValueError("v2")])
except* ValueError as e:
    print("matched", len(e.exceptions), "ValueErrors")
except* TypeError as e:
    print("matched", len(e.exceptions), "TypeErrors")

# Test 8: BaseExceptionGroup constructor
eg = BaseExceptionGroup("bg", [KeyboardInterrupt()])
print(type(eg).__name__)
print(eg.message)
print(len(eg.exceptions))

# Test 9: empty group rejection
try:
    ExceptionGroup("empty", [])
except ValueError:
    print("empty group rejected")

# Test 10: except* with no match at all (naked re-raise)
try:
    try:
        raise ExceptionGroup("eg", [RuntimeError("r")])
    except* ValueError:
        print("should not reach")
except ExceptionGroup:
    print("unmatched group reraised")

# Test 11: full match consumes entire group
try:
    raise ExceptionGroup("eg", [ValueError("a"), ValueError("b")])
except* ValueError as e:
    print("full match", len(e.exceptions))
print("no reraise after full match")

# Test 12: except* with non-exception-group BaseException
try:
    try:
        raise KeyboardInterrupt()
    except* KeyboardInterrupt:
        print("caught KeyboardInterrupt via except*")
except BaseException:
    print("fallback")

print("all tests passed")
