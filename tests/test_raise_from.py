# Test raise...from (A3 fix)
try:
    try:
        raise ValueError("x") from TypeError("y")
    except ValueError as e:
        print("caught:", e)
except Exception as e:
    print("outer:", e)
print("done")
