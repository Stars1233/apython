# Test round() builtin

# 1-arg: int
print(round(5))        # 5

# 1-arg: float banker's rounding
print(round(1.5))      # 2 (round to even)
print(round(2.5))      # 2 (round to even)
print(round(3.5))      # 4
print(round(0.5))      # 0 (round to even)
print(round(-0.5))     # 0 (round to even)

# 2-arg: float with ndigits
print(round(1.234, 2))  # 1.23
print(round(1.235, 2))  # 1.24  (may vary due to float precision)
print(round(1.0, 0))    # 1.0

# 2-arg: int with positive ndigits (no-op)
print(round(42, 2))     # 42

# 2-arg: int with negative ndigits
print(round(1234, -2))  # 1200
print(round(1250, -2))  # 1200 (banker's: round to even hundred)
