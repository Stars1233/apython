# Operators, from source.
#
# CPython folds constant arithmetic before it ever emits an opcode, so a .pyc
# never exercises `3 * "ab"`, `True & False` or `-7 // 2` at runtime.  Our
# compiler does not fold, which is what turned these up:
#
#   * unary minus was recursing at the BINARY power it shares a table row with,
#     so `-7 // 2` parsed as `-(7 // 2)` -- -3 rather than -4;
#   * str had no sq_repeat, so the reflected `3 * "ab"` fell through to int's
#     nb_multiply, which read the string's length and returned 6;
#   * bool's bitwise operators returned int, so `True & False` was 0;
#   * `n % 2.0` skipped float coercion -- a guard meant to keep `"fmt" % x`
#     away from float division was written to exclude every remainder rather
#     than only a non-numeric left operand -- and dereferenced the float as a
#     PyIntObject;
#   * `assert x` emitted a CALL that CPython does not.
SRC = '''
# Unary binds tighter than a term, looser than a power.
print(-7 // 2, -7 % 3, -2 ** 2, -2 ** 3 ** 2, - -5, +-5, ~-5)
print(-7 * 2, -7 / 2, 2 * -3, 2 ** -1)

# Repetition, both ways round.
print(3 * "ab", "ab" * 3, 3 * [1, 2], [1, 2] * 3)
print(3 * (1,), (1,) * 3, 3 * b"xy", b"xy" * 3)

# bool narrows for bitwise and widens for everything else.
T, F = True, False
print(T & F, T | F, T ^ F)
print(type(T & F).__name__, type(T | F).__name__, type(T ^ F).__name__)
print(T & 1, type(T & 1).__name__, T + T, type(T + T).__name__)

# Mixed int/float arithmetic, including the operators with a sign rule.
n = -7
print(n + 2.0, n - 2.0, n * 2.0, n / 2.0, n // 2.0, n % 3.0)
print(-7.0 // 2, -7.0 % 3, 7 // -2, 7 % -3, divmod(-7, 3), divmod(7, -3))
print("%.2f|%s|%d" % (1.5, 2.5, 3))

# assert, with and without a message.
try:
    assert 1 == 2
except AssertionError as e:
    print("bare:", repr(str(e)))
try:
    assert 1 == 2, "with a message"
except AssertionError as e:
    print("msg:", e)
assert True
assert 1 == 1, "never raised"

# Unary + is __pos__, not a no-op.
class P:
    def __pos__(self):
        return "pos"


print(+P(), +5, +-5)
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)
