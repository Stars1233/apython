# The subclass-first rule, on all four of the axes it has to hold.
#
# Python's rule: when the RIGHT operand's type is a proper subclass of the
# left's and OVERRIDES the reflected operation, the right operand goes first.
# It is one rule, but this interpreter reaches it through four different
# functions, and it was implemented in one of them.
#
#   COMPARE_OP / BINARY_OP        op_binary_op, which had it
#   sum(), pow(), math.prod, ...  obj_binary_op, which did not
#   `in`, list.count, sorted, ... obj_richcompare_bool, which did not
#   a builtin's subclass          slots.asm, which installed no reflected slot
#
# The last is the one that looks least like the others: for a subclass of a
# builtin the inherited slot ANSWERS, so nothing ever reaches the reflected
# dunder unless the subclass has a slot of its own.


class MyInt(int):
    def __radd__(self, other):
        return "MyInt.__radd__"

    def __rmul__(self, other):
        return "MyInt.__rmul__"


class MyFloat(float):
    def __radd__(self, other):
        return "MyFloat.__radd__"

    def __rsub__(self, other):
        return "MyFloat.__rsub__"


class MyStr(str):
    def __radd__(self, other):
        return "MyStr.__radd__"


# --- the operator, which already worked ---------------------------------
print("1 + MyInt(2)   =", 1 + MyInt(2))
print("1 + MyFloat(2) =", 1 + MyFloat(2))
print("1.0 + MyFloat(2) =", 1.0 + MyFloat(2))
print("2 * MyInt(3)   =", 2 * MyInt(3))
print("'a' + MyStr('b') =", "a" + MyStr("b"))
print("1.0 - MyFloat(2) =", 1.0 - MyFloat(2))

# --- the same operation reached through a builtin ------------------------
print("sum ints  =", sum([1, 2, MyInt(3)]))
print("sum floats=", sum([1.0, MyFloat(2)]))
print("pow       =", pow(MyInt(2), 3))


class PowRight(int):
    def __rpow__(self, other, mod=None):
        return "PowRight.__rpow__"


print("pow refl  =", pow(2, PowRight(3)))

try:
    import math
    print("math.prod =", math.prod([2, MyInt(3)]))
except Exception as e:
    print("math.prod !!", type(e).__name__, e)


# --- comparison, which every container asks ------------------------------
class SK(str):
    def __eq__(self, other):
        return False

    def __ne__(self, other):
        return True

    def __hash__(self):
        return str.__hash__(self)


k = SK("hello")
print("expression  :", "hello" == k, k == "hello")
print("in list     :", k in ["hello"])
print("in tuple    :", k in ("hello",))
print("in set      :", k in {"hello"})
print("in dict     :", k in {"hello": 1})
print("list.count  :", ["hello"].count(k))
print("list.index  :", ["hello", k].index(k))
print("tuple.count :", ("hello",).count(k))


class SI(int):
    def __lt__(self, other):
        return True

    def __gt__(self, other):
        return False


print("min         :", min(5, SI(9)))
print("max         :", max(5, SI(9)))


# --- sorted() must not depend on the order it was already in -------------
def sort_of(seq):
    try:
        return sorted(seq)
    except TypeError as e:
        return "TypeError: " + str(e)


print("['a', None] :", sort_of(["a", None]))
print("[None, 'a'] :", sort_of([None, "a"]))
print("[1, None]   :", sort_of([1, None]))
print("[None, 1]   :", sort_of([None, 1]))
print("[1, 'a']    :", sort_of([1, "a"]))
print("['a', 1]    :", sort_of(["a", 1]))
print("[1.5, None] :", sort_of([1.5, None]))
print("[None, 1.5] :", sort_of([None, 1.5]))
print("[{}, {}]    :", sort_of([{}, {}]))

# And a list that IS sortable still sorts.
print("ordinary    :", sorted([3, 1, 2]), sorted(["b", "a"]), sorted([1.5, 0.5]))
print("stable      :", sorted([(1, "b"), (1, "a"), (0, "c")], key=lambda t: t[0]))


# --- a subclass that does NOT override is not preferred ------------------
class Plain(int):
    pass


print("no override :", 1 + Plain(2), type(1 + Plain(2)).__name__)


# --- NotImplemented from the reflected call hands back -------------------
class Declines(int):
    def __radd__(self, other):
        return NotImplemented


print("declines    :", 1 + Declines(2))


# --- and a reflected call that raises propagates -------------------------
class Angry(int):
    def __radd__(self, other):
        raise ValueError("no")


try:
    1 + Angry(2)
except ValueError as e:
    print("raises      :", e)

try:
    sum([1, Angry(2)])
except ValueError as e:
    print("raises (sum):", e)
