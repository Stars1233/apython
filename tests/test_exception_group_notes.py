# __notes__ on an exception group, and what split() does with one.
#
# CPython shallow-copies __notes__ onto each part of a split when it is a
# SEQUENCE, and copies nothing at all when it is not -- the parts simply have
# no __notes__ attribute.  eg_derive reads the attribute with obj_getattr_opt,
# which answers a VALUE, and then released it with obj_decref, which
# dereferences whatever it is handed.  A list survives that; `eg.__notes__ = 5`
# is a refcount decrement at address 5.
#
# That is CPython's own test_exception_group.test_split_does_not_copy_non_
# sequence_notes, which segfaulted here.

# The ordinary case: add_note, then split, and both halves carry the note.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.add_note("first")
eg.add_note("second")
match, rest = eg.split(ValueError)
print("notes on match:", match.__notes__)
print("notes on rest:", rest.__notes__)
print("shallow copy, not shared:", match.__notes__ is not eg.__notes__)

# A non-sequence __notes__ is not copied, and asking for it is an
# AttributeError rather than a crash.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.__notes__ = 123
match, rest = eg.split(TypeError)
print("int notes, match has notes:", hasattr(match, "__notes__"))
print("int notes, rest has notes:", hasattr(rest, "__notes__"))
print("original untouched:", eg.__notes__)

# The same for the other immediates obj_getattr_opt can hand back NaN-boxed,
# and for None, which is a pointer but not a sequence.
for value in (0, -1, 2**50, -(2**50), 1.5, -0.0, float("inf"), True, None):
    eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
    eg.__notes__ = value
    match, rest = eg.split(ValueError)
    print(
        "notes=%-6r copied:" % (value,),
        hasattr(match, "__notes__"),
        hasattr(rest, "__notes__"),
        "orig:",
        repr(eg.__notes__),
    )

# A boxed int -- outside the immediate range, so a real object -- takes the
# same path and must not be over-released either.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.__notes__ = 10**30
match, rest = eg.split(ValueError)
print("big int notes copied:", hasattr(match, "__notes__"), "orig:", eg.__notes__)

# A str is a sequence, so CPython copies it.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.__notes__ = "not a list"
match, rest = eg.split(ValueError)
print("str notes:", repr(getattr(match, "__notes__", None)))

# A tuple is a sequence too.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.__notes__ = ("a", "b")
match, rest = eg.split(ValueError)
print("tuple notes:", getattr(match, "__notes__", None))

# subgroup() and derive() reach the same copy.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.add_note("kept")
sub = eg.subgroup(ValueError)
print("subgroup notes:", sub.__notes__)

eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
eg.__notes__ = 7
sub = eg.subgroup(ValueError)
print("subgroup int notes copied:", hasattr(sub, "__notes__"))

# No __notes__ at all is the common case and must stay clean.
eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
match, rest = eg.split(ValueError)
print("absent notes:", hasattr(match, "__notes__"), hasattr(rest, "__notes__"))

# Repeat the immediate case enough times that an over-release of a shared
# immediate would show as something other than luck.
for _ in range(200):
    eg = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
    eg.__notes__ = 5
    eg.split(ValueError)
print("loop survived")
print("done")
