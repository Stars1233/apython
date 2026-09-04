# range's own protocol.
#
# range had almost none of it.  `range(3) == range(3)` was False -- with no
# tp_richcompare the comparison fell back to identity -- and
# `{range(3), range(0, 3, 1)}` held two elements where CPython holds one.
# `r.start`, `r.index(x)` and `r.count(x)` were all AttributeError, and so was
# `range.__len__`: range_obj_type had no tp_dict at all, so every one of these
# lived only in a slot where nothing could ask for it by name.
#
# CPython's equality is not field-by-field: two ranges are equal when they
# generate the same sequence, so all empty ranges are equal, a one-element
# range ignores its step, and only from two elements up does the step count.
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
r = range(2, 12, 3)
t("range index", lambda: r.index(5))
t("range index miss", lambda: r.index(6))
t("range count", lambda: r.count(5))
t("range count 0", lambda: r.count(6))
t("range start", lambda: (r.start, r.stop, r.step))
t("range eq", lambda: range(3) == range(3))
t("range eq step", lambda: range(0,3,1) == range(3))
t("range ne", lambda: range(3) == range(4))
t("range hash", lambda: hash(range(3)) == hash(range(0,3)))
t("range in set", lambda: len({range(3), range(0,3,1)}))
t("range dunder", lambda: [n for n in ('__eq__','__hash__','__len__','__getitem__','__iter__','__reversed__','__contains__','index','count','start','stop','step') if hasattr(range, n)])
t("range bool", lambda: (bool(range(0)), bool(range(1))))
t("range repr", lambda: (repr(range(3)), repr(range(1,5,2))))
t("range neg idx", lambda: r[-1])
t("range slice", lambda: r[1:3])
t("range contains", lambda: (5 in r, 6 in r))
t("range reversed", lambda: list(reversed(range(3))))
t("range empty eq", lambda: range(0) == range(5,3))
