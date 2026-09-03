# slice.indices(length), and slice's type dict.
#
# Only the internal helper behind the subscript operators existed, so
# `slice(1, 10, 2).indices(20)` was an AttributeError -- and it is how
# anything implementing __getitem__ for slices in Python resolves one.
# slice answered start/stop/step through its tp_getattr and had no tp_dict,
# so `hasattr(slice, "start")` was False and they were missing from dir().
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
t("indices", lambda: slice(1,10,2).indices(20))
t("indices neg", lambda: slice(None,None,-1).indices(5))
t("indices short", lambda: slice(1,10,2).indices(3))
t("indices zero step", lambda: slice(1,2,0).indices(5))
t("indices neg len", lambda: slice(1,2).indices(-1))
t("indices none", lambda: slice(None).indices(4))
t("indices negstart", lambda: slice(-2,None).indices(5))
t("indices arity", lambda: slice(1,2).indices())
t("indices bad", lambda: slice(1,2).indices("x"))
t("attrs", lambda: [n for n in ('indices','start','stop','step') if hasattr(slice,n)])
t("inst attrs", lambda: (slice(1,2,3).start, slice(1,2,3).stop, slice(1,2,3).step))
t("dir has", lambda: all(n in dir(slice(1,2)) for n in ('indices','start','stop','step')))
t("slice getattr miss", lambda: getattr(slice(1,2),'zz','d'))
t("__index__ arg", lambda: (lambda I: slice(1,10,2).indices(I()))(type('I',(object,),{'__index__':lambda s:20})))
