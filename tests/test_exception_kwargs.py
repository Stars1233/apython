# AttributeError(name=, obj=) and ImportError(name=, path=).
#
# These are how CPython reports what was being looked up, and the stdlib reads
# both back: importlib populates ModuleNotFoundError.name, and the "did you
# mean" machinery reads AttributeError.name and .obj.  Neither existed here --
# the keywords were silently folded into .args, so `.args` came out one item
# too long and `.name` was an AttributeError of its own.
#
# They are the only builtin exceptions that take keyword arguments.
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
t("AE name/obj", lambda: (lambda e: (e.name, e.obj))(AttributeError("x", name="n", obj=5)))
t("AE args", lambda: AttributeError("x", name="n", obj=5).args)
t("AE plain", lambda: (lambda e: (e.name, e.obj))(AttributeError("x")))
t("AE noargs", lambda: (lambda e: (e.name, e.obj, e.args))(AttributeError()))
t("AE 2 args", lambda: AttributeError("a","b",name="n").args)
t("AE str", lambda: str(AttributeError("x", name="n")))
t("IE name/path", lambda: (lambda e: (e.name, e.path))(ImportError("x", name="n", path="p")))
t("IE args", lambda: ImportError("a", name="n", path="p").args)
t("IE plain", lambda: (lambda e: (e.name, e.path))(ImportError("x")))
t("MNF name", lambda: (lambda e: e.name)(ModuleNotFoundError("x", name="m")))
t("MNF plain", lambda: (lambda e: (e.name, e.path))(ModuleNotFoundError("x")))
t("AE bad kw", lambda: AttributeError("x", foo=1))
t("IE bad kw", lambda: ImportError("x", foo=1))
t("VE kw", lambda: ValueError("x", name="n"))
t("AE subclass", lambda: (lambda C: (lambda e:(e.name,e.obj))(C("x",name="n")))(type('C',(AttributeError,),{})))
def gg():
    try:
        raise AttributeError("m", name="n", obj=7)
    except AttributeError as e:
        return e.name, e.obj, e.args, str(e)
t("raised AE", gg)
t("real missing attr", lambda: (lambda: getattr(object(), 'zzz'))())
def real():
    try:
        object().zzz
    except AttributeError as e:
        return type(e).__name__
t("real AE type", real)
