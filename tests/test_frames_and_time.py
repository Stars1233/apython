# sys._getframe, and the time module's calendar half.
#
# A PyFrame is pooled and recycled, so sys._getframe cannot hand one back:
# what it returns is a snapshot, and the chain of them is built when it is
# called.  Nothing maintained a frame chain at all before this --
# PyFrame.prev_frame was written once, as zero, and never read.
#
# time had time() and sleep() and nothing that knew what a date is, which is
# where logging, hashlib and random all stopped.  localtime, gmtime, mktime
# and strftime are libc's; struct_time and the conversion either way are not.
import sys
import time


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


# --- sys._getframe
def inner():
    f = sys._getframe(0)
    return (f.f_code.co_name,
            f.f_back.f_code.co_name,
            f.f_back.f_back.f_code.co_name,
            f.f_globals.get("__name__"),
            type(f.f_lineno).__name__,
            sys._getframe(1).f_code.co_name)


def outer():
    return inner()


show("chain", outer)
show("module level", lambda: sys._getframe(0).f_code.co_name)
show("modulename", lambda: sys._getframemodulename(0))
show("too deep", lambda: sys._getframe(10000))
# A negative depth is depth 0, not an error.  The repr carries an address,
# so only the code it names is compared.
show("negative", lambda: sys._getframe(-1).f_code.co_name)


def survives():
    # The snapshot outlives the frames it was taken from, which is the whole
    # point of copying rather than holding a PyFrame*.
    def a():
        return sys._getframe(0)
    fr = a()
    return fr.f_code.co_name, fr.f_back.f_code.co_name


show("outlives", survives)
show("has locals", lambda: isinstance(sys._getframe(0).f_locals, dict))
show("no such attr", lambda: getattr(sys._getframe(0), "zzz", "absent"))

# --- sys.exc_info
show("exc_info clear", lambda: sys.exc_info())


def during_handler():
    try:
        raise ValueError("x")
    except ValueError:
        t, v, tb = sys.exc_info()
        return t.__name__, str(v), tb is not None


show("exc_info handling", during_handler)

# --- time, all in UTC so the answers do not depend on the machine
_t = time.gmtime(0)
show("gmtime tuple", lambda: tuple(_t))
show("gmtime fields", lambda: (_t.tm_year, _t.tm_mon, _t.tm_mday, _t.tm_hour,
                               _t.tm_min, _t.tm_sec, _t.tm_wday, _t.tm_yday,
                               _t.tm_isdst))
show("gmtime named-only", lambda: (_t.tm_zone, _t.tm_gmtoff))
show("type", lambda: type(_t).__name__)
show("len", lambda: len(_t))
show("repr", lambda: repr(_t))
show("indexable", lambda: (_t[0], _t[8]))
show("gmtime later", lambda: tuple(time.gmtime(1000000000)))
show("gmtime negative", lambda: tuple(time.gmtime(-1)))
show("strftime", lambda: time.strftime("%Y-%m-%d %H:%M:%S", _t))
show("strftime names", lambda: time.strftime("%a %A %b %B", _t))
show("strftime numbers", lambda: time.strftime("%j %U %w %y %p", _t))
show("strftime percent", lambda: time.strftime("100%%", _t))
show("asctime", lambda: time.asctime(_t))
show("mktime round trip",
     lambda: tuple(time.localtime(time.mktime(time.localtime(1000000000)))))
show("localtime is a struct_time",
     lambda: type(time.localtime(0)).__name__)
show("strftime bad type", lambda: time.strftime(5, _t))
show("strftime short tuple", lambda: time.strftime("%Y", (1, 2, 3)))
show("mktime bad", lambda: time.mktime(5))
show("gmtime of now is a struct_time",
     lambda: type(time.gmtime()).__name__)

print("done")
