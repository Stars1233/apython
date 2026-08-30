# Two things the compiler got wrong that only running whole programs through it
# exposed: a comma inside a subscript, and leaving an except clause early.
#
# `d[1, 2]` subscripts with the tuple `(1, 2)`; there is no n-ary subscript,
# which is why `d[1,]` and `d[(1,)]` are the same expression.
#
# `return` inside an `except` block has to pop the exception state on its way
# out and unbind the name.  Skipping that leaves the exception "being handled"
# for the rest of the process, and the interpreter reports it as unhandled at
# exit -- an error message with no relation to where it came from.
SRC = '''
d = {(1, 2): "a", (1,): "b", 1: "c"}
print(d[1, 2], d[1,], d[1])


class G:
    def __getitem__(self, k):
        return k

    def __setitem__(self, k, v):
        self.last = (k, v)


g = G()
print(g[1, 2], g[1, 2, 3], g[1, ...])
g[1, 2] = 9
print(g.last)
print(list[int], dict[str, int], tuple[int, ...])


def caught(fn):
    try:
        return fn()
    except BaseException as e:
        return "caught " + type(e).__name__


print(caught(lambda: 1 / 0), caught(lambda: "ok"))


def loops(k):
    out = []
    for i in range(3):
        try:
            if k == "b":
                raise ValueError("v")
            if k == "c":
                raise KeyError("k")
            out.append(i)
        except ValueError:
            out.append("VE")
            break
        except KeyError as e:
            out.append("KE" + str(e))
            continue
        finally:
            out.append("fin")
    return out


print(loops("a"))
print(loops("b"))
print(loops("c"))


def nested():
    try:
        raise TypeError("t")
    except TypeError as e:
        try:
            return str(e)
        finally:
            pass


print(nested())
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)
