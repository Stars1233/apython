# __annotations__ and __type_params__ come from the class ITSELF, not its MRO.
#
# CPython answers both from a getset descriptor on `type`, and a data
# descriptor on the metatype wins over anything the class inherits through its
# own MRO.  So a subclass of an annotated class has EMPTY annotations, and the
# empty dict it gets on first read is its own.
#
# Answered at the end of the MRO walk instead, as they were here, a subclass
# reported its BASE's dict -- and because the dict is handed out by reference,
# `Sub.__annotations__['y'] = str` then wrote into `Base.__annotations__`.
# dataclasses, typing.get_type_hints and attrs all read the attribute per class
# and would have seen every base's fields repeated in every subclass.


class Base:
    x: int


class Sub(Base):
    pass


class Own(Base):
    y: str


print(Base.__annotations__, "the class that has them")
print(Sub.__annotations__, "a subclass that declares none")
print(Own.__annotations__, "a subclass that declares its own")

# The dict is the class's own, so writing into it does not reach the base.
Sub.__annotations__["z"] = float
print(Base.__annotations__, "the base is untouched")
print(Sub.__annotations__, "and the subclass kept the write")

# Created on first read and KEPT, so it is the same object every time and it
# shows up in the class's dict.
a = Sub.__annotations__
print(a is Sub.__annotations__, "the same dict on every read")
print("__annotations__" in vars(Sub), "and it lands in the class dict")

# Assignment replaces it.
Sub.__annotations__ = {"replaced": bool}
print(Sub.__annotations__, "assignment replaces it")


# --- __type_params__ --------------------------------------------------------
# PEP 695 puts a tuple on the class that declares the parameters; every other
# class answers an empty tuple, and again not its base's.
class G[T]:
    pass


class GSub(G):
    pass


print(len(G.__type_params__), "a generic class has its parameters")
print(G.__type_params__[0].__name__, "and they are named")
print(GSub.__type_params__, "a subclass of one has none")
print(Base.__type_params__, "and so does an ordinary class")
print(int.__type_params__, "and a builtin")


# --- what a static type says ------------------------------------------------
for t in (int, str, type(None)):
    try:
        print(t.__name__, t.__annotations__, "a static type")
    except AttributeError as e:
        print(t.__name__, "AttributeError:", e)
