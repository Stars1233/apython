# A metaclass __prepare__ that raises must stop the class from being built.
# A NULL from the call was read as "there is no __prepare__", so the body ran
# anyway, the class was created, and the exception surfaced much later at an
# unrelated instruction.
class Boom(Exception):
    pass


class MetaRaise(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        raise Boom("nope")


built = []
try:
    class C(metaclass=MetaRaise):
        built.append("body ran")

except Boom as e:
    print("caught", e)
else:
    print("NOT RAISED")

print(built, "C" in globals())


# A __prepare__ that returns normally still works, and so does one that
# returns something odd (CPython requires a mapping; ours falls back).
class MetaOk(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        return {"injected": 7}


class D(metaclass=MetaOk):
    x = 1


print(D.injected, D.x)


# A metaclass with no __prepare__ at all is unaffected.
class MetaBare(type):
    pass


class E(metaclass=MetaBare):
    y = 2


print(E.y)


# The exception really is the one raised, not a later mystery.
try:
    class F(metaclass=MetaRaise):
        pass

except Boom:
    print("second one caught too")

print("end")
