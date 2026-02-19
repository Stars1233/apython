# Test __del__ destructor

class Foo:
    def __del__(self):
        print("Foo deleted")

def test_basic():
    f = Foo()
    # f goes out of scope → __del__ fires

test_basic()

# Test __del__ with multiple refs
class Bar:
    def __init__(self, name):
        self.name = name
    def __del__(self):
        print(self.name, "deleted")

def test_multi_ref():
    b = Bar("b1")
    # b goes out of scope → __del__ fires

test_multi_ref()

# Test no __del__ on class without it
class Plain:
    pass

def test_plain():
    p = Plain()

test_plain()
print("plain done")

# Test __del__ with inheritance
class Base:
    def __del__(self):
        print("base del")

class Child(Base):
    pass

def test_inherit():
    c = Child()

test_inherit()
