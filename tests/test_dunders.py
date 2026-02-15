# Test user-defined dunder methods

# Custom comparison
class Comparable:
    def __init__(self, val):
        self.val = val
    def __eq__(self, other):
        return self.val == other.val
    def __lt__(self, other):
        return self.val < other.val
    def __repr__(self):
        return "Comparable(" + str(self.val) + ")"

a = Comparable(1)
b = Comparable(2)
c = Comparable(1)
print(a == c)
print(a == b)
print(a < b)
print(b < a)

# Custom arithmetic
class Vec:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)
    def __repr__(self):
        return "Vec(" + str(self.x) + ", " + str(self.y) + ")"

v1 = Vec(1, 2)
v2 = Vec(3, 4)
v3 = v1 + v2
print(v3)

# Custom iterator
class Counter:
    def __init__(self, n):
        self.n = n
        self.i = 0
    def __iter__(self):
        return self
    def __next__(self):
        if self.i >= self.n:
            return None  # can't raise StopIteration yet
        val = self.i
        self.i = self.i + 1
        return val

# Custom container (__getitem__, __setitem__, __contains__, __len__)
class MyList:
    def __init__(self):
        self.data = [0, 0, 0]
    def __getitem__(self, idx):
        return self.data[idx]
    def __setitem__(self, idx, val):
        self.data[idx] = val
    def __len__(self):
        return len(self.data)
    def __contains__(self, val):
        for item in self.data:
            if item == val:
                return True
        return False

ml = MyList()
ml[0] = 10
ml[1] = 20
ml[2] = 30
print(ml[0], ml[1], ml[2])
print(len(ml))
print(20 in ml)
print(99 in ml)

# Custom bool
class Falsy:
    def __bool__(self):
        return False

class Truthy:
    def __bool__(self):
        return True

print(bool(Falsy()))
print(bool(Truthy()))
