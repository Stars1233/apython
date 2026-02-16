class Point:
    __match_args__ = ('x', 'y')
    def __init__(self, x, y):
        self.x = x
        self.y = y

def describe(obj):
    match obj:
        case Point(x=px, y=py):
            print(f"Point({px}, {py})")
        case int():
            print(f"int:{obj}")
        case str():
            print(f"str:{obj}")
        case _:
            print(f"other:{obj}")

describe(Point(3, 4))
describe(42)
describe("hello")
describe([1, 2])
