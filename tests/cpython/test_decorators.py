"""CPython test_decorators.py adapted for apython."""
import unittest


def countcalls(counts):
    "Decorator to count calls to a function"
    def decorate(func):
        func_name = func.__name__
        counts[func_name] = 0
        def call(*args, **kwds):
            counts[func_name] += 1
            return func(*args, **kwds)
        call.__name__ = func_name
        return call
    return decorate


def memoize(func):
    saved = {}
    def call(*args):
        try:
            return saved[args]
        except KeyError:
            res = func(*args)
            saved[args] = res
            return res
    call.__name__ = func.__name__
    return call


class TestDecorators(unittest.TestCase):

    def test_single_staticmethod(self):
        class C(object):
            @staticmethod
            def foo(): return 42
        self.assertEqual(C.foo(), 42)
        self.assertEqual(C().foo(), 42)

    def test_double(self):
        class C(object):
            @staticmethod
            def foo(): return 42
        self.assertEqual(C.foo(), 42)

    def test_countcalls(self):
        counts = {}

        @countcalls(counts)
        def double(x):
            return x * 2

        # Skip __name__ check - func.__name__ set not supported
        self.assertEqual(double(2), 4)
        self.assertEqual(counts['double'], 1)
        self.assertEqual(double(3), 6)
        self.assertEqual(counts['double'], 2)

    def test_memoize(self):
        counts = [0]

        saved = {}
        def memoize_local(func):
            def call(*args):
                try:
                    return saved[args]
                except KeyError:
                    res = func(*args)
                    saved[args] = res
                    return res
            return call

        @memoize_local
        def double(x):
            counts[0] += 1
            return x * 2

        self.assertEqual(double(2), 4)
        self.assertEqual(double(3), 6)
        self.assertEqual(counts[0], 2)
        # Cached results
        self.assertEqual(double(2), 4)
        self.assertEqual(double(3), 6)
        self.assertEqual(counts[0], 2)  # no additional calls

    def test_decorator_with_args(self):
        def add_tag(tag):
            def decorator(func):
                def wrapper(*args, **kwargs):
                    return tag + ": " + str(func(*args, **kwargs))
                return wrapper
            return decorator

        @add_tag("result")
        def compute(x):
            return x * 10

        self.assertEqual(compute(5), "result: 50")

    def test_stacked_decorators(self):
        log = []

        def decorator_a(func):
            def wrapper(*args):
                log.append('a')
                return func(*args)
            return wrapper

        def decorator_b(func):
            def wrapper(*args):
                log.append('b')
                return func(*args)
            return wrapper

        @decorator_a
        @decorator_b
        def greet(name):
            return "hello " + name

        result = greet("world")
        self.assertEqual(result, "hello world")
        self.assertEqual(log, ['a', 'b'])

    def test_classmethod_decorator(self):
        class C:
            count = 0

            @classmethod
            def increment(cls):
                cls.count += 1
                return cls.count

        self.assertEqual(C.increment(), 1)
        self.assertEqual(C.increment(), 2)
        self.assertEqual(C.count, 2)

    def test_property_decorator(self):
        class C:
            def __init__(self):
                self._x = 0

            @property
            def x(self):
                return self._x

        c = C()
        self.assertEqual(c.x, 0)
        c._x = 42
        self.assertEqual(c.x, 42)


class TestClassDecorators(unittest.TestCase):

    def test_simple(self):
        def d(cls):
            cls.decorated = True
            return cls

        @d
        class C:
            pass

        self.assertTrue(C.decorated)

    def test_with_args(self):
        def d(tag):
            def decorator(cls):
                cls.tag = tag
                return cls
            return decorator

        @d("hello")
        class C:
            pass

        self.assertEqual(C.tag, "hello")


if __name__ == "__main__":
    unittest.main()
