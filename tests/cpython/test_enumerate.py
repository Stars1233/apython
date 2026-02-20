# Simplified test_enumerate.py for apython
# Based on CPython's test_enumerate.py

# Test classes for different iteration protocols
class G:
    'Sequence using __getitem__'
    def __init__(self, seqn):
        self.seqn = seqn
    def __getitem__(self, i):
        return self.seqn[i]

class I:
    'Sequence using iterator protocol'
    def __init__(self, seqn):
        self.seqn = seqn
        self.i = 0
    def __iter__(self):
        return self
    def __next__(self):
        if self.i >= len(self.seqn):
            raise StopIteration
        v = self.seqn[self.i]
        self.i += 1
        return v

class Ig:
    'Sequence using iterator protocol defined with a generator'
    def __init__(self, seqn):
        self.seqn = seqn
    def __iter__(self):
        for val in self.seqn:
            yield val

# Basic functionality tests
def test_basic():
    seq = 'abc'
    expected = [(0, 'a'), (1, 'b'), (2, 'c')]
    result = list(enumerate(seq))
    assert result == expected, f"basic: {result} != {expected}"
    print("PASS: test_basic")

def test_empty():
    result = list(enumerate(''))
    assert result == [], f"empty: {result} != []"
    print("PASS: test_empty")

def test_iter_is_self():
    e = enumerate('abc')
    assert iter(e) is e, "iter(enumerate) should be enumerate"
    print("PASS: test_iter_is_self")

def test_type():
    assert type(enumerate('abc')) is enumerate, "type should be enumerate"
    print("PASS: test_type")

def test_getitem_seq():
    seq = 'abc'
    expected = [(0, 'a'), (1, 'b'), (2, 'c')]
    result = list(enumerate(G(seq)))
    assert result == expected, f"getitem: {result} != {expected}"
    print("PASS: test_getitem_seq")

def test_iterator_seq():
    seq = 'abc'
    expected = [(0, 'a'), (1, 'b'), (2, 'c')]
    result = list(enumerate(I(seq)))
    assert result == expected, f"iterator: {result} != {expected}"
    print("PASS: test_iterator_seq")

def test_generator_seq():
    seq = 'abc'
    expected = [(0, 'a'), (1, 'b'), (2, 'c')]
    result = list(enumerate(Ig(seq)))
    assert result == expected, f"generator: {result} != {expected}"
    print("PASS: test_generator_seq")

def test_start():
    seq = 'abc'
    expected = [(11, 'a'), (12, 'b'), (13, 'c')]
    result = list(enumerate(seq, 11))
    assert result == expected, f"start: {result} != {expected}"
    print("PASS: test_start")

def test_start_kwarg():
    seq = 'abc'
    expected = [(11, 'a'), (12, 'b'), (13, 'c')]
    result = list(enumerate(seq, start=11))
    assert result == expected, f"start kwarg: {result} != {expected}"
    print("PASS: test_start_kwarg")

def test_iterable_kwarg():
    seq = 'abc'
    expected = [(0, 'a'), (1, 'b'), (2, 'c')]
    result = list(enumerate(iterable=Ig(seq)))
    assert result == expected, f"iterable kwarg: {result} != {expected}"
    print("PASS: test_iterable_kwarg")

def test_both_kwargs():
    seq = 'abc'
    expected = [(5, 'a'), (6, 'b'), (7, 'c')]
    result = list(enumerate(iterable=Ig(seq), start=5))
    assert result == expected, f"both kwargs: {result} != {expected}"
    # Also test reversed order
    result2 = list(enumerate(start=5, iterable=Ig(seq)))
    assert result2 == expected, f"both kwargs reversed: {result2} != {expected}"
    print("PASS: test_both_kwargs")

def test_negative_start():
    result = list(enumerate('ab', -5))
    expected = [(-5, 'a'), (-4, 'b')]
    assert result == expected, f"negative start: {result} != {expected}"
    print("PASS: test_negative_start")

def test_tuple_input():
    result = list(enumerate((10, 20, 30)))
    expected = [(0, 10), (1, 20), (2, 30)]
    assert result == expected, f"tuple: {result} != {expected}"
    print("PASS: test_tuple_input")

def test_list_input():
    result = list(enumerate([10, 20, 30]))
    expected = [(0, 10), (1, 20), (2, 30)]
    assert result == expected, f"list: {result} != {expected}"
    print("PASS: test_list_input")

def test_range_input():
    result = list(enumerate(range(3)))
    expected = [(0, 0), (1, 1), (2, 2)]
    assert result == expected, f"range: {result} != {expected}"
    print("PASS: test_range_input")

def test_large_start():
    # Test with a large positive start value
    result = list(enumerate('a', 1000000))
    expected = [(1000000, 'a')]
    assert result == expected, f"large start: {result} != {expected}"
    print("PASS: test_large_start")

def test_noniterable_raises():
    try:
        enumerate(42)
        print("FAIL: test_noniterable_raises - should have raised TypeError")
    except TypeError:
        print("PASS: test_noniterable_raises")

def test_no_args_raises():
    try:
        enumerate()
        print("FAIL: test_no_args_raises - should have raised TypeError")
    except TypeError:
        print("PASS: test_no_args_raises")

def test_too_many_args_raises():
    try:
        enumerate('abc', 1, 2)
        print("FAIL: test_too_many_args_raises - should have raised TypeError")
    except TypeError:
        print("PASS: test_too_many_args_raises")

def test_bad_start_type_raises():
    try:
        enumerate('abc', 'x')
        print("FAIL: test_bad_start_type_raises - should have raised TypeError")
    except TypeError:
        print("PASS: test_bad_start_type_raises")

# Run all tests
test_basic()
test_empty()
test_iter_is_self()
test_type()
test_getitem_seq()
test_iterator_seq()
test_generator_seq()
test_start()
test_start_kwarg()
test_iterable_kwarg()
test_both_kwargs()
test_negative_start()
test_tuple_input()
test_list_input()
test_range_input()
test_large_start()
test_noniterable_raises()
test_no_args_raises()
test_too_many_args_raises()
test_bad_start_type_raises()

print("\nAll enumerate tests passed!")
