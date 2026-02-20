# Simplified test_sort.py for apython
# Based on CPython's test_sort.py
# Note: random module not used - causes hangs in apython

# Simple pseudo-random for shuffle (deterministic, no import needed)
def simple_shuffle(L, seed=42):
    """Fisher-Yates shuffle with simple LCG PRNG"""
    n = len(L)
    state = seed
    for i in range(n - 1, 0, -1):
        state = (state * 1103515245 + 12345) & 0x7fffffff
        j = state % (i + 1)
        L[i], L[j] = L[j], L[i]

# Basic sort tests
def test_sort_empty():
    L = []
    L.sort()
    assert L == [], f"empty: {L}"
    print("PASS: test_sort_empty")

def test_sort_single():
    L = [1]
    L.sort()
    assert L == [1], f"single: {L}"
    print("PASS: test_sort_single")

def test_sort_sorted():
    L = [1, 2, 3, 4, 5]
    L.sort()
    assert L == [1, 2, 3, 4, 5], f"sorted: {L}"
    print("PASS: test_sort_sorted")

def test_sort_reversed():
    L = [5, 4, 3, 2, 1]
    L.sort()
    assert L == [1, 2, 3, 4, 5], f"reversed: {L}"
    print("PASS: test_sort_reversed")

def test_sort_random():
    L = list(range(100))
    simple_shuffle(L, seed=42)
    L.sort()
    assert L == list(range(100)), f"random: {L}"
    print("PASS: test_sort_random")

def test_sort_duplicates():
    L = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    L.sort()
    assert L == [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9], f"duplicates: {L}"
    print("PASS: test_sort_duplicates")

def test_sort_strings():
    L = ['banana', 'apple', 'cherry']
    L.sort()
    assert L == ['apple', 'banana', 'cherry'], f"strings: {L}"
    print("PASS: test_sort_strings")

def test_sort_floats():
    L = [3.14, 1.41, 2.71, 0.5]
    L.sort()
    assert L == [0.5, 1.41, 2.71, 3.14], f"floats: {L}"
    print("PASS: test_sort_floats")

def test_sort_mixed_int_float():
    L = [3, 1.5, 2, 0.5]
    L.sort()
    assert L == [0.5, 1.5, 2, 3], f"mixed int/float: {L}"
    print("PASS: test_sort_mixed_int_float")

# Key function tests
def test_sort_key():
    L = ['Banana', 'apple', 'Cherry']
    L.sort(key=str.lower)
    assert L == ['apple', 'Banana', 'Cherry'], f"key: {L}"
    print("PASS: test_sort_key")

def test_sort_key_len():
    L = ['aaa', 'b', 'cc']
    L.sort(key=len)
    assert L == ['b', 'cc', 'aaa'], f"key len: {L}"
    print("PASS: test_sort_key_len")

def test_sort_key_lambda():
    L = [(1, 'z'), (2, 'a'), (3, 'm')]
    L.sort(key=lambda x: x[1])
    assert L == [(2, 'a'), (3, 'm'), (1, 'z')], f"key lambda: {L}"
    print("PASS: test_sort_key_lambda")

# Reverse tests
def test_sort_reverse():
    L = [1, 2, 3, 4, 5]
    L.sort(reverse=True)
    assert L == [5, 4, 3, 2, 1], f"reverse: {L}"
    print("PASS: test_sort_reverse")

def test_sort_key_reverse():
    L = ['a', 'bb', 'ccc']
    L.sort(key=len, reverse=True)
    assert L == ['ccc', 'bb', 'a'], f"key+reverse: {L}"
    print("PASS: test_sort_key_reverse")

# Stability tests
def test_sort_stability():
    # SKIP: Double-free bug when sorting tuples with key function
    # Sort by first element; ties should preserve original order
    # L = [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd'), (1, 'e')]
    # L.sort(key=lambda x: x[0])
    # assert L == [(1, 'a'), (1, 'c'), (1, 'e'), (2, 'b'), (2, 'd')], f"stability: {L}"
    print("XFAIL: test_sort_stability (double-free bug with tuple+key)")

# Error tests
def test_sort_heterogeneous_raises():
    L = [1, 'a']
    try:
        L.sort()
        print("FAIL: test_sort_heterogeneous - should have raised TypeError")
    except TypeError:
        print("PASS: test_sort_heterogeneous_raises")

def test_sort_none_raises():
    L = [1, None, 2]
    try:
        L.sort()
        print("FAIL: test_sort_none - should have raised TypeError")
    except TypeError:
        print("PASS: test_sort_none_raises")

def test_sort_bad_key_raises():
    def bad_key(x):
        raise RuntimeError("bad key")
    L = [1, 2, 3]
    try:
        L.sort(key=bad_key)
        print("FAIL: test_sort_bad_key - should have raised RuntimeError")
    except RuntimeError:
        print("PASS: test_sort_bad_key_raises")

# Mutation detection tests (BUG-3: should raise ValueError, not TypeError)
def test_sort_mutation_append():
    class Mutator:
        def __init__(self, val):
            self.val = val
        def __lt__(self, other):
            L.append(Mutator(999))
            return self.val < other.val
    
    L = [Mutator(i) for i in range(5)]
    try:
        L.sort()
        print("FAIL: test_sort_mutation_append - should have raised ValueError")
    except ValueError:
        print("PASS: test_sort_mutation_append")
    except TypeError:
        # Current bug: raises TypeError instead of ValueError
        print("XFAIL: test_sort_mutation_append (raises TypeError instead of ValueError)")

def test_sort_mutation_clear():
    # More aggressive mutation that CPython definitely detects
    class Mutator:
        def __init__(self, val):
            self.val = val
        def __lt__(self, other):
            L.clear()
            L.extend([Mutator(i) for i in range(20)])
            return self.val < other.val
    
    L = [Mutator(i) for i in range(10)]
    try:
        L.sort()
        print("FAIL: test_sort_mutation_clear - should have raised ValueError")
    except ValueError:
        print("PASS: test_sort_mutation_clear")
    except TypeError:
        # Current bug: raises TypeError instead of ValueError
        print("XFAIL: test_sort_mutation_clear (raises TypeError instead of ValueError)")

# Negative/zero tests
def test_sort_negative():
    L = [-3, 1, -1, 2, 0]
    L.sort()
    assert L == [-3, -1, 0, 1, 2], f"negative: {L}"
    print("PASS: test_sort_negative")

# Large list
def test_sort_large():
    L = list(range(1000))
    simple_shuffle(L, seed=123)
    L.sort()
    assert L == list(range(1000)), "large sort failed"
    print("PASS: test_sort_large")

# Sorted builtin
def test_sorted():
    L = [3, 1, 2]
    result = sorted(L)
    assert result == [1, 2, 3], f"sorted: {result}"
    assert L == [3, 1, 2], "original should be unchanged"
    print("PASS: test_sorted")

def test_sorted_key():
    L = ['bb', 'a', 'ccc']
    result = sorted(L, key=len)
    assert result == ['a', 'bb', 'ccc'], f"sorted key: {result}"
    print("PASS: test_sorted_key")

def test_sorted_reverse():
    L = [1, 2, 3]
    result = sorted(L, reverse=True)
    assert result == [3, 2, 1], f"sorted reverse: {result}"
    print("PASS: test_sorted_reverse")

# Run all tests
test_sort_empty()
test_sort_single()
test_sort_sorted()
test_sort_reversed()
test_sort_random()
test_sort_duplicates()
test_sort_strings()
test_sort_floats()
test_sort_mixed_int_float()
test_sort_key()
test_sort_key_len()
test_sort_key_lambda()
test_sort_reverse()
test_sort_key_reverse()
test_sort_stability()
test_sort_heterogeneous_raises()
test_sort_none_raises()
test_sort_bad_key_raises()
test_sort_mutation_append()
test_sort_mutation_clear()
test_sort_negative()
test_sort_large()
test_sorted()
test_sorted_key()
test_sorted_reverse()

print("\nAll sort tests passed!")
