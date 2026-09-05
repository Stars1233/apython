# itertools - adapted from CPython for apython

class chain:
    """chain(*iterables) -- each iterable in turn, as one iterator.

    This was a native builtin, which put the bare name `chain` in
    builtins where CPython has no such name.  In Python it is also a
    class rather than a function, so `from_iterable` has somewhere to
    live and the repr reads as CPython's.
    """

    def __init__(self, *iterables):
        self._source = iter(iterables)
        self._current = None

    @classmethod
    def from_iterable(cls, iterable):
        self = cls.__new__(cls)
        self._source = iter(iterable)
        self._current = None
        return self

    def __iter__(self):
        return self

    def __next__(self):
        while True:
            if self._current is not None:
                try:
                    return next(self._current)
                except StopIteration:
                    self._current = None
            self._current = iter(next(self._source))


def islice(iterable, *args):
    """islice(iterable, stop) or islice(iterable, start, stop[, step])"""
    if len(args) == 1:
        start, stop, step = 0, args[0], 1
    elif len(args) == 2:
        start, stop, step = args[0], args[1], 1
    elif len(args) == 3:
        start, stop, step = args
    else:
        raise TypeError("islice expected 2-4 arguments")

    it = iter(iterable)
    # Skip start elements
    for i in range(start):
        try:
            next(it)
        except StopIteration:
            return

    count = 0
    for i, item in enumerate(it):
        if stop is not None and start + i >= stop:
            return
        if i % step == 0:
            yield item


def count(start=0, step=1):
    """count(start=0, step=1) --> count object
    Return a count object whose .__next__() method returns consecutive values."""
    n = start
    while True:
        yield n
        n += step


def repeat(obj, times=None):
    """repeat(object [,times]) -> create an iterator which returns the object
    for the specified number of times."""
    if times is None:
        while True:
            yield obj
    else:
        for i in range(times):
            yield obj


def cycle(iterable):
    """cycle(iterable) --> cycle object
    Return elements from the iterable until it is exhausted.
    Then repeat the sequence indefinitely."""
    saved = []
    for element in iterable:
        yield element
        saved.append(element)
    while saved:
        for element in saved:
            yield element


def accumulate(iterable, func=None, initial=None):
    """accumulate(iterable[, func, initial]) --> accumulate object"""
    it = iter(iterable)
    if initial is not None:
        total = initial
    else:
        try:
            total = next(it)
        except StopIteration:
            return
    yield total
    for element in it:
        if func is None:
            total = total + element
        else:
            total = func(total, element)
        yield total


def starmap(function, iterable):
    """starmap(function, iterable) --> starmap object"""
    for args in iterable:
        yield function(*args)


def product(*iterables, repeat=1):
    """product(*iterables, repeat=1) --> product object"""
    pools = [list(pool) for pool in iterables] * repeat
    result = [[]]
    for pool in pools:
        result = [x + [y] for x in result for y in pool]
    for prod in result:
        yield tuple(prod)


def takewhile(predicate, iterable):
    """takewhile(predicate, iterable) --> takewhile object"""
    for x in iterable:
        if predicate(x):
            yield x
        else:
            break


def dropwhile(predicate, iterable):
    """dropwhile(predicate, iterable) --> dropwhile object"""
    it = iter(iterable)
    for x in it:
        if not predicate(x):
            yield x
            break
    for x in it:
        yield x


def filterfalse(predicate, iterable):
    """filterfalse(predicate, iterable) --> filterfalse object"""
    if predicate is None:
        predicate = bool
    for x in iterable:
        if not predicate(x):
            yield x


def zip_longest(*iterables, fillvalue=None):
    """zip_longest(*iterables, fillvalue=None) --> zip_longest object"""
    iterators = [iter(it) for it in iterables]
    if not iterators:
        return
    active = len(iterators)
    while True:
        values = []
        for i, it in enumerate(iterators):
            if it is None:
                values.append(fillvalue)
                continue
            try:
                values.append(next(it))
            except StopIteration:
                active = active - 1
                if not active:
                    return
                iterators[i] = None
                values.append(fillvalue)
        yield tuple(values)


def pairwise(iterable):
    """pairwise(iterable) --> pairwise object"""
    it = iter(iterable)
    try:
        prev = next(it)
    except StopIteration:
        return
    for cur in it:
        yield (prev, cur)
        prev = cur


def permutations(iterable, r=None):
    """permutations(iterable, r=None) --> permutations object"""
    pool = tuple(iterable)
    n = len(pool)
    if r is None:
        r = n
    if r > n or r < 0:
        return
    indices = list(range(n))
    cycles = list(range(n, n - r, -1))
    yield tuple(pool[i] for i in indices[:r])
    while n:
        for i in range(r - 1, -1, -1):
            cycles[i] = cycles[i] - 1
            if cycles[i] == 0:
                indices[i:] = indices[i + 1:] + indices[i:i + 1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield tuple(pool[k] for k in indices[:r])
                break
        else:
            return


def combinations(iterable, r):
    """combinations(iterable, r) --> combinations object"""
    pool = tuple(iterable)
    n = len(pool)
    if r < 0:
        raise ValueError("r must be non-negative")
    if r > n:
        return
    indices = list(range(r))
    yield tuple(pool[i] for i in indices)
    while True:
        for i in range(r - 1, -1, -1):
            if indices[i] != i + n - r:
                break
        else:
            return
        indices[i] = indices[i] + 1
        for j in range(i + 1, r):
            indices[j] = indices[j - 1] + 1
        yield tuple(pool[i] for i in indices)


def combinations_with_replacement(iterable, r):
    """combinations_with_replacement(iterable, r) --> object"""
    pool = tuple(iterable)
    n = len(pool)
    if r < 0:
        raise ValueError("r must be non-negative")
    if not n and r:
        return
    indices = [0] * r
    yield tuple(pool[i] for i in indices)
    while True:
        for i in range(r - 1, -1, -1):
            if indices[i] != n - 1:
                break
        else:
            return
        indices[i:] = [indices[i] + 1] * (r - i)
        yield tuple(pool[i] for i in indices)


def groupby(iterable, key=None):
    """groupby(iterable, key=None) --> groupby object

    Unlike CPython's, each group is materialised as a list rather than being a
    view that shares the underlying iterator: a group here stays usable after
    the next one has been taken.
    """
    if key is None:
        key = lambda x: x
    it = iter(iterable)
    try:
        cur = next(it)
    except StopIteration:
        return
    curkey = key(cur)
    group = [cur]
    for x in it:
        k = key(x)
        if k == curkey:
            group.append(x)
        else:
            yield (curkey, iter(group))
            curkey = k
            group = [x]
    yield (curkey, iter(group))


def tee(iterable, n=2):
    """tee(iterable, n=2) --> tuple of n independent iterators

    Materialises the source, where CPython's shares a deque between the
    branches; the observable answers are the same for any finite iterable.
    """
    items = list(iterable)
    return tuple(iter(list(items)) for _ in range(n))
