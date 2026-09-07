# BINARY_SUBSCR and STORE_SUBSCR rewrite themselves into inline handlers when
# they see a list or tuple indexed by an int.  Everything here is about the
# edges of that guard: the shapes that must deopt, and the ownership rules the
# inline path has to honour that the generic protocol used to handle.
#
# Every loop is inside a function so the site executes many times -- a
# specialization that is installed and never exercised proves nothing.


def positive_and_negative():
    l = [10, 20, 30, 40, 50]
    t = (1, 2, 3, 4, 5)
    out = []
    for i in range(5):
        out.append(l[i])
        out.append(t[i])
    for i in range(1, 6):
        out.append(l[-i])
        out.append(t[-i])
    return out


def store_positive_and_negative():
    l = [0] * 5
    for i in range(5):
        l[i] = i * i
    for i in range(1, 6):
        l[-i] = l[-i] + 100
    return l


def out_of_range_get():
    l = [1, 2, 3]
    seen = []
    for i in (0, 1, 2, 3, -3, -4):
        try:
            seen.append(l[i])
        except IndexError as e:
            seen.append("IndexError: %s" % e)
    return seen


def out_of_range_store():
    l = [1, 2, 3]
    seen = []
    for i in (0, 3, -4):
        try:
            l[i] = 9
            seen.append(list(l))
        except IndexError as e:
            seen.append("IndexError: %s" % e)
    return seen


def wrong_key_type():
    l = [1, 2, 3]
    seen = []
    # The site specializes on the int, then must deopt for every other key.
    for k in (0, 1, "x", 2, 1.5, 0):
        try:
            seen.append(l[k])
        except TypeError as e:
            seen.append("TypeError: %s" % e)
    return seen


def alternating_containers():
    # One site that sees a list, a tuple, a dict and a str in turn.  Each
    # deopt rewrites the generic opcode back, and the next list or tuple
    # re-specializes it; the answers must not depend on which it is holding.
    d = {0: "d0", 1: "d1"}
    containers = ([1, 2], (3, 4), d, "ab", [5, 6], (7, 8))
    out = []
    for _ in range(3):
        for c in containers:
            out.append(c[0])
            out.append(c[1])
    return out


class MyList(list):
    pass


class MyTuple(tuple):
    pass


def subclasses_deopt():
    # A subclass is not list_type, so the guard must reject it -- and a
    # subclass may override __getitem__, which the inline read would miss.
    class Doubling(list):
        def __getitem__(self, i):
            return list.__getitem__(self, i) * 2

    plain = MyList([1, 2, 3])
    tup = MyTuple((4, 5, 6))
    doubling = Doubling([7, 8, 9])
    out = []
    for i in range(3):
        out.append(plain[i])
        out.append(tup[i])
        out.append(doubling[i])
    return out


def slices_are_not_indices():
    l = [1, 2, 3, 4, 5]
    t = (1, 2, 3, 4, 5)
    return [l[1:3], l[::-1], l[:], t[1:3], t[::2]]


def last_reference_is_the_stack():
    # `make()[i]` -- the container's only reference is the one on the value
    # stack, so the item has to be INCREFd before the container is dropped.
    def make():
        return [object(), "kept", object()]

    out = []
    for _ in range(200):
        out.append(make()[1])
    return len(out), out[0], out[-1], len(set(out))


def store_releases_the_old_value():
    # The old occupant is released after the store, and a __del__ reached from
    # that release must see the new value already in place.
    order = []

    class Noisy:
        def __init__(self, tag, box):
            self.tag = tag
            self.box = box

        def __del__(self):
            # The list must already hold the replacement by now.
            order.append((self.tag, self.box[0]))

    box = [None]
    box[0] = Noisy("first", box)
    box[0] = "replacement"
    box[0] = "second"
    return order, box


def big_list_indices():
    # Indices past 255, so the loop counter is nothing like the oparg -- and a
    # reminder that BINARY_SUBSCR has no oparg at all, which is why its deopt
    # may rewind rbx.
    l = list(range(1000))
    total = 0
    for i in range(0, 1000, 7):
        total += l[i]
        l[i] = l[i] + 1
    return total, l[0], l[994], l[999]


def nested_and_chained():
    grid = [[r * 10 + c for c in range(5)] for r in range(5)]
    out = []
    for r in range(5):
        for c in range(5):
            out.append(grid[r][c])
    grid[2][3] = 999
    return out, grid[2]


print(positive_and_negative())
print(store_positive_and_negative())
print(out_of_range_get())
print(out_of_range_store())
print(wrong_key_type())
print(alternating_containers())
print(subclasses_deopt())
print(slices_are_not_indices())
print(last_reference_is_the_stack())
print(store_releases_the_old_value())
print(big_list_indices())
print(nested_and_chained())

# bytes and bytearray index to ints, and neither may take the list path.
b = b"abc"
ba = bytearray(b"xyz")
print([b[i] for i in range(3)], [ba[i] for i in range(3)])
ba[0] = 65
print(ba, ba[-1])

# A tuple of one element, and an empty list -- the bounds check at the edge.
print((7,)[0], (7,)[-1])
try:
    print([][0])
except IndexError as e:
    print("IndexError: %s" % e)

# A __del__ reached from the release may mutate the list.  Storing before
# releasing is what keeps the slot address valid across it; the other order
# wrote through a reallocated ob_item.
def del_mutates_the_list():
    log = []

    class Grower:
        def __init__(self, target):
            self.target = target

        def __del__(self):
            # Force ob_item to be reallocated from inside the release.
            for i in range(64):
                self.target.append(i)
            log.append(len(self.target))

    l = [None, "tail"]
    l[0] = Grower(l)
    l[0] = "replaced"          # releases the Grower, which grows `l`
    return log, l[0], l[1], len(l)


print(del_mutates_the_list())
