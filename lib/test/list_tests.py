"""
Tests common to list and UserList.UserList
Adapted from CPython for apython - avoids assertRaises and dunder calls.
"""

import sys
import unittest

from test import seq_tests


class CommonTest(seq_tests.CommonTest):

    def test_repr(self):
        l0 = []
        l2 = [0, 1, 2]
        a0 = self.type2test(l0)
        a2 = self.type2test(l2)

        self.assertEqual(str(a0), str(l0))
        self.assertEqual(repr(a0), repr(l0))
        self.assertEqual(repr(a2), repr(l2))
        self.assertEqual(str(a2), "[0, 1, 2]")
        self.assertEqual(repr(a2), "[0, 1, 2]")

    def test_set_subscript(self):
        a = self.type2test(range(20))
        try:
            a[0:10:0] = [1,2,3]
            self.fail("Expected ValueError")
        except ValueError:
            pass
        try:
            a[0:10] = 1
            self.fail("Expected TypeError")
        except TypeError:
            pass
        try:
            a[0:10:2] = [1,2]
            self.fail("Expected ValueError")
        except ValueError:
            pass
        a[2:10:3] = [1,2,3]
        self.assertEqual(a, self.type2test([0, 1, 1, 3, 4, 2, 6, 7, 3,
                                            9, 10, 11, 12, 13, 14, 15,
                                            16, 17, 18, 19]))

    def test_reversed(self):
        a = self.type2test(range(20))
        r = reversed(a)
        self.assertEqual(list(r), self.type2test(range(19, -1, -1)))
        try:
            next(r)
            self.fail("Expected StopIteration")
        except StopIteration:
            pass
        self.assertEqual(list(reversed(self.type2test())),
                         self.type2test())

    def test_setitem(self):
        a = self.type2test([0, 1])
        a[0] = 0
        a[1] = 100
        self.assertEqual(a, self.type2test([0, 100]))
        a[-1] = 200
        self.assertEqual(a, self.type2test([0, 200]))
        a[-2] = 100
        self.assertEqual(a, self.type2test([100, 200]))
        try:
            a[-3] = 200
            self.fail("Expected IndexError")
        except IndexError:
            pass
        try:
            a[2] = 200
            self.fail("Expected IndexError")
        except IndexError:
            pass

        a = self.type2test([])
        try:
            a[0] = 200
            self.fail("Expected IndexError")
        except IndexError:
            pass

        a = self.type2test([0,1,2,3,4])
        a[0] = 1
        a[1] = 2
        a[2] = 3
        self.assertEqual(a, self.type2test([1,2,3,3,4]))
        a[0] = 5
        a[1] = 6
        a[2] = 7
        self.assertEqual(a, self.type2test([5,6,7,3,4]))
        a[-2] = 88
        a[-1] = 99
        self.assertEqual(a, self.type2test([5,6,7,88,99]))
        a[-2] = 8
        a[-1] = 9
        self.assertEqual(a, self.type2test([5,6,7,8,9]))

    def test_delitem(self):
        a = self.type2test([0, 1])
        del a[1]
        self.assertEqual(a, [0])
        del a[0]
        self.assertEqual(a, [])

        a = self.type2test([0, 1])
        del a[-2]
        self.assertEqual(a, [1])
        del a[-1]
        self.assertEqual(a, [])

        a = self.type2test([0, 1])
        try:
            del a[-3]
            self.fail("Expected IndexError")
        except IndexError:
            pass
        try:
            del a[2]
            self.fail("Expected IndexError")
        except IndexError:
            pass

        a = self.type2test([])
        try:
            del a[0]
            self.fail("Expected IndexError")
        except IndexError:
            pass

    def test_setslice(self):
        l = [0, 1]
        a = self.type2test(l)

        for i in range(-3, 4):
            a[:i] = l[:i]
            self.assertEqual(a, l)
            a2 = a[:]
            a2[:i] = a[:i]
            self.assertEqual(a2, a)
            a[i:] = l[i:]
            self.assertEqual(a, l)
            a2 = a[:]
            a2[i:] = a[i:]
            self.assertEqual(a2, a)
            for j in range(-3, 4):
                a[i:j] = l[i:j]
                self.assertEqual(a, l)
                a2 = a[:]
                a2[i:j] = a[i:j]
                self.assertEqual(a2, a)

        aa2 = a2[:]
        aa2[:0] = [-2, -1]
        self.assertEqual(aa2, [-2, -1, 0, 1])
        aa2[0:] = []
        self.assertEqual(aa2, [])

        a = self.type2test([1, 2, 3, 4, 5])
        a[:-1] = a
        self.assertEqual(a, self.type2test([1, 2, 3, 4, 5, 5]))
        a = self.type2test([1, 2, 3, 4, 5])
        a[1:] = a
        self.assertEqual(a, self.type2test([1, 1, 2, 3, 4, 5]))
        a = self.type2test([1, 2, 3, 4, 5])
        a[1:-1] = a
        self.assertEqual(a, self.type2test([1, 1, 2, 3, 4, 5, 5]))

        a = self.type2test([])
        a[:] = tuple(range(10))
        self.assertEqual(a, self.type2test(range(10)))

    def test_slice_assign_iterator(self):
        x = self.type2test(range(5))
        x[0:3] = reversed(range(3))
        self.assertEqual(x, self.type2test([2, 1, 0, 3, 4]))

        x[:] = reversed(range(3))
        self.assertEqual(x, self.type2test([2, 1, 0]))

    def test_delslice(self):
        a = self.type2test([0, 1])
        del a[1:2]
        del a[0:1]
        self.assertEqual(a, self.type2test([]))

        a = self.type2test([0, 1])
        del a[-2:-1]
        self.assertEqual(a, self.type2test([1]))

        a = self.type2test([0, 1])
        del a[1:]
        del a[:1]
        self.assertEqual(a, self.type2test([]))

        a = self.type2test([0, 1])
        del a[-1:]
        self.assertEqual(a, self.type2test([0]))

        a = self.type2test([0, 1])
        del a[:]
        self.assertEqual(a, self.type2test([]))

    def test_append(self):
        a = self.type2test([])
        a.append(0)
        a.append(1)
        a.append(2)
        self.assertEqual(a, self.type2test([0, 1, 2]))

    def test_extend(self):
        a1 = self.type2test([0])
        a2 = self.type2test((0, 1))
        a = a1[:]
        a.extend(a2)
        self.assertEqual(a, a1 + a2)

        a.extend(self.type2test([]))
        self.assertEqual(a, a1 + a2)

        a.extend(a)
        self.assertEqual(a, self.type2test([0, 0, 1, 0, 0, 1]))

        a = self.type2test("spam")
        a.extend("eggs")
        self.assertEqual(a, list("spameggs"))

    def test_insert(self):
        a = self.type2test([0, 1, 2])
        a.insert(0, -2)
        a.insert(1, -1)
        a.insert(2, 0)
        self.assertEqual(a, [-2, -1, 0, 0, 1, 2])

        b = a[:]
        b.insert(-2, "foo")
        b.insert(-200, "left")
        b.insert(200, "right")
        self.assertEqual(b, self.type2test(["left",-2,-1,0,0,"foo",1,2,"right"]))

    def test_pop(self):
        a = self.type2test([-1, 0, 1])
        a.pop()
        self.assertEqual(a, [-1, 0])
        a.pop(0)
        self.assertEqual(a, [0])
        try:
            a.pop(5)
            self.fail("Expected IndexError")
        except IndexError:
            pass
        a.pop(0)
        self.assertEqual(a, [])
        try:
            a.pop()
            self.fail("Expected IndexError")
        except IndexError:
            pass

    def test_remove(self):
        a = self.type2test([0, 0, 1])
        a.remove(1)
        self.assertEqual(a, [0, 0])
        a.remove(0)
        self.assertEqual(a, [0])
        a.remove(0)
        self.assertEqual(a, [])

        try:
            a.remove(0)
            self.fail("Expected ValueError")
        except ValueError:
            pass

        d = self.type2test('abcdefghcij')
        d.remove('c')
        self.assertEqual(d, self.type2test('abdefghcij'))
        d.remove('c')
        self.assertEqual(d, self.type2test('abdefghij'))
        try:
            d.remove('c')
            self.fail("Expected ValueError")
        except ValueError:
            pass
        self.assertEqual(d, self.type2test('abdefghij'))

    def test_index(self):
        super().test_index()
        a = self.type2test([-2, -1, 0, 0, 1, 2])
        a.remove(0)
        try:
            a.index(2, 0, 4)
            self.fail("Expected ValueError")
        except ValueError:
            pass
        self.assertEqual(a, self.type2test([-2, -1, 0, 1, 2]))

    def test_reverse(self):
        u = self.type2test([-2, -1, 0, 1, 2])
        u2 = u[:]
        u.reverse()
        self.assertEqual(u, [2, 1, 0, -1, -2])
        u.reverse()
        self.assertEqual(u, u2)

    def test_clear(self):
        u = self.type2test([2, 3, 4])
        u.clear()
        self.assertEqual(u, [])

        u = self.type2test([])
        u.clear()
        self.assertEqual(u, [])

        u = self.type2test([])
        u.append(1)
        u.clear()
        u.append(2)
        self.assertEqual(u, [2])

    def test_copy(self):
        u = self.type2test([1, 2, 3])
        v = u.copy()
        self.assertEqual(v, [1, 2, 3])

        u = self.type2test([])
        v = u.copy()
        self.assertEqual(v, [])

        # test that it's indeed a copy and not a reference
        u = self.type2test(['a', 'b'])
        v = u.copy()
        v.append('i')
        self.assertEqual(u, ['a', 'b'])
        self.assertEqual(v, u + ['i'])

        # test that it's a shallow, not a deep copy
        u = self.type2test([1, 2, [3, 4], 5])
        v = u.copy()
        self.assertEqual(u, v)
        self.assertIs(v[3], u[3])

    def test_sort(self):
        u = self.type2test([1, 0])
        u.sort()
        self.assertEqual(u, [0, 1])

        u = self.type2test([2,1,0,-1,-2])
        u.sort()
        self.assertEqual(u, self.type2test([-2,-1,0,1,2]))

    def test_slice(self):
        u = self.type2test("spam")
        u[:2] = "h"
        self.assertEqual(u, list("ham"))

    def test_iadd(self):
        super().test_iadd()
        u = self.type2test([0, 1])
        u2 = u
        u += [2, 3]
        self.assertIs(u, u2)

        u = self.type2test("spam")
        u += "eggs"
        self.assertEqual(u, self.type2test("spameggs"))

    def test_imul(self):
        super().test_imul()
        s = self.type2test([])
        oldid = id(s)
        s *= 10
        self.assertEqual(id(s), oldid)

    def test_extendedslicing(self):
        #  subscript
        a = self.type2test([0,1,2,3,4])

        #  deletion
        del a[::2]
        self.assertEqual(a, self.type2test([1,3]))
        a = self.type2test(range(5))
        del a[1::2]
        self.assertEqual(a, self.type2test([0,2,4]))
        a = self.type2test(range(5))
        del a[1::-2]
        self.assertEqual(a, self.type2test([0,2,3,4]))
        a = self.type2test(range(10))
        del a[::1000]
        self.assertEqual(a, self.type2test([1, 2, 3, 4, 5, 6, 7, 8, 9]))
        #  assignment
        a = self.type2test(range(10))
        a[::2] = [-1]*5
        self.assertEqual(a, self.type2test([-1, 1, -1, 3, -1, 5, -1, 7, -1, 9]))
        a = self.type2test(range(10))
        a[::-4] = [10]*3
        self.assertEqual(a, self.type2test([0, 10, 2, 3, 4, 10, 6, 7, 8 ,10]))
        a = self.type2test(range(4))
        a[::-1] = a
        self.assertEqual(a, self.type2test([3, 2, 1, 0]))
        a = self.type2test(range(10))
        b = a[:]
        c = a[:]
        a[2:3] = self.type2test(["two", "elements"])
        b[2:3] = self.type2test(["two", "elements"])
        c[2:3:] = self.type2test(["two", "elements"])
        self.assertEqual(a, b)
        self.assertEqual(a, c)
        a = self.type2test(range(10))
        a[::2] = tuple(range(5))
        self.assertEqual(a, self.type2test([0, 1, 1, 3, 2, 5, 3, 7, 4, 9]))

    def test_exhausted_iterator(self):
        a = self.type2test([1, 2, 3])
        exhit = iter(a)
        empit = iter(a)
        for x in exhit:  # exhaust the iterator
            next(empit)  # not exhausted
        a.append(9)
        self.assertEqual(list(exhit), [])
        self.assertEqual(list(empit), [9])
        self.assertEqual(a, self.type2test([1, 2, 3, 9]))
