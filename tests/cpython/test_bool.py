"""Tests for bool type, adapted from CPython's test_bool.py"""

import unittest


class BoolTest(unittest.TestCase):

    def test_subclass(self):
        try:
            class C(bool):
                pass
        except TypeError:
            pass
        else:
            self.fail("bool should not be subclassable")

    def test_repr(self):
        self.assertEqual(repr(False), 'False')
        self.assertEqual(repr(True), 'True')

    def test_str(self):
        self.assertEqual(str(False), 'False')
        self.assertEqual(str(True), 'True')

    def test_int(self):
        self.assertEqual(int(False), 0)
        self.assertIsNot(int(False), False)
        self.assertEqual(int(True), 1)
        self.assertIsNot(int(True), True)

    def test_float(self):
        self.assertEqual(float(False), 0.0)
        self.assertIsNot(float(False), False)
        self.assertEqual(float(True), 1.0)
        self.assertIsNot(float(True), True)

    def test_math(self):
        self.assertEqual(+False, 0)
        self.assertIsNot(+False, False)
        self.assertEqual(-False, 0)
        self.assertIsNot(-False, False)
        self.assertEqual(abs(False), 0)
        self.assertIsNot(abs(False), False)
        self.assertEqual(+True, 1)
        self.assertIsNot(+True, True)
        self.assertEqual(-True, -1)
        self.assertEqual(abs(True), 1)
        self.assertIsNot(abs(True), True)
        self.assertEqual(~False, -1)
        self.assertEqual(~True, -2)

    def test_add(self):
        self.assertIs(False + False, 0)
        self.assertIs(False + True, 1)
        self.assertIs(True + False, 1)
        self.assertIs(True + True, 2)

    def test_sub(self):
        self.assertIs(False - False, 0)
        self.assertIs(True - False, 1)
        self.assertIs(True - True, 0)

    def test_mul(self):
        self.assertIs(False * False, 0)
        self.assertIs(False * True, 0)
        self.assertIs(True * False, 0)
        self.assertIs(True * True, 1)

    def test_floordiv(self):
        self.assertIs(True // True, 1)
        try:
            True // False
        except ZeroDivisionError:
            pass
        else:
            self.fail("True // False should raise ZeroDivisionError")
        self.assertIs(False // True, 0)
        try:
            False // False
        except ZeroDivisionError:
            pass
        else:
            self.fail("False // False should raise ZeroDivisionError")

    def test_mod(self):
        self.assertIs(True % True, 0)
        try:
            True % False
        except ZeroDivisionError:
            pass
        else:
            self.fail("True % False should raise ZeroDivisionError")
        self.assertIs(False % True, 0)
        try:
            False % False
        except ZeroDivisionError:
            pass
        else:
            self.fail("False % False should raise ZeroDivisionError")

    def test_pow(self):
        self.assertIs(True ** False, 1)
        self.assertIs(True ** True, 1)
        self.assertIs(False ** False, 1)
        self.assertIs(False ** True, 0)

    def test_lshift(self):
        self.assertIs(False << False, 0)
        self.assertIs(False << True, 0)
        self.assertIs(True << False, 1)
        self.assertIs(True << True, 2)

    def test_rshift(self):
        self.assertIs(False >> False, 0)
        self.assertIs(False >> True, 0)
        self.assertIs(True >> False, 1)
        self.assertIs(True >> True, 0)

    def test_and(self):
        self.assertIs(False & False, False)
        self.assertIs(False & True, False)
        self.assertIs(True & False, False)
        self.assertIs(True & True, True)
        # Mixed bool & int should return int
        self.assertIs(False & 0, 0)
        self.assertIs(False & 1, 0)
        self.assertIs(True & 1, 1)
        self.assertIs(0 & False, 0)
        self.assertIs(0 & True, 0)
        self.assertIs(1 & False, 0)
        self.assertIs(1 & True, 1)

    def test_or(self):
        self.assertIs(False | False, False)
        self.assertIs(False | True, True)
        self.assertIs(True | False, True)
        self.assertIs(True | True, True)
        # Mixed bool | int should return int
        self.assertIs(False | 0, 0)
        self.assertIs(False | 1, 1)
        self.assertIs(True | 0, 1)
        self.assertIs(True | 1, 1)
        self.assertIs(0 | False, 0)
        self.assertIs(0 | True, 1)
        self.assertIs(1 | False, 1)
        self.assertIs(1 | True, 1)

    def test_xor(self):
        self.assertIs(False ^ False, False)
        self.assertIs(False ^ True, True)
        self.assertIs(True ^ False, True)
        self.assertIs(True ^ True, False)
        # Mixed bool ^ int should return int
        self.assertIs(False ^ 0, 0)
        self.assertIs(False ^ 1, 1)
        self.assertIs(True ^ 0, 1)
        self.assertIs(True ^ 1, 0)
        self.assertIs(0 ^ False, 0)
        self.assertIs(0 ^ True, 1)
        self.assertIs(1 ^ False, 1)
        self.assertIs(1 ^ True, 0)

    def test_string(self):
        self.assertIs("xyz".endswith("z"), True)
        self.assertIs("xyz".endswith("x"), False)
        self.assertIs("xyz0123".isalnum(), True)
        self.assertIs("@#$%".isalnum(), False)
        self.assertIs("xyz".isalpha(), True)
        self.assertIs("@#$%".isalpha(), False)
        self.assertIs("0123".isdigit(), True)
        self.assertIs("xyz".isdigit(), False)
        self.assertIs("xyz".islower(), True)
        self.assertIs("XYZ".islower(), False)
        self.assertIs(" ".isspace(), True)
        self.assertIs("\t".isspace(), True)
        self.assertIs("\n".isspace(), True)
        self.assertIs("xyz".isspace(), False)
        self.assertIs("XYZ".isupper(), True)
        self.assertIs("xyz".isupper(), False)
        self.assertIs("xyz".startswith("x"), True)
        self.assertIs("xyz".startswith("z"), False)

    def test_types(self):
        # types of bool should be int
        self.assertIs(type(True), bool)
        self.assertIs(type(False), bool)
        # isinstance
        self.assertIs(isinstance(True, bool), True)
        self.assertIs(isinstance(False, bool), True)
        self.assertIs(isinstance(True, int), True)
        self.assertIs(isinstance(False, int), True)
        self.assertIs(isinstance(1, bool), False)
        self.assertIs(isinstance(0, bool), False)

    def test_convert_to_bool(self):
        # Verify that converting to bool gives True/False, not 1/0
        self.assertIs(bool(10), True)
        self.assertIs(bool(1), True)
        self.assertIs(bool(-1), True)
        self.assertIs(bool(0), False)
        self.assertIs(bool("hello"), True)
        self.assertIs(bool(""), False)
        self.assertIs(bool(), False)

    def test_isinstance(self):
        self.assertIs(isinstance(True, bool), True)
        self.assertIs(isinstance(False, bool), True)
        self.assertIs(isinstance(True, int), True)
        self.assertIs(isinstance(False, int), True)
        self.assertIs(isinstance(1, bool), False)
        self.assertIs(isinstance(0, bool), False)

    def test_issubclass(self):
        self.assertIs(issubclass(bool, int), True)
        self.assertIs(issubclass(int, bool), False)

    def test_contains(self):
        self.assertIs(1 in {}, False)
        self.assertIs(1 in {1: 2}, True)

    def test_identity(self):
        self.assertIs(True, True)
        self.assertIs(False, False)
        self.assertIsNot(True, False)
        self.assertIsNot(False, True)

    def test_boolean(self):
        self.assertEqual(True & 1, 1)
        self.assertEqual(True | 0, 1)
        self.assertEqual(True ^ 1, 0)

    def test_format(self):
        self.assertEqual("%d" % False, "0")
        self.assertEqual("%d" % True, "1")
        self.assertEqual("%x" % False, "0")
        self.assertEqual("%x" % True, "1")

    def test_hasattr(self):
        self.assertIs(hasattr([], "append"), True)
        self.assertIs(hasattr([], "wobble"), False)

    def test_callable(self):
        self.assertIs(callable(len), True)
        self.assertIs(callable(1), False)

    def test_comparison(self):
        self.assertIs(True == 1, True)
        self.assertIs(True == True, True)
        self.assertIs(True != 0, True)
        self.assertIs(False == 0, True)
        self.assertIs(False == False, True)
        self.assertIs(False != 1, True)

    def test_real_and_imag(self):
        self.assertEqual(True.real, 1)
        self.assertEqual(True.imag, 0)
        self.assertIs(type(True.real), int)
        self.assertIs(type(True.imag), int)
        self.assertEqual(False.real, 0)
        self.assertEqual(False.imag, 0)
        self.assertIs(type(False.real), int)
        self.assertIs(type(False.imag), int)

    def test_blocked(self):
        class Foo:
            def __bool__(self):
                return "yes"

        try:
            bool(Foo())
        except TypeError:
            pass
        else:
            self.fail("__bool__ returning non-bool should raise TypeError")

        class Bar:
            __bool__ = None

        try:
            bool(Bar())
        except TypeError:
            pass
        else:
            self.fail("__bool__ = None should raise TypeError")

    def test_sane_len(self):
        class BadLen:
            def __len__(self):
                return -1

        try:
            bool(BadLen())
        except ValueError:
            pass
        except TypeError:
            pass  # acceptable
        else:
            self.fail("__len__ returning -1 should raise")

    def test_keyword_args(self):
        try:
            bool(x=10)
        except TypeError:
            pass
        else:
            self.fail("bool(x=10) should raise TypeError")


if __name__ == '__main__':
    unittest.main()
