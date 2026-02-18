"""Adapted from CPython Lib/test/test_float.py — PEP-required behavior only."""
import unittest


INF = float("inf")
NINF = float("-inf")
NAN = float("nan")


class FloatArithmeticTests(unittest.TestCase):
    """PEP: +, -, *, /, //, %, **, unary -, unary +"""

    def test_add(self):
        self.assertEqual(1.0 + 2.0, 3.0)
        self.assertEqual(1.5 + 2.5, 4.0)
        self.assertEqual(-1.0 + 1.0, 0.0)

    def test_add_mixed(self):
        self.assertEqual(1.0 + 2, 3.0)
        self.assertEqual(1 + 2.0, 3.0)

    def test_sub(self):
        self.assertEqual(3.0 - 1.0, 2.0)
        self.assertEqual(1.0 - 3.0, -2.0)

    def test_sub_mixed(self):
        self.assertEqual(3.0 - 1, 2.0)
        self.assertEqual(3 - 1.0, 2.0)

    def test_mul(self):
        self.assertEqual(2.0 * 3.0, 6.0)
        self.assertEqual(-2.0 * 3.0, -6.0)
        self.assertEqual(0.5 * 4.0, 2.0)

    def test_mul_mixed(self):
        self.assertEqual(2.0 * 3, 6.0)
        self.assertEqual(3 * 2.0, 6.0)

    def test_truediv(self):
        self.assertEqual(6.0 / 3.0, 2.0)
        self.assertEqual(7.0 / 2.0, 3.5)
        self.assertEqual(-6.0 / 3.0, -2.0)

    def test_truediv_mixed(self):
        self.assertEqual(6.0 / 3, 2.0)
        self.assertEqual(6 / 3.0, 2.0)

    def test_truediv_by_zero(self):
        with self.assertRaises(ZeroDivisionError):
            1.0 / 0.0

    def test_floordiv(self):
        self.assertEqual(7.0 // 2.0, 3.0)
        self.assertEqual(-7.0 // 2.0, -4.0)
        self.assertEqual(7.0 // -2.0, -4.0)

    def test_floordiv_mixed(self):
        self.assertEqual(7.0 // 2, 3.0)
        self.assertEqual(7 // 2.0, 3.0)

    def test_mod(self):
        self.assertEqual(7.0 % 3.0, 1.0)
        self.assertEqual(-7.0 % 3.0, 2.0)

    def test_mod_mixed(self):
        self.assertEqual(7.0 % 3, 1.0)
        self.assertEqual(7 % 3.0, 1.0)

    def test_power(self):
        self.assertEqual(2.0 ** 3, 8.0)
        self.assertEqual(2.0 ** 3.0, 8.0)
        self.assertEqual(4.0 ** 0.5, 2.0)
        self.assertEqual(2.0 ** 0, 1.0)
        self.assertEqual(2.0 ** -1, 0.5)

    def test_power_mixed(self):
        self.assertEqual(2 ** 3.0, 8.0)
        result = 10.0 ** -1
        self.assertTrue(abs(result - 0.1) < 1e-15)

    def test_unary_neg(self):
        self.assertEqual(-1.5, -(1.5))
        self.assertEqual(--1.5, 1.5)

    def test_unary_pos(self):
        self.assertEqual(+1.5, 1.5)
        self.assertEqual(+(-1.5), -1.5)


class FloatComparisonTests(unittest.TestCase):
    """PEP: ==, !=, <, <=, >, >=, NaN comparisons"""

    def test_eq(self):
        self.assertTrue(1.0 == 1.0)
        self.assertFalse(1.0 == 2.0)

    def test_ne(self):
        self.assertTrue(1.0 != 2.0)
        self.assertFalse(1.0 != 1.0)

    def test_lt(self):
        self.assertTrue(1.0 < 2.0)
        self.assertFalse(2.0 < 1.0)
        self.assertFalse(1.0 < 1.0)

    def test_le(self):
        self.assertTrue(1.0 <= 2.0)
        self.assertTrue(1.0 <= 1.0)
        self.assertFalse(2.0 <= 1.0)

    def test_gt(self):
        self.assertTrue(2.0 > 1.0)
        self.assertFalse(1.0 > 2.0)
        self.assertFalse(1.0 > 1.0)

    def test_ge(self):
        self.assertTrue(2.0 >= 1.0)
        self.assertTrue(1.0 >= 1.0)
        self.assertFalse(1.0 >= 2.0)

    def test_mixed_int_float(self):
        self.assertTrue(1.0 == 1)
        self.assertTrue(1 == 1.0)
        self.assertTrue(1 < 1.5)
        self.assertTrue(1.5 > 1)

    def test_nan_comparisons(self):
        """NaN compares False for everything except !="""
        nan = float("nan")
        self.assertFalse(nan == nan)
        self.assertTrue(nan != nan)
        self.assertFalse(nan < nan)
        self.assertFalse(nan <= nan)
        self.assertFalse(nan > nan)
        self.assertFalse(nan >= nan)
        self.assertFalse(nan == 0)
        self.assertTrue(nan != 0)

    def test_inf_comparisons(self):
        inf = float("inf")
        self.assertTrue(inf > 1e308)
        self.assertTrue(-inf < -1e308)
        self.assertTrue(inf == inf)
        self.assertFalse(inf == -inf)


class FloatReprTests(unittest.TestCase):
    """PEP: repr/str roundtrip, inf, nan, -0.0, short repr"""

    def test_repr_basic(self):
        self.assertEqual(repr(1.0), '1.0')
        self.assertEqual(repr(0.5), '0.5')
        self.assertEqual(repr(-1.0), '-1.0')
        self.assertEqual(repr(0.0), '0.0')

    def test_repr_precision(self):
        self.assertEqual(repr(3.14), '3.14')
        self.assertEqual(repr(1.5), '1.5')

    def test_repr_inf(self):
        self.assertEqual(repr(float("inf")), 'inf')
        self.assertEqual(repr(float("-inf")), '-inf')

    def test_repr_nan(self):
        self.assertEqual(repr(float("nan")), 'nan')

    def test_str_same_as_repr(self):
        """In Python 3, str(float) == repr(float)"""
        for x in [1.0, 0.5, -1.0, 3.14, 0.0]:
            self.assertEqual(str(x), repr(x))

    def test_repr_neg_zero(self):
        self.assertEqual(repr(-0.0), '-0.0')


class FloatConstructorTests(unittest.TestCase):
    """PEP: float(int), float(float), float(str), float()"""

    def test_no_arg(self):
        self.assertEqual(float(), 0.0)

    def test_from_int(self):
        self.assertEqual(float(0), 0.0)
        self.assertEqual(float(1), 1.0)
        self.assertEqual(float(-1), -1.0)
        self.assertEqual(float(100), 100.0)

    def test_from_float(self):
        self.assertEqual(float(1.5), 1.5)
        self.assertEqual(float(-3.14), -3.14)

    def test_from_string(self):
        self.assertEqual(float("3.14"), 3.14)
        self.assertEqual(float("1"), 1.0)
        self.assertEqual(float("-1.5"), -1.5)
        self.assertEqual(float("0"), 0.0)
        self.assertEqual(float("1e10"), 1e10)
        self.assertEqual(float("-1e10"), -1e10)
        self.assertEqual(float("1.5e2"), 150.0)

    def test_from_string_whitespace(self):
        self.assertEqual(float(" 3.14 "), 3.14)
        self.assertEqual(float("  1  "), 1.0)

    def test_from_string_inf(self):
        self.assertEqual(float("inf"), float("inf"))
        self.assertEqual(float("-inf"), float("-inf"))
        self.assertEqual(float("Infinity"), float("inf"))
        self.assertEqual(float("-Infinity"), float("-inf"))

    def test_from_string_nan(self):
        # NaN != NaN, so check repr
        self.assertEqual(repr(float("nan")), "nan")

    def test_from_string_errors(self):
        with self.assertRaises((ValueError, TypeError)):
            float("")
        with self.assertRaises((ValueError, TypeError)):
            float("abc")
        with self.assertRaises((ValueError, TypeError)):
            float("1.2.3")


class FloatHashTests(unittest.TestCase):
    """PEP: hash(float(x)) == hash(x) for integer-valued floats"""

    def test_hash_integer_valued(self):
        """hash(float(n)) == hash(n) for small integers"""
        for n in range(0, 50):
            self.assertEqual(hash(float(n)), hash(n),
                             "hash(float(%d)) != hash(%d)" % (n, n))

    def test_hash_consistency(self):
        self.assertEqual(hash(1.0), hash(1))
        self.assertEqual(hash(0.0), hash(0))
        self.assertEqual(hash(-1.0), hash(-1))


class FloatFormatTests(unittest.TestCase):
    """PEP: format specs via f-strings"""

    def test_format_f(self):
        self.assertEqual(f'{3.14:.2f}', '3.14')
        self.assertEqual(f'{3.14159:.2f}', '3.14')
        self.assertEqual(f'{1.0:.1f}', '1.0')

    def test_format_e(self):
        self.assertEqual(f'{1000.0:.2e}', '1.00e+03')
        self.assertEqual(f'{0.001:.2e}', '1.00e-03')

    def test_format_E(self):
        self.assertEqual(f'{1000.0:.2E}', '1.00E+03')

    def test_format_g(self):
        self.assertEqual(f'{1.0:.6g}', '1')
        self.assertEqual(f'{1234567.0:.6g}', '1.23457e+06')


class FloatBoolTests(unittest.TestCase):
    """PEP: bool(0.0) False, bool(nonzero) True"""

    def test_zero_is_false(self):
        self.assertFalse(bool(0.0))
        self.assertFalse(bool(-0.0))

    def test_nonzero_is_true(self):
        self.assertTrue(bool(1.0))
        self.assertTrue(bool(-1.0))
        self.assertTrue(bool(0.5))

    def test_nan_is_true(self):
        self.assertTrue(bool(float("nan")))

    def test_inf_is_true(self):
        self.assertTrue(bool(float("inf")))
        self.assertTrue(bool(float("-inf")))


class FloatBuiltinTests(unittest.TestCase):
    """PEP: abs(), int(), round()"""

    def test_abs(self):
        self.assertEqual(abs(1.5), 1.5)
        self.assertEqual(abs(-1.5), 1.5)
        self.assertEqual(abs(0.0), 0.0)

    def test_int_truncation(self):
        self.assertEqual(int(1.9), 1)
        self.assertEqual(int(-1.9), -1)
        self.assertEqual(int(1.0), 1)
        self.assertEqual(int(0.5), 0)

    def test_int_overflow(self):
        # int(inf) raises ValueError (or OverflowError in CPython)
        got_exc = False
        try:
            int(float("inf"))
        except ValueError:
            got_exc = True
        except OverflowError:
            got_exc = True
        self.assertTrue(got_exc, "Expected exception for int(inf)")

        got_exc = False
        try:
            int(float("nan"))
        except ValueError:
            got_exc = True
        except OverflowError:
            got_exc = True
        self.assertTrue(got_exc, "Expected exception for int(nan)")

    def test_round_no_ndigits(self):
        self.assertEqual(round(0.5), 0)       # banker's rounding
        self.assertEqual(round(1.5), 2)       # banker's rounding
        self.assertEqual(round(2.5), 2)       # banker's rounding
        self.assertEqual(round(3.5), 4)       # banker's rounding
        self.assertEqual(round(2.7), 3)
        self.assertEqual(round(-0.5), 0)      # banker's rounding

    def test_round_with_ndigits(self):
        self.assertTrue(abs(round(3.14159, 2) - 3.14) < 1e-10)
        self.assertTrue(abs(round(3.14159, 4) - 3.1416) < 1e-10)
        self.assertEqual(round(1234.0, -2), 1200.0)

    def test_pow_builtin(self):
        self.assertEqual(pow(2.0, 3), 8.0)
        self.assertEqual(pow(2.0, 3.0), 8.0)
        self.assertEqual(pow(4.0, 0.5), 2.0)


class InfNanTests(unittest.TestCase):
    """PEP: inf/nan creation, repr, arithmetic properties"""

    def test_inf_from_overflow(self):
        self.assertEqual(1e300 * 1e300, float("inf"))

    def test_nan_from_invalid(self):
        # inf * 0 = nan (can't use assertEqual because nan != nan)
        result = float("inf") * 0.0
        self.assertTrue(result != result)  # NaN property

    def test_inf_arithmetic(self):
        inf = float("inf")
        self.assertEqual(inf + 1, inf)
        self.assertEqual(inf * 2, inf)
        self.assertEqual(-inf + (-1), -inf)

    def test_inf_div(self):
        inf = float("inf")
        self.assertEqual(1.0 / inf, 0.0)

    def test_nan_propagation(self):
        nan = float("nan")
        result = nan + 1
        self.assertTrue(result != result)  # still NaN
        result = nan * 2
        self.assertTrue(result != result)

    def test_repr_inf_nan(self):
        self.assertEqual(repr(float("inf")), 'inf')
        self.assertEqual(repr(float("-inf")), '-inf')
        self.assertEqual(repr(float("nan")), 'nan')

    def test_str_inf_nan(self):
        self.assertEqual(str(float("inf")), 'inf')
        self.assertEqual(str(float("-inf")), '-inf')
        self.assertEqual(str(float("nan")), 'nan')


if __name__ == '__main__':
    unittest.main()
