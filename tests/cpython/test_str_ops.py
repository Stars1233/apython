"""Adapted from CPython Lib/test/string_tests.py — string operators and builtins."""
import unittest


class StrConcatTests(unittest.TestCase):
    """str + str concatenation."""

    def test_basic(self):
        self.assertEqual("hello" + " " + "world", "hello world")

    def test_empty_left(self):
        self.assertEqual("" + "abc", "abc")

    def test_empty_right(self):
        self.assertEqual("abc" + "", "abc")

    def test_both_empty(self):
        self.assertEqual("" + "", "")

    def test_type_error_int(self):
        with self.assertRaises(TypeError):
            "hello" + 42

    def test_type_error_list(self):
        with self.assertRaises(TypeError):
            "hello" + [1, 2]

    def test_long_strings(self):
        a = "a" * 100
        b = "b" * 100
        self.assertEqual(len(a + b), 200)

    def test_smallstr_to_heap(self):
        # SmallStr boundary: <=14 bytes inline, >14 heap
        s = "abcdefghijklmn"   # 14 bytes, SmallStr
        t = "o"
        result = s + t         # 15 bytes, must go to heap
        self.assertEqual(result, "abcdefghijklmno")
        self.assertEqual(len(result), 15)


class StrRepeatTests(unittest.TestCase):
    """str * int repetition."""

    def test_basic(self):
        self.assertEqual("ab" * 3, "ababab")

    def test_single_char(self):
        self.assertEqual("x" * 5, "xxxxx")

    def test_zero(self):
        self.assertEqual("abc" * 0, "")

    def test_one(self):
        self.assertEqual("abc" * 1, "abc")

    def test_negative(self):
        self.assertEqual("abc" * -1, "")
        self.assertEqual("abc" * -100, "")

    def test_empty_repeat(self):
        self.assertEqual("" * 5, "")

    def test_right_multiply(self):
        self.assertEqual(3 * "ab", "ababab")

    def test_smallstr_to_heap(self):
        s = "abcde"   # 5 bytes SmallStr
        result = s * 3  # 15 bytes, goes to heap
        self.assertEqual(result, "abcdeabcdeabcde")


class StrCompareTests(unittest.TestCase):
    """String comparisons."""

    def test_equal(self):
        self.assertTrue("abc" == "abc")
        self.assertFalse("abc" == "def")

    def test_not_equal(self):
        self.assertTrue("abc" != "def")
        self.assertFalse("abc" != "abc")

    def test_less_than(self):
        self.assertTrue("abc" < "abd")
        self.assertTrue("abc" < "abcd")
        self.assertTrue("" < "a")

    def test_less_equal(self):
        self.assertTrue("abc" <= "abc")
        self.assertTrue("abc" <= "abd")

    def test_greater_than(self):
        self.assertTrue("abd" > "abc")
        self.assertTrue("abcd" > "abc")
        self.assertTrue("a" > "")

    def test_greater_equal(self):
        self.assertTrue("abc" >= "abc")
        self.assertTrue("abd" >= "abc")

    def test_empty_strings(self):
        self.assertTrue("" == "")
        self.assertFalse("" != "")
        self.assertFalse("" < "")
        self.assertTrue("" <= "")

    def test_case_sensitivity(self):
        # 'A' (65) < 'a' (97) in ASCII
        self.assertTrue("A" < "a")
        self.assertTrue("ABC" < "abc")

    def test_not_equal_to_int(self):
        self.assertFalse("42" == 42)
        self.assertTrue("42" != 42)

    def test_long_strings(self):
        a = "x" * 100
        b = "x" * 100
        self.assertTrue(a == b)
        c = "x" * 99 + "y"
        self.assertTrue(c > a)


class StrContainsTests(unittest.TestCase):
    """The 'in' operator for strings."""

    def test_basic(self):
        self.assertTrue("ll" in "hello")
        self.assertFalse("xyz" in "hello")

    def test_single_char(self):
        self.assertTrue("h" in "hello")
        self.assertTrue("o" in "hello")
        self.assertFalse("z" in "hello")

    def test_whole_string(self):
        self.assertTrue("hello" in "hello")

    def test_empty_in_string(self):
        self.assertTrue("" in "hello")

    def test_empty_in_empty(self):
        self.assertTrue("" in "")

    def test_not_in(self):
        self.assertFalse("x" not in "xyz")
        self.assertTrue("a" not in "xyz")


class StrIndexTests(unittest.TestCase):
    """Indexing and slicing."""

    def test_basic_index(self):
        s = "hello"
        self.assertEqual(s[0], "h")
        self.assertEqual(s[1], "e")
        self.assertEqual(s[4], "o")

    def test_negative_index(self):
        s = "hello"
        self.assertEqual(s[-1], "o")
        self.assertEqual(s[-5], "h")

    def test_index_error(self):
        s = "hello"
        with self.assertRaises(IndexError):
            s[5]
        with self.assertRaises(IndexError):
            s[-6]

    def test_basic_slice(self):
        s = "hello world"
        self.assertEqual(s[0:5], "hello")
        self.assertEqual(s[6:11], "world")

    def test_slice_defaults(self):
        s = "hello"
        self.assertEqual(s[:3], "hel")
        self.assertEqual(s[3:], "lo")
        self.assertEqual(s[:], "hello")

    def test_slice_negative(self):
        s = "hello"
        self.assertEqual(s[-3:], "llo")
        self.assertEqual(s[:-2], "hel")

    def test_slice_step(self):
        s = "abcdefgh"
        self.assertEqual(s[::2], "aceg")
        self.assertEqual(s[1::2], "bdfh")

    def test_slice_negative_step(self):
        s = "abcde"
        self.assertEqual(s[::-1], "edcba")
        self.assertEqual(s[4:1:-1], "edc")

    def test_slice_empty_range(self):
        s = "hello"
        self.assertEqual(s[3:1], "")
        self.assertEqual(s[5:10], "")

    def test_long_string_index(self):
        s = "a" * 50 + "b"
        self.assertEqual(s[50], "b")
        self.assertEqual(s[-1], "b")


class StrBuiltinTests(unittest.TestCase):
    """Built-in functions on strings."""

    def test_len(self):
        self.assertEqual(len(""), 0)
        self.assertEqual(len("a"), 1)
        self.assertEqual(len("hello"), 5)
        self.assertEqual(len("a" * 100), 100)

    def test_bool(self):
        self.assertFalse(bool(""))
        self.assertTrue(bool("a"))
        self.assertTrue(bool("hello"))

    def test_bool_in_if(self):
        if "":
            self.fail("empty string should be falsy")
        if "x":
            pass
        else:
            self.fail("non-empty string should be truthy")

    def test_hash_equal_strings(self):
        self.assertEqual(hash("abc"), hash("abc"))
        self.assertEqual(hash(""), hash(""))

    def test_hash_deterministic(self):
        h1 = hash("hello world")
        h2 = hash("hello world")
        self.assertEqual(h1, h2)

    def test_str_constructor(self):
        self.assertEqual(str(42), "42")
        self.assertEqual(str(0), "0")
        self.assertEqual(str(-1), "-1")
        self.assertEqual(str(True), "True")
        self.assertEqual(str(False), "False")
        self.assertEqual(str(3.14), "3.14")
        self.assertEqual(str("abc"), "abc")

    def test_str_constructor_no_args(self):
        self.assertEqual(str(), "")

    def test_repr_string(self):
        self.assertEqual(repr("hello"), "'hello'")
        self.assertEqual(repr(""), "''")

    def test_repr_escape(self):
        r = repr("it's")
        # CPython uses "it's", apython uses 'it\'s' — both valid
        self.assertTrue(r == "\"it's\"" or r == "'it\\'s'")

    def test_ord(self):
        self.assertEqual(ord("a"), 97)
        self.assertEqual(ord("A"), 65)
        self.assertEqual(ord("0"), 48)
        self.assertEqual(ord(" "), 32)
        self.assertEqual(ord("\n"), 10)

    def test_ord_error(self):
        with self.assertRaises(TypeError):
            ord("ab")
        with self.assertRaises(TypeError):
            ord("")

    def test_chr(self):
        self.assertEqual(chr(97), "a")
        self.assertEqual(chr(65), "A")
        self.assertEqual(chr(48), "0")
        self.assertEqual(chr(32), " ")
        self.assertEqual(chr(10), "\n")

    def test_chr_range(self):
        self.assertEqual(chr(0), "\x00")
        self.assertEqual(chr(127), "\x7f")

    def test_iter(self):
        result = list("hello")
        self.assertEqual(len(result), 5)
        self.assertEqual(result[0], "h")
        self.assertEqual(result[1], "e")
        self.assertEqual(result[2], "l")
        self.assertEqual(result[3], "l")
        self.assertEqual(result[4], "o")

    def test_iter_empty(self):
        result = list("")
        self.assertEqual(len(result), 0)

    def test_iter_for_loop(self):
        chars = []
        for c in "abc":
            chars.append(c)
        self.assertEqual(len(chars), 3)
        self.assertEqual(chars[0], "a")
        self.assertEqual(chars[1], "b")
        self.assertEqual(chars[2], "c")


class StrModFormatTests(unittest.TestCase):
    """The % formatting operator."""

    def test_percent_s(self):
        self.assertEqual("hello %s" % "world", "hello world")
        self.assertEqual("%s" % "abc", "abc")

    def test_percent_d(self):
        self.assertEqual("%d" % 42, "42")
        self.assertEqual("%d" % 0, "0")
        self.assertEqual("%d" % -1, "-1")

    def test_percent_r(self):
        self.assertEqual("%r" % "hello", "'hello'")

    def test_percent_percent(self):
        self.assertEqual("100%%" % (), "100%")

    def test_tuple_args(self):
        self.assertEqual("%s and %s" % ("a", "b"), "a and b")
        self.assertEqual("%d + %d = %d" % (1, 2, 3), "1 + 2 = 3")

    def test_percent_s_non_string(self):
        self.assertEqual("%s" % 42, "42")
        self.assertEqual("%s" % True, "True")
        self.assertEqual("%s" % 3.14, "3.14")

    def test_percent_x(self):
        self.assertEqual("%x" % 255, "ff")
        self.assertEqual("%x" % 0, "0")
        self.assertEqual("%x" % 16, "10")

    def test_mixed_format(self):
        self.assertEqual("name=%s age=%d" % ("bob", 30), "name=bob age=30")


if __name__ == "__main__":
    unittest.main()
