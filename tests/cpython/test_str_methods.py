"""Adapted from CPython Lib/test/string_tests.py — string method tests."""
import unittest


class StrFindTests(unittest.TestCase):
    """str.find, str.rfind, str.count"""

    def test_find_basic(self):
        self.assertEqual("hello world".find("world"), 6)
        self.assertEqual("hello".find("ll"), 2)
        self.assertEqual("hello".find("h"), 0)

    def test_find_not_found(self):
        self.assertEqual("hello".find("xyz"), -1)
        self.assertEqual("hello".find("lll"), -1)

    def test_find_empty_string(self):
        self.assertEqual("".find("a"), -1)

    def test_find_empty_sub(self):
        self.assertEqual("hello".find(""), 0)
        self.assertEqual("".find(""), 0)

    def test_find_single_char(self):
        self.assertEqual("abcabc".find("b"), 1)
        self.assertEqual("abcabc".find("c"), 2)

    def test_find_at_end(self):
        self.assertEqual("hello".find("o"), 4)
        self.assertEqual("hello".find("lo"), 3)

    def test_find_whole_string(self):
        self.assertEqual("abc".find("abc"), 0)

    def test_find_longer_than_string(self):
        self.assertEqual("ab".find("abc"), -1)

    def test_find_type_error(self):
        with self.assertRaises(TypeError):
            "hello".find(42)

    def test_rfind_basic(self):
        self.assertEqual("hello world".rfind("o"), 7)
        self.assertEqual("abcabc".rfind("abc"), 3)
        self.assertEqual("abcabc".rfind("b"), 4)

    def test_rfind_not_found(self):
        self.assertEqual("hello".rfind("xyz"), -1)

    def test_rfind_empty_sub(self):
        self.assertEqual("hello".rfind(""), 5)
        self.assertEqual("".rfind(""), 0)

    def test_rfind_single_occurrence(self):
        self.assertEqual("hello".rfind("h"), 0)
        self.assertEqual("hello".rfind("ll"), 2)

    def test_count_basic(self):
        self.assertEqual("hello".count("l"), 2)
        self.assertEqual("aaa".count("a"), 3)
        self.assertEqual("aaa".count("aa"), 1)

    def test_count_not_found(self):
        self.assertEqual("hello".count("xyz"), 0)

    def test_count_empty_sub(self):
        self.assertEqual("hello".count(""), 6)  # len+1
        self.assertEqual("".count(""), 1)

    def test_count_whole_string(self):
        self.assertEqual("abc".count("abc"), 1)

    def test_count_type_error(self):
        with self.assertRaises(TypeError):
            "hello".count(42)


class StrCaseTests(unittest.TestCase):
    """str.upper, str.lower"""

    def test_upper_basic(self):
        self.assertEqual("hello".upper(), "HELLO")
        self.assertEqual("Hello World".upper(), "HELLO WORLD")

    def test_upper_already(self):
        self.assertEqual("HELLO".upper(), "HELLO")

    def test_upper_empty(self):
        self.assertEqual("".upper(), "")

    def test_upper_mixed(self):
        self.assertEqual("abc123".upper(), "ABC123")
        self.assertEqual("a!b@c".upper(), "A!B@C")

    def test_lower_basic(self):
        self.assertEqual("HELLO".lower(), "hello")
        self.assertEqual("Hello World".lower(), "hello world")

    def test_lower_already(self):
        self.assertEqual("hello".lower(), "hello")

    def test_lower_empty(self):
        self.assertEqual("".lower(), "")

    def test_lower_mixed(self):
        self.assertEqual("ABC123".lower(), "abc123")
        self.assertEqual("A!B@C".lower(), "a!b@c")

    def test_smallstr_upper(self):
        # Short string that stays SmallStr
        self.assertEqual("hi".upper(), "HI")

    def test_heap_str_lower(self):
        # Long string that must be on heap
        s = "ABCDEFGHIJKLMNOP"  # 16 bytes, heap
        self.assertEqual(s.lower(), "abcdefghijklmnop")


class StrStripTests(unittest.TestCase):
    """str.strip, str.lstrip, str.rstrip"""

    def test_strip_spaces(self):
        self.assertEqual("  hello  ".strip(), "hello")
        self.assertEqual("   ".strip(), "")

    def test_strip_tabs(self):
        self.assertEqual("\thello\t".strip(), "hello")

    def test_strip_newlines(self):
        self.assertEqual("\nhello\n".strip(), "hello")
        self.assertEqual("\r\nhello\r\n".strip(), "hello")

    def test_strip_mixed_whitespace(self):
        self.assertEqual(" \t\n\rhello \t\n\r".strip(), "hello")

    def test_strip_empty(self):
        self.assertEqual("".strip(), "")

    def test_strip_no_whitespace(self):
        self.assertEqual("hello".strip(), "hello")

    def test_lstrip(self):
        self.assertEqual("  hello  ".lstrip(), "hello  ")
        self.assertEqual("   ".lstrip(), "")
        self.assertEqual("\t\nhello".lstrip(), "hello")

    def test_rstrip(self):
        self.assertEqual("  hello  ".rstrip(), "  hello")
        self.assertEqual("   ".rstrip(), "")
        self.assertEqual("hello\t\n".rstrip(), "hello")

    def test_strip_form_feed(self):
        self.assertEqual("\x0chello\x0c".strip(), "hello")

    def test_strip_vertical_tab(self):
        self.assertEqual("\x0bhello\x0b".strip(), "hello")


class StrSplitTests(unittest.TestCase):
    """str.split"""

    def test_split_by_char(self):
        self.assertEqual("a,b,c".split(","), ["a", "b", "c"])

    def test_split_by_whitespace(self):
        self.assertEqual("a b c".split(), ["a", "b", "c"])

    def test_split_whitespace_multiple(self):
        self.assertEqual("  a  b  c  ".split(), ["a", "b", "c"])

    def test_split_whitespace_tabs(self):
        self.assertEqual("a\tb\tc".split(), ["a", "b", "c"])

    def test_split_whitespace_mixed(self):
        self.assertEqual("a \t b \n c".split(), ["a", "b", "c"])

    def test_split_multi_char_sep(self):
        self.assertEqual("a::b::c".split("::"), ["a", "b", "c"])

    def test_split_no_sep_found(self):
        self.assertEqual("abc".split(","), ["abc"])

    def test_split_empty_parts(self):
        self.assertEqual(",a,,b,".split(","), ["", "a", "", "b", ""])

    def test_split_single(self):
        self.assertEqual("hello".split(), ["hello"])

    def test_split_empty_string_whitespace(self):
        self.assertEqual("".split(), [])

    def test_split_all_whitespace(self):
        self.assertEqual("   ".split(), [])


class StrReplaceTests(unittest.TestCase):
    """str.replace"""

    def test_replace_basic(self):
        self.assertEqual("hello world".replace("world", "python"), "hello python")

    def test_replace_multiple(self):
        self.assertEqual("aaa".replace("a", "b"), "bbb")

    def test_replace_empty_old(self):
        # Empty old string: interleave
        self.assertEqual("ab".replace("", "-"), "-a-b-")

    def test_replace_delete(self):
        self.assertEqual("hello".replace("l", ""), "heo")

    def test_replace_no_match(self):
        self.assertEqual("hello".replace("xyz", "abc"), "hello")

    def test_replace_whole_string(self):
        self.assertEqual("abc".replace("abc", "xyz"), "xyz")

    def test_replace_empty_string(self):
        self.assertEqual("".replace("a", "b"), "")

    def test_replace_longer_replacement(self):
        self.assertEqual("ab".replace("a", "xyz"), "xyzb")

    def test_replace_overlapping_not_found(self):
        # "aaa".replace("aa", "b") should be "ba" (non-overlapping left-to-right)
        self.assertEqual("aaa".replace("aa", "b"), "ba")


class StrStartEndTests(unittest.TestCase):
    """str.startswith, str.endswith"""

    def test_startswith_basic(self):
        self.assertTrue("hello".startswith("hel"))
        self.assertTrue("hello".startswith("hello"))
        self.assertFalse("hello".startswith("ello"))

    def test_startswith_empty(self):
        self.assertTrue("hello".startswith(""))
        self.assertTrue("".startswith(""))

    def test_startswith_longer_than_string(self):
        self.assertFalse("hi".startswith("hello"))

    def test_startswith_single_char(self):
        self.assertTrue("abc".startswith("a"))
        self.assertFalse("abc".startswith("b"))

    def test_endswith_basic(self):
        self.assertTrue("hello".endswith("llo"))
        self.assertTrue("hello".endswith("hello"))
        self.assertFalse("hello".endswith("hell"))

    def test_endswith_empty(self):
        self.assertTrue("hello".endswith(""))
        self.assertTrue("".endswith(""))

    def test_endswith_longer_than_string(self):
        self.assertFalse("hi".endswith("hello"))

    def test_endswith_single_char(self):
        self.assertTrue("abc".endswith("c"))
        self.assertFalse("abc".endswith("b"))


class StrPrefixSuffixTests(unittest.TestCase):
    """str.removeprefix, str.removesuffix"""

    def test_removeprefix_basic(self):
        self.assertEqual("hello world".removeprefix("hello "), "world")

    def test_removeprefix_no_match(self):
        self.assertEqual("hello".removeprefix("xyz"), "hello")

    def test_removeprefix_empty(self):
        self.assertEqual("hello".removeprefix(""), "hello")

    def test_removeprefix_whole_string(self):
        self.assertEqual("abc".removeprefix("abc"), "")

    def test_removeprefix_longer_than_string(self):
        self.assertEqual("hi".removeprefix("hello"), "hi")

    def test_removesuffix_basic(self):
        self.assertEqual("hello world".removesuffix(" world"), "hello")

    def test_removesuffix_no_match(self):
        self.assertEqual("hello".removesuffix("xyz"), "hello")

    def test_removesuffix_empty(self):
        self.assertEqual("hello".removesuffix(""), "hello")

    def test_removesuffix_whole_string(self):
        self.assertEqual("abc".removesuffix("abc"), "")

    def test_removesuffix_longer_than_string(self):
        self.assertEqual("hi".removesuffix("hello"), "hi")


class StrJoinTests(unittest.TestCase):
    """str.join"""

    def test_join_basic(self):
        self.assertEqual(", ".join(["a", "b", "c"]), "a, b, c")

    def test_join_empty_sep(self):
        self.assertEqual("".join(["a", "b", "c"]), "abc")

    def test_join_single_element(self):
        self.assertEqual(",".join(["abc"]), "abc")

    def test_join_empty_list(self):
        self.assertEqual(",".join([]), "")

    def test_join_two_elements(self):
        self.assertEqual("-".join(["hello", "world"]), "hello-world")

    def test_join_empty_strings(self):
        self.assertEqual(",".join(["", "", ""]), ",,")

    def test_join_long_sep(self):
        self.assertEqual(" and ".join(["a", "b"]), "a and b")

    def test_join_type_error(self):
        with self.assertRaises(TypeError):
            ",".join([1, 2, 3])


class StrIsTests(unittest.TestCase):
    """str.isdigit, str.isalpha, str.isalnum, str.isspace, str.isupper, str.islower"""

    # isdigit
    def test_isdigit_true(self):
        self.assertTrue("0123456789".isdigit())
        self.assertTrue("0".isdigit())

    def test_isdigit_false(self):
        self.assertFalse("12a".isdigit())
        self.assertFalse("abc".isdigit())
        self.assertFalse("1.2".isdigit())
        self.assertFalse(" ".isdigit())

    def test_isdigit_empty(self):
        self.assertFalse("".isdigit())

    # isalpha
    def test_isalpha_true(self):
        self.assertTrue("abc".isalpha())
        self.assertTrue("ABC".isalpha())
        self.assertTrue("AbC".isalpha())
        self.assertTrue("a".isalpha())

    def test_isalpha_false(self):
        self.assertFalse("abc1".isalpha())
        self.assertFalse("1".isalpha())
        self.assertFalse("a b".isalpha())
        self.assertFalse("a!".isalpha())

    def test_isalpha_empty(self):
        self.assertFalse("".isalpha())

    # isalnum
    def test_isalnum_true(self):
        self.assertTrue("abc123".isalnum())
        self.assertTrue("abc".isalnum())
        self.assertTrue("123".isalnum())
        self.assertTrue("a".isalnum())
        self.assertTrue("1".isalnum())

    def test_isalnum_false(self):
        self.assertFalse("abc 123".isalnum())
        self.assertFalse("!".isalnum())
        self.assertFalse("a!b".isalnum())

    def test_isalnum_empty(self):
        self.assertFalse("".isalnum())

    # isspace
    def test_isspace_true(self):
        self.assertTrue(" ".isspace())
        self.assertTrue("   ".isspace())
        self.assertTrue("\t".isspace())
        self.assertTrue("\n".isspace())
        self.assertTrue("\r".isspace())
        self.assertTrue(" \t\n\r".isspace())

    def test_isspace_false(self):
        self.assertFalse("a".isspace())
        self.assertFalse(" a ".isspace())
        self.assertFalse("1".isspace())

    def test_isspace_empty(self):
        self.assertFalse("".isspace())

    # isupper
    def test_isupper_true(self):
        self.assertTrue("ABC".isupper())
        self.assertTrue("ABC123".isupper())
        self.assertTrue("ABC DEF".isupper())

    def test_isupper_false(self):
        self.assertFalse("ABCa".isupper())
        self.assertFalse("abc".isupper())
        self.assertFalse("Abc".isupper())

    def test_isupper_no_cased(self):
        # CPython: "123".isupper() -> False (no cased chars)
        self.assertFalse("123".isupper())
        self.assertFalse(" ".isupper())

    def test_isupper_empty(self):
        self.assertFalse("".isupper())

    # islower
    def test_islower_true(self):
        self.assertTrue("abc".islower())
        self.assertTrue("abc123".islower())
        self.assertTrue("abc def".islower())

    def test_islower_false(self):
        self.assertFalse("abcA".islower())
        self.assertFalse("ABC".islower())
        self.assertFalse("Abc".islower())

    def test_islower_no_cased(self):
        self.assertFalse("123".islower())
        self.assertFalse(" ".islower())

    def test_islower_empty(self):
        self.assertFalse("".islower())


class StrEncodeTests(unittest.TestCase):
    """str.encode"""

    def test_encode_basic(self):
        self.assertEqual("hello".encode(), b"hello")

    def test_encode_utf8(self):
        self.assertEqual("hello".encode("utf-8"), b"hello")

    def test_encode_empty(self):
        self.assertEqual("".encode(), b"")

    def test_encode_type(self):
        self.assertIsInstance("hello".encode(), bytes)


class StrFormatTests(unittest.TestCase):
    """str.format"""

    def test_format_empty_braces(self):
        self.assertEqual("{} {}".format("hello", "world"), "hello world")

    def test_format_numbered(self):
        self.assertEqual("{0} {1}".format("hello", "world"), "hello world")
        self.assertEqual("{1} {0}".format("hello", "world"), "world hello")

    def test_format_literal_braces(self):
        self.assertEqual("{{}}".format(), "{}")
        self.assertEqual("{{0}}".format(), "{0}")

    def test_format_int(self):
        self.assertEqual("{} is {}".format("x", 42), "x is 42")

    def test_format_single(self):
        self.assertEqual("{}".format("test"), "test")

    def test_format_no_placeholders(self):
        self.assertEqual("hello".format(), "hello")

    def test_format_multiple_same(self):
        self.assertEqual("{0}{0}".format("ab"), "abab")

    def test_format_mixed_types(self):
        self.assertEqual("{} {} {}".format(1, "two", 3.0), "1 two 3.0")


if __name__ == "__main__":
    unittest.main()
