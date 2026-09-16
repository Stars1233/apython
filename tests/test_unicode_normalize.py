"""The four normal forms, over the generated tables.

The tables come from a running CPython's own `unicodedata`, so what is tested
here is the ALGORITHM over them: the fully expanded decompositions, the stable
canonical ordering, the blocked test that governs recomposition, and the
Hangul arithmetic that is in neither table.

Every case below was measured against CPython before it was written down.
"""

import unicodedata


def check(form, text, want):
    got = unicodedata.normalize(form, text)
    assert got == want, "%s(%r) = %r, want %r" % (form, text, got, want)


def test_ascii_is_already_normalized():
    for form in ("NFC", "NFD", "NFKC", "NFKD"):
        s = "hello world 123"
        assert unicodedata.normalize(form, s) == s
        assert unicodedata.normalize(form, "") == ""


def test_canonical_decomposition_and_composition():
    # U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
    check("NFD", "À", "À")
    check("NFC", "À", "À")
    check("NFC", "À", "À")
    # Recursive: U+1E17 decomposes to U+0113, which decomposes again.
    check("NFD", "ḗ", "ḗ")
    check("NFC", "ḗ", "ḗ")
    # A singleton decomposition never composes back.
    check("NFC", "Å", "Å")        # ANGSTROM SIGN -> A WITH RING
    check("NFC", "Ω", "Ω")        # OHM SIGN -> CAPITAL OMEGA


def test_compatibility_only_shows_in_the_k_forms():
    check("NFD", "ﬁ", "ﬁ")        # LATIN SMALL LIGATURE FI
    check("NFKD", "ﬁ", "fi")
    check("NFKC", "ﬁ", "fi")
    check("NFKC", "½", "1⁄2")     # VULGAR FRACTION ONE HALF
    check("NFKC", " ", " ")            # NO-BREAK SPACE
    check("NFC", " ", " ")
    # A character with only a canonical mapping decomposes the same way under
    # both, which is what the empty compatibility span means.
    check("NFKD", "À", "À")


def test_canonical_ordering_is_stable():
    # U+0323 is class 220 and U+0301 is class 230, so the dot below sorts
    # first however it was written.
    check("NFD", "q̣́", "q̣́")
    check("NFD", "q̣́", "q̣́")
    # Two marks of EQUAL class keep their written order -- an unstable sort
    # would swap them and change the string.
    a = "á̀"
    b = "à́"
    assert unicodedata.normalize("NFD", a) != unicodedata.normalize("NFD", b)
    check("NFD", a, "á̀")
    check("NFD", b, "à́")


def test_composition_is_blocked_by_an_intervening_mark():
    # U+0300 is class 230; it cannot reach the `a` past another class-230
    # mark, so only the first one composes.
    check("NFC", "á̀", "á̀")
    # A lower class in between does not block: U+0327 is class 202.
    check("NFC", "á̧", "à̧".replace("à", "á"))
    # A starter resets what can be folded into.
    check("NFC", "áb́", "áb́".replace("b́", "b́"))


def test_hangul_is_arithmetic():
    # U+AC00 is the syllable GA: an L and a V with no trailing consonant.
    check("NFD", "가", "가")
    check("NFC", "가", "가")
    # U+AC01 is GAG, which adds a T.
    check("NFD", "각", "각")
    check("NFC", "각", "각")
    # An LV syllable plus a T composes to LVT.
    check("NFC", "각", "각")
    # The last syllable in the block.
    check("NFD", "힣", "힣")
    check("NFC", "힣", "힣")
    # A jamo that is not part of a syllable stands alone.
    check("NFC", "ᄀ", "ᄀ")


def test_decomposition_reports_the_raw_mapping():
    assert unicodedata.decomposition("À") == "0041 0300"
    assert unicodedata.decomposition("ḗ") == "0113 0301"
    assert unicodedata.decomposition("a") == ""
    assert unicodedata.decomposition("ﬁ") == "<compat> 0066 0069"
    assert unicodedata.decomposition(" ") == "<noBreak> 0020" or \
        unicodedata.decomposition(" ") == "<compat> 0020"
    # A five-digit code point is written with five digits.
    assert unicodedata.decomposition("\U0001d15e") == "1D157 1D165"


def test_normalize_refusals():
    try:
        unicodedata.normalize("NFX", "a")
    except ValueError as e:
        assert "normalization form" in str(e), e
    else:
        raise AssertionError("no ValueError")
    for bad in ((1, "a"), ("NFC", 1)):
        try:
            unicodedata.normalize(*bad)
        except TypeError:
            pass
        else:
            raise AssertionError("no TypeError for %r" % (bad,))


def test_longer_text():
    """A whole string rather than a character: the buffer has to grow, and
    every step has to survive a mix of scripts."""
    src = ("Ká̀ffee 가각힣 "
           "ḗÅﬁ½ " * 40)
    for form in ("NFC", "NFD", "NFKC", "NFKD"):
        out = unicodedata.normalize(form, src)
        # Idempotent, which is the property that catches a half-applied step.
        assert unicodedata.normalize(form, out) == out, form
    assert unicodedata.normalize("NFD", src) != src
    assert len(unicodedata.normalize("NFD", src)) > len(src)


# --- the frozen 3.2 database -------------------------------------------------

def test_ucd_3_2_0_is_a_second_database():
    """`stringprep` folds an IDN label against the version RFC 3454 froze, so
    a property that has moved since 2002 must not move the answer."""
    old = unicodedata.ucd_3_2_0
    assert old.unidata_version == "3.2.0"
    assert unicodedata.unidata_version != "3.2.0"
    # U+231A WATCH: 'N' in 3.2 and 'W' now.
    assert old.east_asian_width("\u231a") == "N"
    assert unicodedata.east_asian_width("\u231a") == "W"
    # U+0F3A: not mirrored in 3.2, mirrored now.
    assert old.mirrored("\u0f3a") == 0
    assert unicodedata.mirrored("\u0f3a") == 1
    # Everything a fold needs is there and answers for the old version.
    assert old.category("\u231a") == "So"
    assert old.bidirectional("a") == "L"
    assert old.combining("\u0301") == 230
    assert old.normalize("NFKC", "\ufb01") == "fi"
    assert old.decomposition("\u00c0") == "0041 0300"


def test_corrigendum_4_is_frozen_too():
    """Unicode Corrigendum #4 corrected five decompositions AFTER 3.2, and a
    frozen database keeps the pre-corrigendum expansion -- while its
    `decomposition()` reports the corrected mapping, which is CPython's own
    split and not an inconsistency to tidy up."""
    old = unicodedata.ucd_3_2_0
    for cp, before in ((0x2F868, 0x2136A), (0x2F874, 0x5F33),
                       (0x2F91F, 0x43AB), (0x2F95F, 0x7AAE),
                       (0x2F9BF, 0x4D57)):
        assert old.normalize("NFD", chr(cp)) == chr(before), hex(cp)
        assert old.normalize("NFC", chr(cp)) == chr(before), hex(cp)
        # The current database has the corrected one.
        assert unicodedata.normalize("NFD", chr(cp)) != chr(before), hex(cp)


def test_stringprep_folds():
    """The consumer that made the frozen copy worth having."""
    import stringprep
    assert stringprep.in_table_a1("\u0221")        # unassigned in 3.2
    assert not stringprep.in_table_a1("a")
    assert stringprep.in_table_c11(" ")
    assert stringprep.in_table_d1("\u05be")        # a RandALCat character
    assert stringprep.map_table_b2("A") == "a"


def test_is_normalized():
    """The question CPython answers from a quick-check table and this answers
    by normalising: the same answer for every input, and what
    test_unicodedata's run over NormalizationTest.txt needs to exist."""
    assert unicodedata.is_normalized("NFC", "abc")
    assert unicodedata.is_normalized("NFD", "abc")
    assert not unicodedata.is_normalized("NFC", "e\u0301")
    assert unicodedata.is_normalized("NFD", "e\u0301")
    assert unicodedata.is_normalized("NFC", "\u00e9")
    assert not unicodedata.is_normalized("NFKC", "\ufb01")
    assert unicodedata.is_normalized("NFC", "\ufb01")
    assert unicodedata.is_normalized("NFC", "")
    # Hangul, which is arithmetic rather than a table row.
    assert unicodedata.is_normalized("NFC", "\uac00")
    assert not unicodedata.is_normalized("NFD", "\uac00")
    # The frozen database answers it too.
    assert unicodedata.ucd_3_2_0.is_normalized("NFC", "abc")
    try:
        unicodedata.is_normalized("NFX", "a")
    except ValueError as e:
        assert "normalization form" in str(e), e
    else:
        raise AssertionError("no ValueError")


def test_str_subclasses_are_accepted():
    """CPython's unicode converter takes a subclass; an exact ob_type compare
    refused one at every entry point here."""

    class S(str):
        pass

    assert unicodedata.normalize("NFC", S("e\u0301")) == "\u00e9"
    assert unicodedata.is_normalized("NFD", S("abc"))
    assert unicodedata.decomposition(S("\u00c0")) == "0041 0300"
    assert unicodedata.category(S("a")) == "Ll"
    assert unicodedata.combining(S("\u0301")) == 230
    assert unicodedata.ucd_3_2_0.category(S("a")) == "Ll"
    assert unicodedata.ucd_3_2_0.normalize("NFC", S("e\u0301")) == "\u00e9"


for fn in (test_ascii_is_already_normalized,
           test_canonical_decomposition_and_composition,
           test_compatibility_only_shows_in_the_k_forms,
           test_canonical_ordering_is_stable,
           test_composition_is_blocked_by_an_intervening_mark,
           test_hangul_is_arithmetic,
           test_decomposition_reports_the_raw_mapping,
           test_normalize_refusals,
           test_longer_text,
           test_ucd_3_2_0_is_a_second_database,
           test_corrigendum_4_is_frozen_too,
           test_stringprep_folds,
           test_is_normalized,
           test_str_subclasses_are_accepted):
    fn()
    print(fn.__name__, 'ok')
print('OK')
