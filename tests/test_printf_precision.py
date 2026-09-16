"""`%`-formatting and format() read a precision differently, and both matter.

str_mod has no integer path of its own: it rewrites the printf spec into a
format() spec and hands it to the same engine.  That made `'%.3d' % 5` a
`ValueError: Precision not allowed in integer format specifier`, which is
format()'s rule applied where printf's belongs -- a precision on `d i u o x X`
is a MINIMUM DIGIT COUNT, and the answer is '005'.

The two languages differ in the other direction as well: a sign, an alternate
form and '=' alignment are refused by format() on a string and IGNORED by
printf, so `'%+5s' % 'x'` is legal where `format('x', '+5s')` is not.  Both
directions are pinned here, because a fix to either one alone breaks the
other.
"""


def test_precision_zero_pads_the_digits():
    assert '%.3d' % 5 == '005'
    assert '%.1d' % 1 == '1'
    assert '%.2i' % 3 == '03'
    assert '%.4x' % 255 == '00ff'
    assert '%.4X' % 255 == '00FF'
    assert '%.4o' % 8 == '0010'


def test_precision_with_a_sign_and_an_alternate_form():
    assert '%+.2d' % 3 == '+03'
    assert '% .2d' % 3 == ' 03'
    assert '%.2d' % -3 == '-03'
    assert '%#.4x' % 255 == '0x00ff'
    assert '%#.4o' % 8 == '0o0010'


def test_width_applies_on_top_of_the_precision():
    assert '%5.3d' % 5 == '  005'
    assert '%-5.3d' % 5 == '005  '


def test_the_zero_flag_still_pads_the_field():
    # The '0' flag still pads the FIELD, on top of the digits the precision
    # already supplied -- measured, not assumed.
    assert '%05.3d' % 5 == '00005'
    assert '%05d' % 5 == '00005'
    assert '%05d' % -42 == '-0042'


def test_precision_zero_keeps_the_digit():
    # C's printf gives '' here; Python's does not.
    assert '%.0d' % 0 == '0'
    assert '%.0d' % 7 == '7'


def test_bytes_shares_the_engine():
    assert b'%.1d' % 1 == b'1'
    assert b'%.3d' % 5 == b'005'
    assert bytearray(b'%.3d') % 5 == b'005'


def test_c_out_of_range_is_an_overflow_error():
    for bad in (-1, 0x110000):
        try:
            '%c' % bad
        except OverflowError as exc:
            assert 'range(0x110000)' in str(exc), exc
        else:
            raise AssertionError('OverflowError not raised')
    assert '%c' % 0x10FFFF == '\U0010FFFF'
    assert '%c' % 65 == 'A'


def test_format_still_refuses_a_precision_on_an_integer():
    try:
        format(42, '5.3d')
    except ValueError as exc:
        assert 'Precision not allowed' in str(exc), exc
    else:
        raise AssertionError('ValueError not raised')


def test_format_refuses_what_printf_ignores():
    for spec, phrase in (('+5s', 'Sign not allowed'),
                         ('#5s', 'Alternate form'),
                         ('=5s', "'=' alignment not allowed")):
        try:
            format('x', spec)
        except ValueError as exc:
            assert phrase in str(exc), (spec, exc)
        else:
            raise AssertionError('ValueError not raised for %r' % spec)

    # ...and printf goes on ignoring them.
    assert '%+5s' % 'x' == '    x'
    assert '%#5s' % 'x' == '    x'
    assert '% 5s' % 'x' == '    x'


def test_s_wants_a_string():
    try:
        format(5, 's')
    except ValueError as exc:
        assert "Unknown format code 's'" in str(exc), exc
    else:
        raise AssertionError('ValueError not raised')

    class S(str):
        pass

    assert format(S('x'), '>5s') == '    x'


def test_the_rest_of_the_mini_language_is_unchanged():
    assert format(42, '<+6d') == '+42   '
    assert format(3.14159, '^10.4f') == '  3.1416  '
    assert format(1234567, '_d') == '1_234_567'
    assert format(255, '#x') == '0xff'
    assert format(-5, '=8d') == '-      5'
    assert format(0.25, '%') == '25.000000%'
    assert f'{3.14159:{1}.{3}}' == '3.14'


for fn in (test_precision_zero_pads_the_digits,
           test_precision_with_a_sign_and_an_alternate_form,
           test_width_applies_on_top_of_the_precision,
           test_the_zero_flag_still_pads_the_field,
           test_precision_zero_keeps_the_digit,
           test_bytes_shares_the_engine,
           test_c_out_of_range_is_an_overflow_error,
           test_format_still_refuses_a_precision_on_an_integer,
           test_format_refuses_what_printf_ignores,
           test_s_wants_a_string,
           test_the_rest_of_the_mini_language_is_unchanged):
    fn()
    print(fn.__name__, 'ok')
print('OK')
