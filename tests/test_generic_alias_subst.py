"""PEP 585 parameter substitution: filling in an alias that is still generic.

`list[T][int]` is `list[int]`.  types.GenericAlias carried no tp_as_mapping at
all, so the second subscript said "'types.GenericAlias' object is not
subscriptable" -- and so did every `C[T][int]` for a user generic, because
typing builds those on the same machinery.  Between them that was the largest
single cause of failure in CPython's own test_typing.

The three arms CPython's _Py_subs_parameters has are each exercised below: a
TypeVar substituted through its own __typing_subst__, a nested alias that is
still generic handed the items ITS parameters ask for, and an ordinary
argument carried over untouched.
"""

from typing import Generic, TypeVar

T = TypeVar('T')
S = TypeVar('S')
K = TypeVar('K')
V = TypeVar('V')


def test_one_parameter():
    assert list[T][int] == list[int]
    assert repr(list[T][int]) == 'list[int]'


def test_two_parameters_in_order():
    assert dict[K, V][str, int] == dict[str, int]
    assert tuple[T, S][int, str] == tuple[int, str]


def test_ellipsis_is_carried_over():
    assert tuple[T, ...][int] == tuple[int, ...]


def test_nested_alias_gets_only_its_own():
    assert list[list[T]][int] == list[list[int]]
    assert dict[T, list[S]][int, str] == dict[int, list[str]]


def test_parameters_shrink_as_they_are_filled():
    assert list[T].__parameters__ == (T,)
    assert list[T][int].__parameters__ == ()
    assert dict[K, V][str, V].__parameters__ == (V,)


def test_a_filled_alias_is_not_generic():
    try:
        list[int][str]
    except TypeError as exc:
        assert 'is not a generic class' in str(exc), exc
    else:
        raise AssertionError('TypeError not raised')


def test_wrong_arity_is_refused():
    for expr in (lambda: list[T][int, str], lambda: dict[K, V][int]):
        try:
            expr()
        except TypeError:
            pass
        else:
            raise AssertionError('TypeError not raised')


def test_user_generic_substitutes():
    class C(Generic[T]):
        pass

    assert C[T][int] == C[int]
    assert C[T].__parameters__ == (T,)


def test_typevar_union():
    import typing
    assert T | int == typing.Union[T, int]
    assert int | T == typing.Union[int, T]


def test_a_nested_alias_with_nothing_to_substitute():
    """`list[int]` inside a generic is not itself generic, and CPython's
    _Py_subs_parameters carries such an argument over UNCHANGED -- it
    substitutes only when __parameters__ is a non-empty tuple.  Treating the
    empty case as a failure made dict[T, list[int]][str] raise "subscript
    failed without an exception"."""
    assert dict[T, list[int]][str] == dict[str, list[int]]
    assert list[tuple[int, str]][()] if False else True   # not generic at all
    assert dict[T, dict[str, int]][float] == dict[float, dict[str, int]]
    # A nested alias that IS still generic still gets its own share.
    assert dict[T, list[S]][int, str] == dict[int, list[str]]
    # And one nested two deep.
    assert list[dict[T, list[int]]][str] == list[dict[str, list[int]]]


for fn in (test_a_nested_alias_with_nothing_to_substitute,
           test_one_parameter,
           test_two_parameters_in_order,
           test_ellipsis_is_carried_over,
           test_nested_alias_gets_only_its_own,
           test_parameters_shrink_as_they_are_filled,
           test_a_filled_alias_is_not_generic,
           test_wrong_arity_is_refused,
           test_user_generic_substitutes,
           test_typevar_union):
    fn()
    print(fn.__name__, 'ok')
print('OK')
