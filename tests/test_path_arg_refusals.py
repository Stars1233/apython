"""Every way a path argument can be refused, and what it costs.

`posix_path_arg` is the one converter thirteen posix entry points and
`_io.FileIO` share.  It RETURNS 0 with the exception pending rather than
raising, and the reason is the callers that resolve two paths: rename,
symlink, link and putenv convert the first, hold what it owns, and then
convert the second.  A raise from the second abandons the C stack, so the
cleanup those callers have ready never runs -- and what leaks is the first
path's resolved string, about 64 bytes per refusal, silently.

The wordings below were all measured against CPython.  The one exception
is the embedded-NUL message, which CPython 3.12 changed mid-series -- see
`normalise` below.
"""

import io
import os


class Resolves:
    """A PathLike whose __fspath__ builds a NEW string each time, which is
    what makes the converter own something it has to release."""

    def __init__(self, value):
        self.value = value

    def __fspath__(self):
        return "".join(self.value)


class Raises:
    def __fspath__(self):
        raise RuntimeError("boom")


class Wrong:
    def __fspath__(self):
        return 5


def normalise(msg):
    """Collapse the one message CPython 3.12 changed mid-series.

    3.12.3 says "embedded null byte"; 3.12.9 and later say
    "stat: embedded null character in path", naming the function and the
    parameter.  The suite diffs against whichever python3 is installed --
    3.12.3 on this box, a later one in CI -- so the wording cannot be
    compared and the fact can.  `tests/test_nul_paths.py` and
    `tests/test_posix.py` do the same, for the same reason.
    """
    if "null" in msg.lower():
        return "embedded null"
    return msg


def refuse(fn, exc, text=None):
    try:
        fn()
    except exc as e:
        if text is not None:
            got = normalise(str(e))
            assert got == text, "%r != %r" % (got, text)
        return
    raise AssertionError("%s was not raised" % exc.__name__)


def test_the_four_refusals():
    refuse(lambda: os.stat(5.5), TypeError)
    refuse(lambda: os.stat("/tmp/a\0b"), ValueError, "embedded null")
    refuse(lambda: os.stat(Raises()), RuntimeError, "boom")
    refuse(lambda: os.stat(Wrong()), TypeError)
    # A PathLike that resolves to something with a NUL is refused after the
    # __fspath__ step, which is the arm that owns its result.
    refuse(lambda: os.stat(Resolves("/tmp/a\0b")), ValueError,
           "embedded null")


def test_io_takes_the_same_road():
    refuse(lambda: io.FileIO(5.5), TypeError,
           "expected str, bytes or os.PathLike object, not float")
    refuse(lambda: io.FileIO("a\0b"), ValueError, "embedded null")
    refuse(lambda: io.FileIO(Raises()), RuntimeError, "boom")
    refuse(lambda: io.FileIO(Wrong()), TypeError)


def test_a_second_bad_path_still_releases_the_first():
    """The shape the returning contract exists for.

    The leaked object is a plain str, which is not GC-tracked -- so
    gc.get_objects() cannot see it and a count over that comes back clean
    either way.  Resident memory is what shows it: the leak was 64 bytes a
    call, which is about 12 MB over the loop below and 0 with the release in
    place.  The threshold is far enough above the allocator's own growth to
    be insensitive to it.
    """
    import resource

    for fn in (lambda: os.rename(Resolves("/tmp/aa"), 5.5),
               lambda: os.symlink(Resolves("/tmp/aa"), 5.5),
               lambda: os.link(Resolves("/tmp/aa"), 5.5)):
        refuse(fn, TypeError)

    def churn(n):
        for _ in range(n):
            try:
                os.rename(Resolves("/tmp/unique-path-for-the-leak-test"), 5.5)
            except TypeError:
                pass

    def rss():
        return resource.getrusage(resource.RUSAGE_SELF).ru_maxrss

    churn(20000)                    # let the pool reach its working size
    before = rss()
    churn(200000)
    grew = rss() - before
    assert grew < 2000, "grew %d kB over 200,000 refusals" % grew


def test_a_good_path_still_works():
    """The success path is the one with the single `ret`, and it must still
    answer -- a converter that refused everything would pass the tests above
    and nothing else."""
    assert os.stat("/etc/hostname").st_size >= 0
    assert os.stat(Resolves("/etc/hostname")).st_size >= 0
    assert os.stat(b"/etc/hostname").st_size >= 0
    f = io.FileIO(Resolves("/etc/hostname"))
    try:
        assert f.read(1) != b""
    finally:
        f.close()


for fn in (test_the_four_refusals,
           test_io_takes_the_same_road,
           test_a_second_bad_path_still_releases_the_first,
           test_a_good_path_still_works):
    fn()
    print(fn.__name__, 'ok')
print('OK')
