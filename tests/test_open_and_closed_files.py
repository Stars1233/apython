"""What `open()` accepts, and what a closed stream does.

Two refusals that were not CPython's:

`_io.FileIO` tested its path argument with an exact compare against `str`
while the message beside it promised "str, bytes or os.PathLike object" -- so
`open(b'/etc/hostname')` and `open(S(...))` for any str SUBCLASS were both
refused.  pdb's own `_ScriptTarget` is a str subclass, which was 49 of its
tests.

And a file object had no closed flag at all: `close()` shut the descriptor and
left the object looking open, so the next write went to a dead fd and the
failure arrived at interpreter exit as "Exception ignored in: <stdout>" with
an exit status of 120.  That is what `apython -m json.tool` did, because
json.tool writes through `with sys.stdout as out:`.
"""

import os
import sys

PATH = '/tmp/apython-open-closed-%d' % os.getpid()


class StrPath(str):
    pass


class BytesPath(bytes):
    pass


class FSPath:
    def __init__(self, path):
        self.path = path

    def __fspath__(self):
        return self.path


def setup():
    with open(PATH, 'w') as handle:
        handle.write('contents')


def teardown():
    os.unlink(PATH)


def test_open_takes_every_path_kind():
    for arg in (PATH,
                PATH.encode(),
                StrPath(PATH),
                BytesPath(PATH.encode()),
                FSPath(PATH),
                FSPath(PATH.encode())):
        with open(arg) as handle:
            assert handle.read() == 'contents', arg


def test_fileio_takes_them_too():
    import _io
    for arg in (PATH, PATH.encode(), StrPath(PATH)):
        handle = _io.FileIO(arg, 'r')
        try:
            assert handle.read() == b'contents', arg
        finally:
            handle.close()


def test_open_refuses_what_is_not_a_path():
    for bad in (3.5, None, [], object()):
        try:
            open(bad)
        except TypeError as exc:
            assert 'os.PathLike' in str(exc), (bad, exc)
        else:
            raise AssertionError('TypeError not raised for %r' % (bad,))


def test_an_embedded_nul_is_refused():
    try:
        open(PATH + '\0extra')
    except ValueError as exc:
        assert 'null byte' in str(exc), exc
    else:
        raise AssertionError('ValueError not raised')


def test_closed_is_reported_and_close_is_idempotent():
    handle = open(PATH)
    assert handle.closed is False
    handle.close()
    assert handle.closed is True
    handle.close()


def test_every_operation_refuses_a_closed_stream():
    handle = open(PATH, 'w')
    handle.close()
    for name, call in (('write', lambda: handle.write('x')),
                       ('flush', lambda: handle.flush()),
                       ('writelines', lambda: handle.writelines(['x']))):
        try:
            call()
        except ValueError as exc:
            assert 'closed file' in str(exc), (name, exc)
        else:
            raise AssertionError('%s did not refuse a closed stream' % name)

    reader = open(PATH)
    reader.close()
    try:
        reader.read()
    except ValueError as exc:
        assert 'closed file' in str(exc), exc
    else:
        raise AssertionError('read did not refuse a closed stream')


def test_closing_a_stream_does_not_lose_buffered_bytes():
    handle = open(PATH, 'w')
    handle.write('buffered')
    handle.close()
    with open(PATH) as check:
        assert check.read() == 'buffered'


setup()
try:
    for fn in (test_open_takes_every_path_kind,
               test_fileio_takes_them_too,
               test_open_refuses_what_is_not_a_path,
               test_an_embedded_nul_is_refused,
               test_closed_is_reported_and_close_is_idempotent,
               test_every_operation_refuses_a_closed_stream,
               test_closing_a_stream_does_not_lose_buffered_bytes):
        fn()
        print(fn.__name__, 'ok')
finally:
    teardown()
print('OK')
