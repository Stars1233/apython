"""asyncio - the event loop's Python half.

The loop itself is assembly, in the builtin module `_asynciocore`: the task
type, the ready queue, the two I/O backends, and `run`, `sleep`, `gather`,
`wait_for` and `create_task` on top of them.  Everything in this file is
built out of that, the way `lib/_io.py` is built out of `_iocore` and
`lib/_socket.py` out of `_socketcore`.

What used to be here was assembly too, and it predated any socket support:
`src/pyo/asyncio_streams.asm` hard-coded 127.0.0.1 and ignored the host it
was given, discarded what connect() returned, read into a fixed stack buffer,
handed back `str` where CPython hands back `bytes`, and raised OSErrors built
from fixed strings with no errno -- so `except ConnectionRefusedError` could
not catch one.  There is a real socket layer underneath now, and the streams
are ordinary Python over it.

The loop owes this file exactly one primitive: `wait_fd(fd, events)`, an
awaitable that suspends the running task until a descriptor is ready.  That
is the same IO_WAIT sentinel the old assembly yielded from four hand-written
awaitables of its own; it is simply reachable now.
"""

import _asynciocore
import _socket

from _asynciocore import (CancelledError, Task, TimeoutError, create_task,
                          gather, get_running_loop, run, sleep, wait_fd,
                          wait_for)

__all__ = ["CancelledError", "IncompleteReadError", "LimitOverrunError",
           "Server", "StreamReader", "StreamWriter", "Task", "TimeoutError",
           "create_task", "gather", "get_running_loop", "open_connection",
           "run", "sleep", "start_server", "wait_for", "wait_fd"]

# The poll masks the two backends take, and what CPython's add_reader and
# add_writer amount to.
_READABLE = 1
_WRITABLE = 4

_DEFAULT_LIMIT = 64 * 1024


class IncompleteReadError(EOFError):
    """Fewer bytes than asked for, because the stream ended."""

    def __init__(self, partial, expected):
        super().__init__("%d bytes read on a total of %r expected bytes"
                         % (len(partial), expected))
        self.partial = partial
        self.expected = expected


class LimitOverrunError(Exception):
    """The separator was not found within the reader's limit."""

    def __init__(self, message, consumed):
        super().__init__(message)
        self.consumed = consumed


class StreamReader:
    """The read half of a connection.

    A buffer and a socket.  Everything that reads goes through `_fill`, which
    is the only place that waits, so there is one description of what "wait
    for the peer" means rather than one per method.
    """

    def __init__(self, sock=None, limit=_DEFAULT_LIMIT):
        self._sock = sock
        self._buf = bytearray()
        self._eof = False
        self._limit = limit
        self._exception = None

    # --- what a protocol-less reader needs, and what tests feed by hand ---

    def feed_data(self, data):
        self._buf.extend(data)

    def feed_eof(self):
        self._eof = True

    def at_eof(self):
        return self._eof and not self._buf

    def exception(self):
        return self._exception

    def set_exception(self, exc):
        self._exception = exc

    def _check(self):
        if self._exception is not None:
            raise self._exception

    async def _fill(self):
        """Wait for one chunk.  Answers False at end of file."""
        self._check()
        if self._eof:
            return False
        if self._sock is None:
            self._eof = True
            return False
        # A readiness report is a hint, not a promise: poll can wake a task
        # for a descriptor that has nothing on it by the time the task runs,
        # and the answer to that is to wait again rather than to fail.  This
        # is why every reader over a non-blocking socket has this loop.
        while True:
            await wait_fd(self._sock.fileno(), _READABLE)
            try:
                chunk = self._sock.recv(_DEFAULT_LIMIT)
                break
            except BlockingIOError:
                continue
            except OSError as exc:
                self._exception = exc
                self._eof = True
                raise
        if not chunk:
            self._eof = True
            return False
        self._buf.extend(chunk)
        return True

    async def read(self, n=-1):
        self._check()
        if n == 0:
            return b""
        if n < 0:
            while await self._fill():
                pass
            data = bytes(self._buf)
            del self._buf[:]
            return data
        while not self._buf and not self._eof:
            await self._fill()
        data = bytes(self._buf[:n])
        del self._buf[:len(data)]
        return data

    async def readexactly(self, n):
        self._check()
        if n < 0:
            raise ValueError("readexactly size can not be less than zero")
        while len(self._buf) < n:
            if not await self._fill():
                partial = bytes(self._buf)
                del self._buf[:]
                raise IncompleteReadError(partial, n)
        data = bytes(self._buf[:n])
        del self._buf[:n]
        return data

    async def readuntil(self, separator=b"\n"):
        self._check()
        if not separator:
            raise ValueError("Separator should be at least one-byte string")
        start = 0
        while True:
            index = bytes(self._buf).find(separator, start)
            if index >= 0:
                end = index + len(separator)
                if end > self._limit:
                    raise LimitOverrunError(
                        "Separator is found, but chunk is longer than limit",
                        end)
                data = bytes(self._buf[:end])
                del self._buf[:end]
                return data
            if len(self._buf) > self._limit:
                raise LimitOverrunError(
                    "Separator is not found, and chunk exceed the limit",
                    len(self._buf))
            # Only the tail can hold the start of a separator that is split
            # across two reads.
            start = max(0, len(self._buf) - len(separator) + 1)
            if not await self._fill():
                partial = bytes(self._buf)
                del self._buf[:]
                raise IncompleteReadError(partial, None)

    async def readline(self):
        try:
            return await self.readuntil(b"\n")
        except IncompleteReadError as exc:
            return exc.partial
        except LimitOverrunError as exc:
            raise ValueError(str(exc))

    def __aiter__(self):
        return self

    async def __anext__(self):
        line = await self.readline()
        if not line:
            raise StopAsyncIteration
        return line


class StreamWriter:
    """The write half.  Buffers, and drains when asked."""

    def __init__(self, sock=None, reader=None):
        self._sock = sock
        self._reader = reader
        self._buf = bytearray()
        self._closing = False
        self._closed = False

    def write(self, data):
        if self._closing:
            raise ConnectionResetError("Connection is closed")
        self._buf.extend(data)

    def writelines(self, lines):
        for line in lines:
            self.write(line)

    def can_write_eof(self):
        return True

    def write_eof(self):
        if self._sock is not None:
            self._sock.shutdown(_socket.SHUT_WR)

    def get_extra_info(self, name, default=None):
        if self._sock is None:
            return default
        if name == "socket":
            return self._sock
        if name == "peername":
            try:
                return self._sock.getpeername()
            except OSError:
                return default
        if name == "sockname":
            try:
                return self._sock.getsockname()
            except OSError:
                return default
        return default

    async def drain(self):
        """Push the buffer out, waiting for writability as often as it takes."""
        while self._buf:
            if self._sock is None:
                del self._buf[:]
                return
            await wait_fd(self._sock.fileno(), _WRITABLE)
            try:
                sent = self._sock.send(bytes(self._buf))
            except BlockingIOError:
                continue            # a hint, as in _fill above
            if sent <= 0:
                raise ConnectionResetError("Connection lost")
            del self._buf[:sent]

    def close(self):
        self._closing = True

    def is_closing(self):
        return self._closing

    async def wait_closed(self):
        # Anything still buffered goes out first, which is what CPython's
        # close-then-wait_closed pair amounts to for a stream socket.
        if self._buf and not self._closed:
            try:
                await self.drain()
            except OSError:
                pass
        if not self._closed:
            self._closed = True
            if self._sock is not None:
                self._sock.close()

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc):
        self.close()
        await self.wait_closed()


class Server:
    """What start_server hands back: the listening socket and its accept task."""

    def __init__(self, sock, callback, limit):
        self._sock = sock
        self._callback = callback
        self._limit = limit
        self._closing = False
        self._task = None
        self.sockets = [sock]

    def _start(self):
        self._task = create_task(self._serve())
        return self

    async def _serve(self):
        while not self._closing:
            try:
                await wait_fd(self._sock.fileno(), _READABLE)
                # _socket.socket has the syscall; `accept` proper, which
                # wraps the descriptor back up, is lib/socket.py's.
                fd, _addr = self._sock._accept()
            except BlockingIOError:
                continue            # a hint, as in _fill above
            except OSError:
                if self._closing:
                    return
                raise
            conn = _socket.socket(self._sock.family, self._sock.type,
                                  fileno=fd)
            if self._closing:
                conn.close()
                return
            conn.setblocking(False)
            reader = StreamReader(conn, self._limit)
            writer = StreamWriter(conn, reader)
            create_task(self._run_client(reader, writer))

    async def _run_client(self, reader, writer):
        try:
            result = self._callback(reader, writer)
            if result is not None and hasattr(result, "__await__"):
                await result
        finally:
            writer.close()
            await writer.wait_closed()

    def close(self):
        self._closing = True
        if self._sock is not None:
            self._sock.close()
            self._sock = None

    def is_serving(self):
        return not self._closing

    async def wait_closed(self):
        return None

    async def serve_forever(self):
        while not self._closing:
            await sleep(0.05)

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc):
        self.close()
        await self.wait_closed()


def _sockaddr(host, port, family):
    """The address to hand a socket, resolved the way getaddrinfo resolves."""
    if host is None or host == "":
        host = "0.0.0.0"
    infos = _socket.getaddrinfo(host, port, family, _socket.SOCK_STREAM)
    if not infos:
        raise OSError("getaddrinfo() returned empty list")
    return infos[0][0], infos[0][4]


async def open_connection(host=None, port=None, limit=_DEFAULT_LIMIT,
                          family=0, **kwargs):
    """Connect, and hand back the two halves.

    The connect is non-blocking and finished by waiting for writability, which
    is what makes a refused connection a ConnectionRefusedError with an errno
    rather than an OSError built from a fixed string.
    """
    family, address = _sockaddr(host, port, family)
    sock = _socket.socket(family, _socket.SOCK_STREAM)
    sock.setblocking(False)
    try:
        try:
            sock.connect(address)
        except BlockingIOError:
            await wait_fd(sock.fileno(), _WRITABLE)
            err = sock._error()
            if err:
                raise OSError(err, _socket._strerror(err))
    except BaseException:
        sock.close()
        raise
    reader = StreamReader(sock, limit)
    writer = StreamWriter(sock, reader)
    return reader, writer


async def start_server(client_connected_cb, host=None, port=None,
                       limit=_DEFAULT_LIMIT, family=0, backlog=100, **kwargs):
    """Listen, and run the callback for each connection."""
    family, address = _sockaddr(host, port, family)
    sock = _socket.socket(family, _socket.SOCK_STREAM)
    sock.setsockopt(_socket.SOL_SOCKET, _socket.SO_REUSEADDR, 1)
    sock.setblocking(False)
    try:
        sock.bind(address)
        sock.listen(backlog)
    except BaseException:
        sock.close()
        raise
    return Server(sock, client_connected_cb, limit)._start()
