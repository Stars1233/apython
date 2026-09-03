# Sockets: the _socket type, over loopback.
#
# Every address here is 127.0.0.1 with port 0 -- the kernel picks the port and
# the test reads it back -- so nothing collides with anything else on the
# machine and no test needs a fixed number.  The error paths are checked by
# exception TYPE, never by message: the message is the platform's strerror.

import _socket

print("=== constants ===")
print(_socket.AF_INET, _socket.AF_UNIX, _socket.SOCK_STREAM, _socket.SOCK_DGRAM)
print(_socket.SOL_SOCKET, _socket.SO_REUSEADDR, _socket.SO_ERROR, _socket.SO_TYPE)
print(_socket.SHUT_RD, _socket.SHUT_WR, _socket.SHUT_RDWR)
# has_ipv6 is False here and True on CPython, so only its type is comparable.
print(_socket.has_ipv6 in (True, False))
print(_socket.error is OSError, _socket.timeout is TimeoutError)
print(issubclass(_socket.gaierror, OSError), issubclass(_socket.herror, OSError))

print("=== byte order ===")
print(_socket.htons(0x1234), _socket.ntohs(0x3412))
print(_socket.htonl(0x01020304), _socket.ntohl(0x04030201))
print(_socket.htons(_socket.htons(0x1234)) == 0x1234)

print("=== addresses ===")
print(_socket.inet_aton("1.2.3.4"))
print(_socket.inet_ntoa(_socket.inet_aton("255.0.128.1")))
print(_socket.inet_ntop(_socket.AF_INET, _socket.inet_pton(_socket.AF_INET, "10.0.0.1")))
# inet_aton's short forms are part of its contract: the last field fills in
# for the ones that are missing.
print(_socket.inet_aton("127.1"), _socket.inet_aton("1.2.3"), _socket.inet_aton("0x7f.1"))
for bad in ("1.2.3.4.5", "1.2.3.256", "", "a.b.c.d", "1.2.3.-1"):
    try:
        _socket.inet_aton(bad)
        print("no error for", repr(bad))
    except OSError:
        print("rejected", repr(bad))

print("=== a stream round trip ===")
srv = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
srv.setsockopt(_socket.SOL_SOCKET, _socket.SO_REUSEADDR, 1)
srv.bind(("127.0.0.1", 0))
srv.listen(5)
host, port = srv.getsockname()
print("bound", host, port > 0, srv.family, srv.type, srv.proto)

cli = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
cli.connect(("127.0.0.1", port))
fd, peer = srv._accept()
conn = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0, fileno=fd)
print("accepted", peer[0], peer[1] > 0)
print("peername", cli.getpeername() == ("127.0.0.1", port))
print("sockname", conn.getsockname() == ("127.0.0.1", port))

print("send", cli.send(b"hello"))
print("recv", conn.recv(16))
conn.sendall(b"a longer answer, sent whole")
print("recv all", cli.recv(64))

buf = bytearray(8)
conn.send(b"into")
n = cli.recv_into(buf, 4)
print("recv_into", n, bytes(buf[:n]))

print("sotype", cli.getsockopt(_socket.SOL_SOCKET, _socket.SO_TYPE))
print("soerror", cli.getsockopt(_socket.SOL_SOCKET, _socket.SO_ERROR))
print("opt bytes", len(cli.getsockopt(_socket.SOL_SOCKET, _socket.SO_TYPE, 4)))

print("=== shutdown and close ===")
conn.shutdown(_socket.SHUT_WR)
print("eof", cli.recv(8))
cli.close()
conn.close()
print("fileno after close", cli.fileno())
try:
    cli.recv(1)
    print("recv on a closed socket did not raise")
except OSError as e:
    print("closed raises", type(e).__name__)

print("=== blocking, timeouts ===")
a = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
print("default", a.gettimeout(), a.getblocking())
a.setblocking(False)
print("nonblocking", a.gettimeout(), a.getblocking())
a.settimeout(0.05)
print("timeout", a.gettimeout(), a.getblocking())
a.settimeout(None)
print("blocking again", a.gettimeout(), a.getblocking())
try:
    a.settimeout(-1)
    print("negative timeout accepted")
except ValueError:
    print("negative timeout rejected")

b = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
b.connect(("127.0.0.1", port))
fd2, _ = srv._accept()
c2 = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0, fileno=fd2)
b.settimeout(0.05)
try:
    b.recv(16)
    print("a silent peer did not time out")
except TimeoutError:
    print("timed out")
b.settimeout(None)
c2.send(b"now")
print("after the timeout", b.recv(16))
b.close()
c2.close()

print("=== a nonblocking socket that would block ===")
d = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
d.connect(("127.0.0.1", port))
fd3, _ = srv._accept()
c3 = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0, fileno=fd3)
d.setblocking(False)
try:
    d.recv(16)
    print("no BlockingIOError")
except BlockingIOError:
    print("would block")
d.close()
c3.close()

print("=== connect to a closed port ===")
dead = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
dead.bind(("127.0.0.1", 0))
deadport = dead.getsockname()[1]
dead.close()
probe = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
try:
    probe.connect(("127.0.0.1", deadport))
    print("connect to a dead port succeeded")
except ConnectionRefusedError:
    print("refused")
probe.close()
probe = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
print("connect_ex nonzero", probe.connect_ex(("127.0.0.1", deadport)) != 0)
probe.close()

print("=== datagrams ===")
u1 = _socket.socket(_socket.AF_INET, _socket.SOCK_DGRAM, 0)
u2 = _socket.socket(_socket.AF_INET, _socket.SOCK_DGRAM, 0)
u1.bind(("127.0.0.1", 0))
u2.bind(("127.0.0.1", 0))
u1addr = u1.getsockname()
u2.sendto(b"datagram", u1addr)
data, sender = u1.recvfrom(32)
print("recvfrom", data, sender == u2.getsockname())
u1.sendto(b"reply", 0, sender)
print("reply", u2.recv(32))
u1.close()
u2.close()

print("=== socketpair ===")
p, q = _socket.socketpair()
p.send(b"through a pair")
print("pair", q.recv(32))
q.send(b"back")
print("pair back", p.recv(32))
print("pair names", p.getsockname(), q.getpeername())
p.close()
q.close()

print("=== unix sockets ===")
import posix
path = "/tmp/apython_test_socket_%d" % posix.getpid()
try:
    posix.unlink(path)
except OSError:
    pass
us = _socket.socket(_socket.AF_UNIX, _socket.SOCK_STREAM, 0)
us.bind(path)
us.listen(1)
uc = _socket.socket(_socket.AF_UNIX, _socket.SOCK_STREAM, 0)
uc.connect(path)
ufd, uaddr = us._accept()
uconn = _socket.socket(_socket.AF_UNIX, _socket.SOCK_STREAM, 0, fileno=ufd)
print("unix bound", us.getsockname() == path)
uc.send(b"unix")
print("unix recv", uconn.recv(16))
uc.close(); uconn.close(); us.close()
posix.unlink(path)

print("=== names ===")
print(_socket.gethostbyname("localhost"))
print(_socket.gethostbyname("127.0.0.1"))
print(len(_socket.gethostname()) > 0)
print(_socket.getaddrinfo("127.0.0.1", 80, 0, _socket.SOCK_STREAM))
print(_socket.getaddrinfo("localhost", "http", _socket.AF_INET, _socket.SOCK_STREAM))
# AF_INET explicitly: CPython answers for IPv6 as well, and this does not.
print(_socket.getaddrinfo(None, 8080, _socket.AF_INET, _socket.SOCK_STREAM, 0,
                          _socket.AI_PASSIVE))
try:
    _socket.getaddrinfo("no.such.host.invalid", 80)
    print("resolved an invalid host")
except _socket.gaierror:
    print("gaierror")
print(_socket.getservbyname("http"), _socket.getprotobyname("tcp"))

print("=== defaults ===")
print(_socket.getdefaulttimeout())
_socket.setdefaulttimeout(1.5)
e = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM, 0)
print(_socket.getdefaulttimeout(), e.gettimeout())
e.close()
_socket.setdefaulttimeout(None)
print(_socket.getdefaulttimeout())

srv.close()
print("done")
