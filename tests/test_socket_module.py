# socket: CPython's own Lib/socket.py, over this _socket.
#
# The file in lib/ is CPython's, unmodified, so what this really tests is
# whether _socket presents the surface that file expects: it subclasses the
# socket type, calls its __init__ unbound, wraps _accept, builds IntEnums out
# of the module's constants and stacks the io layers on top with makefile().

import socket

print("=== the enums socket.py builds ===")
print(socket.AF_INET, int(socket.AF_INET), socket.AF_INET == 2)
print(socket.SOCK_STREAM, int(socket.SOCK_STREAM), socket.SOCK_DGRAM)
print(socket.AddressFamily.AF_INET is socket.AF_INET)
print(socket.SocketKind.SOCK_STREAM is socket.SOCK_STREAM)
print(socket.has_dualstack_ipv6() in (True, False))

print("=== a server and a client ===")
srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
srv.bind(("127.0.0.1", 0))
srv.listen(5)
host, port = srv.getsockname()
print("family/type", srv.family, srv.type, srv.proto)

cli = socket.create_connection((host, port), timeout=5)
conn, addr = srv.accept()
print("accepted", addr[0], type(conn).__name__)
print("conn family", conn.family, conn.type)

cli.sendall(b"a message")
print("recv", conn.recv(32))
conn.sendall(b"and a reply")
print("recv", cli.recv(32))

print("=== makefile ===")
conn.sendall(b"line one\nline two\n")
f = cli.makefile("rb")
print("readline", f.readline())
print("readline", f.readline())
f.close()

conn.sendall(b"text mode\n")
t = cli.makefile("r", encoding="utf-8")
print("text", t.readline())
t.close()

w = conn.makefile("wb")
w.write(b"written through a file\n")
w.flush()
print("through the file", cli.recv(64))
w.close()

print("=== context manager and repr ===")
with socket.socket() as tmp:
    print("in with", tmp.fileno() >= 0)
print("after with", tmp.fileno())
print("repr shape", repr(tmp).startswith("<socket.socket [closed]"))

print("=== timeouts through socket.py ===")
cli.settimeout(0.05)
try:
    cli.recv(16)
    print("no timeout")
except TimeoutError:
    print("timed out")
print("is socket.timeout", socket.timeout is TimeoutError)
cli.settimeout(None)

print("=== dup and detach ===")
d = cli.dup()
print("dup", d.fileno() != cli.fileno(), d.family == cli.family)
d.close()
fd = socket.socket().detach()
print("detach", fd > 0)
import posix
posix.close(fd)

print("=== fromfd ===")
c2 = socket.socket()
c2.connect((host, port))
conn2, _ = srv.accept()
adopted = socket.fromfd(conn2.fileno(), socket.AF_INET, socket.SOCK_STREAM)
adopted.sendall(b"adopted")
print("fromfd", c2.recv(16))
adopted.close()
conn2.close()
c2.close()

print("=== socketpair ===")
p, q = socket.socketpair()
print("family", p.family, p.type)
p.sendall(b"pair")
print("pair", q.recv(8))
p.close(); q.close()

print("=== errors ===")
dead = socket.socket()
dead.bind(("127.0.0.1", 0))
deadport = dead.getsockname()[1]
dead.close()
probe = socket.socket()
try:
    probe.connect(("127.0.0.1", deadport))
    print("connected to a dead port")
except ConnectionRefusedError:
    print("refused")
probe.close()
print("error is OSError", socket.error is OSError)

print("=== names ===")
print(socket.gethostbyname("localhost"))
print(socket.getaddrinfo("127.0.0.1", 80, socket.AF_INET, socket.SOCK_STREAM))
print(len(socket.gethostname()) > 0)
print(socket.inet_ntoa(socket.inet_aton("8.8.4.4")))
print(socket.ntohs(socket.htons(4321)))

cli.close()
conn.close()
srv.close()
print("done")
