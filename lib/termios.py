"""termios - the terminal attributes ioctl, in Python.

CPython's is C because ioctl is; here the raw call is posix.ioctl and the
struct is unpacked by hand, the same split _socket and select already use.
The layout is Linux's struct termios on x86-64: four 32-bit flag words, a
line discipline byte, NCCS control characters, then the two speeds.
"""

import posix

class error(Exception):
    """What every one of these raises, as CPython's does -- NOT an OSError.

    The errno and its message are the args, which is the shape a caller
    reads: `except termios.error as e: e.args[0]`.
    """


def _ioctl(fd, request, arg):
    try:
        return posix.ioctl(fd, request, arg)
    except OSError as exc:
        raise error(exc.errno, exc.strerror) from None

NCCS = 32
_SIZE = 60          # 4*4 + 1 + 32 + 3 pad + 4 + 4

_TCGETS = 0x5401
_TCSETS = (0x5402, 0x5403, 0x5404)      # TCSANOW, TCSADRAIN, TCSAFLUSH
_TCSBRK = 0x5409
_TCXONC = 0x5408
_TCFLSH = 0x540B

# The constants, taken from the platform's own header through CPython.
B0 = 0
B1000000 = 4104
B110 = 3
B115200 = 4098
B1152000 = 4105
B1200 = 9
B134 = 4
B150 = 5
B1500000 = 4106
B1800 = 10
B19200 = 14
B200 = 6
B2000000 = 4107
B230400 = 4099
B2400 = 11
B2500000 = 4108
B300 = 7
B3000000 = 4109
B3500000 = 4110
B38400 = 15
B4000000 = 4111
B460800 = 4100
B4800 = 12
B50 = 1
B500000 = 4101
B57600 = 4097
B576000 = 4102
B600 = 8
B75 = 2
B921600 = 4103
B9600 = 13
BRKINT = 2
BS0 = 0
BS1 = 8192
BSDLY = 8192
CBAUD = 4111
CBAUDEX = 4096
CDSUSP = 25
CEOF = 4
CEOL = 0
CEOT = 4
CERASE = 127
CFLUSH = 15
CIBAUD = 269418496
CINTR = 3
CKILL = 21
CLNEXT = 22
CLOCAL = 2048
CQUIT = 28
CR0 = 0
CR1 = 512
CR2 = 1024
CR3 = 1536
CRDLY = 1536
CREAD = 128
CRPRNT = 18
CRTSCTS = 2147483648
CS5 = 0
CS6 = 16
CS7 = 32
CS8 = 48
CSIZE = 48
CSTART = 17
CSTOP = 19
CSTOPB = 64
CSUSP = 26
CWERASE = 23
ECHO = 8
ECHOCTL = 512
ECHOE = 16
ECHOK = 32
ECHOKE = 2048
ECHONL = 64
ECHOPRT = 1024
EXTA = 14
EXTB = 15
FF0 = 0
FF1 = 32768
FFDLY = 32768
FIOASYNC = 21586
FIOCLEX = 21585
FIONBIO = 21537
FIONCLEX = 21584
FIONREAD = 21531
FLUSHO = 4096
HUPCL = 1024
ICANON = 2
ICRNL = 256
IEXTEN = 32768
IGNBRK = 1
IGNCR = 128
IGNPAR = 4
IMAXBEL = 8192
INLCR = 64
INPCK = 16
IOCSIZE_MASK = 1073676288
IOCSIZE_SHIFT = 16
ISIG = 1
ISTRIP = 32
IUCLC = 512
IXANY = 2048
IXOFF = 4096
IXON = 1024
NCC = 8
NCCS = 32
NL0 = 0
NL1 = 256
NLDLY = 256
NOFLSH = 128
N_MOUSE = 2
N_PPP = 3
N_SLIP = 1
N_STRIP = 4
N_TTY = 0
OCRNL = 8
OFDEL = 128
OFILL = 64
OLCUC = 2
ONLCR = 4
ONLRET = 32
ONOCR = 16
OPOST = 1
PARENB = 256
PARMRK = 8
PARODD = 512
PENDIN = 16384
TAB0 = 0
TAB1 = 2048
TAB2 = 4096
TAB3 = 6144
TABDLY = 6144
TCFLSH = 21515
TCGETA = 21509
TCGETS = 21505
TCIFLUSH = 0
TCIOFF = 2
TCIOFLUSH = 2
TCION = 3
TCOFLUSH = 1
TCOOFF = 0
TCOON = 1
TCSADRAIN = 1
TCSAFLUSH = 2
TCSANOW = 0
TCSBRK = 21513
TCSBRKP = 21541
TCSETA = 21510
TCSETAF = 21512
TCSETAW = 21511
TCSETS = 21506
TCSETSF = 21508
TCSETSW = 21507
TCXONC = 21514
TIOCCONS = 21533
TIOCEXCL = 21516
TIOCGETD = 21540
TIOCGICOUNT = 21597
TIOCGLCKTRMIOS = 21590
TIOCGPGRP = 21519
TIOCGSERIAL = 21534
TIOCGSOFTCAR = 21529
TIOCGWINSZ = 21523
TIOCINQ = 21531
TIOCLINUX = 21532
TIOCMBIC = 21527
TIOCMBIS = 21526
TIOCMGET = 21525
TIOCMIWAIT = 21596
TIOCMSET = 21528
TIOCM_CAR = 64
TIOCM_CD = 64
TIOCM_CTS = 32
TIOCM_DSR = 256
TIOCM_DTR = 2
TIOCM_LE = 1
TIOCM_RI = 128
TIOCM_RNG = 128
TIOCM_RTS = 4
TIOCM_SR = 16
TIOCM_ST = 8
TIOCNOTTY = 21538
TIOCNXCL = 21517
TIOCOUTQ = 21521
TIOCPKT = 21536
TIOCPKT_DATA = 0
TIOCPKT_DOSTOP = 32
TIOCPKT_FLUSHREAD = 1
TIOCPKT_FLUSHWRITE = 2
TIOCPKT_NOSTOP = 16
TIOCPKT_START = 8
TIOCPKT_STOP = 4
TIOCSCTTY = 21518
TIOCSERCONFIG = 21587
TIOCSERGETLSR = 21593
TIOCSERGETMULTI = 21594
TIOCSERGSTRUCT = 21592
TIOCSERGWILD = 21588
TIOCSERSETMULTI = 21595
TIOCSERSWILD = 21589
TIOCSER_TEMT = 1
TIOCSETD = 21539
TIOCSLCKTRMIOS = 21591
TIOCSPGRP = 21520
TIOCSSERIAL = 21535
TIOCSSOFTCAR = 21530
TIOCSTI = 21522
TIOCSWINSZ = 21524
TOSTOP = 256
VDISCARD = 13
VEOF = 4
VEOL = 11
VEOL2 = 16
VERASE = 2
VINTR = 0
VKILL = 3
VLNEXT = 15
VMIN = 6
VQUIT = 1
VREPRINT = 12
VSTART = 8
VSTOP = 9
VSUSP = 10
VSWTC = 7
VSWTCH = 7
VT0 = 0
VT1 = 16384
VTDLY = 16384
VTIME = 5
VWERASE = 14
XCASE = 4
XTABS = 6144


def _u32(b, off):
    return b[off] | (b[off + 1] << 8) | (b[off + 2] << 16) | (b[off + 3] << 24)


def _p32(v):
    v &= 0xFFFFFFFF
    return bytes((v & 0xFF, (v >> 8) & 0xFF, (v >> 16) & 0xFF, (v >> 24) & 0xFF))


def tcgetattr(fd):
    """[iflag, oflag, cflag, lflag, ispeed, ospeed, cc]."""
    raw = _ioctl(fd, _TCGETS, bytes(_SIZE))
    iflag = _u32(raw, 0)
    oflag = _u32(raw, 4)
    cflag = _u32(raw, 8)
    lflag = _u32(raw, 12)
    ispeed = _u32(raw, 52)
    ospeed = _u32(raw, 56)
    cc = []
    for i in range(NCCS):
        ch = raw[17 + i:18 + i]
        cc.append(ch)
    # VMIN and VTIME are counts, not characters, and CPython gives them as
    # ints when the line is not canonical.
    if not (lflag & ICANON):
        cc[VMIN] = raw[17 + VMIN]
        cc[VTIME] = raw[17 + VTIME]
    return [iflag, oflag, cflag, lflag, ispeed, ospeed, cc]


def tcsetattr(fd, when, attributes):
    if when not in (TCSANOW, TCSADRAIN, TCSAFLUSH):
        raise error("tcsetattr: bad when value")
    if len(attributes) != 7:
        raise TypeError("tcsetattr, arg 3: must be 7 element list")
    iflag, oflag, cflag, lflag, ispeed, ospeed, cc = attributes
    # A short cc list is not checked here, as CPython's is not: the missing
    # entries are simply zero and the kernel gets the struct either way.
    out = bytearray(_SIZE)
    out[0:4] = _p32(iflag)
    out[4:8] = _p32(oflag)
    out[8:12] = _p32(cflag)
    out[12:16] = _p32(lflag)
    out[16] = 0                 # c_line, which nothing here sets
    for i in range(min(len(cc), NCCS)):
        v = cc[i]
        if isinstance(v, int):
            out[17 + i] = v & 0xFF
        else:
            out[17 + i] = v[0] if len(v) else 0
    out[52:56] = _p32(ispeed)
    out[56:60] = _p32(ospeed)
    _ioctl(fd, _TCSETS[when], bytes(out))


def tcsendbreak(fd, duration):
    _ioctl(fd, _TCSBRK, duration)


def tcdrain(fd):
    _ioctl(fd, _TCSBRK, 1)


def tcflush(fd, queue):
    _ioctl(fd, _TCFLSH, queue)


def tcflow(fd, action):
    # valgrind reports this one as passing an unaddressable pointer: its
    # ioctl table does not know TCXONC takes an integer.  A C program doing
    # the same call is reported identically.
    _ioctl(fd, _TCXONC, action)


def tcgetwinsize(fd):
    raw = _ioctl(fd, TIOCGWINSZ, bytes(8))
    return (raw[0] | (raw[1] << 8), raw[2] | (raw[3] << 8))


def tcsetwinsize(fd, winsz):
    rows, cols = winsz
    out = bytearray(8)
    out[0] = rows & 0xFF
    out[1] = (rows >> 8) & 0xFF
    out[2] = cols & 0xFF
    out[3] = (cols >> 8) & 0xFF
    _ioctl(fd, TIOCSWINSZ, bytes(out))
