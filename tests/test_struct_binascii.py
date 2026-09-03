# _struct and binascii, and the bytes.fromhex that binascii needs.
#
# _struct blocked twelve stdlib modules and binascii five.  Both are C in
# CPython; here _struct is Python over int.to_bytes/int.from_bytes with the
# float codes built from math.frexp and math.ldexp, and binascii is built on
# bytes.hex and bytes.fromhex -- which existed only in the hex() direction.
import _struct
import binascii


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


# --- sizes, including the two alignment rules
for _f in ('<bhilqBHILQfd?', '@bhilq', '=bhilq', '<3s2h', '@ci', '=ci',
           '>2h3b', '<10s', '<3p', 'x', 'ixi', '@i', '<', '2i', '<P', '@P', '@n'):
    show("calcsize %-16r" % _f, lambda f=_f: _struct.calcsize(f))
show("calcsize bad", lambda: _struct.calcsize('<z'))
show("calcsize trailing", lambda: _struct.calcsize('<3'))
show("calcsize native-only", lambda: _struct.calcsize('<n'))

# --- the integer codes, at their edges
_INTS = [('<b', -128), ('<b', 127), ('<B', 0), ('<B', 255),
         ('<h', -32768), ('>h', 32767), ('<H', 65535),
         ('<i', -2**31), ('>i', 2**31 - 1), ('<I', 2**32 - 1),
         ('<q', -2**63), ('>q', 2**63 - 1), ('<Q', 2**64 - 1)]
for _f, _v in _INTS:
    show("pack %-5s %-22d" % (_f, _v), lambda f=_f, v=_v: _struct.pack(f, v))
    show("  round trip",
         lambda f=_f, v=_v: _struct.unpack(f, _struct.pack(f, v)) == (v,))
show("pack overflow", lambda: _struct.pack('<b', 128))
show("pack under", lambda: _struct.pack('<B', -1))
show("pack non-int", lambda: _struct.pack('<i', 'x'))
show("pack bool as int", lambda: _struct.pack('<i', True))

# --- the float codes, including the awkward values
_FLOATS = [('<f', 1.5), ('>f', -0.0), ('<f', 0.1), ('<f', 1e-45),
           ('<f', 3.4028234663852886e38),
           ('<d', 3.141592653589793), ('>d', 1e308), ('<d', 0.1),
           ('<d', -1.7976931348623157e308), ('<d', 5e-324),
           ('<e', 1.5), ('<e', 65504.0), ('<e', 6e-8), ('<e', 0.1)]
for _f, _v in _FLOATS:
    show("pack %-4s %-24r" % (_f, _v), lambda f=_f, v=_v: _struct.pack(f, v))
    show("  round trip",
         lambda f=_f, v=_v: _struct.unpack(f, _struct.pack(f, v)))
show("pack inf", lambda: _struct.unpack('<d', _struct.pack('<d', float('inf'))))
show("pack -inf", lambda: _struct.unpack('<d', _struct.pack('<d', float('-inf'))))
show("pack float overflow", lambda: _struct.pack('<f', 1e300))
show("pack float non-num", lambda: _struct.pack('<f', 'x'))

# --- the rest of the codes
show("bool", lambda: (_struct.pack('<?', True), _struct.pack('<?', False)))
show("bool back", lambda: _struct.unpack('<2?', b'\x01\x00'))
show("char", lambda: (_struct.pack('<c', b'x'), _struct.unpack('<c', b'x')))
show("char bad", lambda: _struct.pack('<c', b'xy'))
show("bytes pad", lambda: _struct.pack('<5s', b'ab'))
show("bytes trunc", lambda: _struct.pack('<5s', b'abcdefgh'))
show("bytes back", lambda: _struct.unpack('<5s', b'ab\x00\x00\x00'))
show("pascal", lambda: _struct.pack('<3p', b'ab'))
show("pascal back", lambda: _struct.unpack('<3p', _struct.pack('<3p', b'ab')))
show("pad", lambda: _struct.pack('<ixi', 1, 2))
show("pad back", lambda: _struct.unpack('<ixi', _struct.pack('<ixi', 1, 2)))

# --- the module-level API
show("pack_into", lambda: (lambda b: (_struct.pack_into('<2i', b, 0, 7, 9),
                                      bytes(b))[1])(bytearray(8)))
show("pack_into short",
     lambda: _struct.pack_into('<2i', bytearray(4), 0, 7, 9))
show("unpack_from", lambda: _struct.unpack_from('<h', b'\x01\x00\x02\x00', 2))
show("unpack_from short", lambda: _struct.unpack_from('<q', b'ab'))
show("iter_unpack", lambda: list(_struct.iter_unpack('<h', b'\x01\x00\x02\x00')))
show("iter_unpack ragged", lambda: list(_struct.iter_unpack('<h', b'\x01')))
show("unpack wrong size", lambda: _struct.unpack('<i', b'ab'))
show("too few args", lambda: _struct.pack('<2i', 1))
show("too many args", lambda: _struct.pack('<i', 1, 2))

_S = _struct.Struct('<ii')
show("Struct size", lambda: _S.size)
show("Struct format", lambda: _S.format)
show("Struct pack", lambda: _S.pack(1, 2))
show("Struct unpack", lambda: _S.unpack(_S.pack(1, 2)))
show("clearcache", lambda: _struct._clearcache())

# --- bytes.fromhex, which binascii is built on
show("fromhex", lambda: bytes.fromhex("48656c6c6f"))
show("fromhex upper", lambda: bytes.fromhex("48656C6C6F"))
show("fromhex spaces", lambda: bytes.fromhex("48 65 6c"))
show("fromhex empty", lambda: bytes.fromhex(""))
show("fromhex odd", lambda: bytes.fromhex("abc"))
show("fromhex bad", lambda: bytes.fromhex("zz"))
show("fromhex split pair", lambda: bytes.fromhex("4 8"))
show("fromhex non-str", lambda: bytes.fromhex(5))
show("bytearray.fromhex", lambda: bytearray.fromhex("4865"))
show("bytearray.fromhex type",
     lambda: type(bytearray.fromhex("4865")).__name__)
show("fromhex all", lambda: bytes.fromhex(bytes(range(256)).hex())
     == bytes(range(256)))

# --- binascii
show("hexlify", lambda: binascii.hexlify(b"Hello"))
show("unhexlify", lambda: binascii.unhexlify(b"48656c6c6f"))
show("hexlify str arg", lambda: binascii.a2b_hex("00ff"))
show("unhexlify odd", lambda: binascii.a2b_hex(b"abc"))
show("unhexlify bad", lambda: binascii.a2b_hex(b"zz"))
show("b64", lambda: binascii.b2a_base64(b"Hello"))
show("b64 no newline", lambda: binascii.b2a_base64(b"Hello", newline=False))
show("b64 back", lambda: binascii.a2b_base64(b"SGVsbG8=\n"))


def b64_roundtrip():
    for s in (b"", b"a", b"ab", b"abc", b"abcd", b"abcde", bytes(range(64))):
        if binascii.a2b_base64(binascii.b2a_base64(s)) != s:
            return "differs at %r" % (s,)
    return "ok"


show("b64 roundtrip", b64_roundtrip)
show("crc32", lambda: binascii.crc32(b"hello"))
show("crc32 seeded", lambda: binascii.crc32(b"world", binascii.crc32(b"hello")))
show("crc32 empty", lambda: binascii.crc32(b""))
show("crc_hqx", lambda: binascii.crc_hqx(b"hello", 0))
show("qp", lambda: binascii.b2a_qp(b"a=b\n"))
show("qp back", lambda: binascii.a2b_qp(b"a=3Db\n"))
show("uu", lambda: binascii.b2a_uu(b"Hello"))
show("uu back", lambda: binascii.a2b_uu(binascii.b2a_uu(b"Hello")))
show("Error is ValueError", lambda: issubclass(binascii.Error, ValueError))

# --- dir() with no arguments, which pickle calls at import
def scope():
    local_one = 1
    return dir()


show("dir()", scope)

print("done")
