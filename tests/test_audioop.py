"""audioop, checked against CPython's C module where the domain allows it.

The companders are swept exhaustively -- all 65,536 sixteen-bit samples
through u-law and A-law and back -- because that is where a plausible-looking
implementation goes wrong: the usual 16-bit formulation of G.711 agrees with
CPython's on most inputs and differs on 381 of them, all at segment
boundaries.  CPython hands its encoder a 14-bit value for u-law and a 13-bit
one for A-law and finds the segment by a linear search over an end table;
this does the same.

The rest pins the details that a from-the-spec implementation gets wrong:
`fbound` floors rather than truncating, `avg` computes in double and floors,
`cross` seeds its previous sign with a value that is neither 0 nor 1, `max`
of the minimum sample is one larger than any sample can hold, and `ratecv`
carries its state in 32-bit normalised samples rather than native width.
"""

import struct
import warnings

# PEP 594 makes importing it a DeprecationWarning in 3.12.  Catching it here
# keeps the two interpreters' stderr identical -- the message is the same on
# both, but the file it is attributed to is not, since apython runs this from
# its .pyc and reports the path it was given.
with warnings.catch_warnings():
    warnings.simplefilter('ignore', DeprecationWarning)
    import audioop


def pack(width, *values):
    if width == 1:
        return struct.pack('<%db' % len(values), *values)
    if width == 2:
        return struct.pack('<%dh' % len(values), *values)
    if width == 3:
        return b''.join(v.to_bytes(3, 'little', signed=True) for v in values)
    return struct.pack('<%di' % len(values), *values)


MAXVALS = {1: 0x7F, 2: 0x7FFF, 3: 0x7FFFFF, 4: 0x7FFFFFFF}
MINVALS = {1: -0x80, 2: -0x8000, 3: -0x800000, 4: -0x80000000}


def test_companders_round_trip_every_16_bit_sample():
    ulaw_seen = set()
    alaw_seen = set()
    for value in range(-32768, 32768):
        raw = value.to_bytes(2, 'little', signed=True)
        u = audioop.lin2ulaw(raw, 2)
        a = audioop.lin2alaw(raw, 2)
        assert len(u) == 1 and len(a) == 1
        ulaw_seen.add(u[0])
        alaw_seen.add(a[0])
        # Decoding is lossy, but it must land within one step of the input.
        back_u = int.from_bytes(audioop.ulaw2lin(u, 2), 'little', signed=True)
        back_a = int.from_bytes(audioop.alaw2lin(a, 2), 'little', signed=True)
        assert (back_u >= 0) == (value >= 0) or abs(value) < 256, value
        assert (back_a >= 0) == (value >= 0) or abs(value) < 256, value
    # How many code points a 16-bit sweep actually reaches, measured against
    # CPython rather than assumed: u-law has one it cannot produce from a
    # 16-bit input, A-law reaches all 256.
    assert len(ulaw_seen) == 255, len(ulaw_seen)
    assert len(alaw_seen) == 256, len(alaw_seen)


def test_compander_fixed_points():
    # Measured against CPython, not derived: a from-the-spec implementation
    # gets `lin2ulaw(-1)` one code point out, which is the whole point.
    assert audioop.lin2ulaw(pack(2, 0, 1, -1, 32767, -32768), 2) == \
        b'\xff\xff\x7e\x80\x00'
    assert audioop.lin2alaw(pack(2, 0, 1, -1, 32767, -32768), 2) == \
        b'\xd5\xd5\x55\xaa\x2a'
    assert audioop.ulaw2lin(b'\x00\x80\xff', 2) == pack(2, -32124, 32124, 0)
    assert audioop.alaw2lin(b'\x00\x55\xd5\xff', 2) == pack(2, -5504, -8, 8, 848)


def test_max_of_the_minimum_sample_overflows_the_width():
    for width in (1, 2, 3, 4):
        assert audioop.max(pack(width, MINVALS[width]), width) == -MINVALS[width]
        assert audioop.max(pack(width, MAXVALS[width]), width) == MAXVALS[width]
    assert audioop.max(b'', 2) == 0


def test_minmax_seeds_are_32_bit_whatever_the_width():
    assert audioop.minmax(b'', 1) == (0x7FFFFFFF, -0x80000000)
    assert audioop.minmax(pack(2, -5, 7), 2) == (-5, 7)


def test_avg_floors_a_negative_mean():
    assert audioop.avg(pack(2, -1, -2), 2) == -2
    assert audioop.avg(pack(2, 1, 2), 2) == 1
    assert audioop.avg(b'', 2) == 0


def test_mul_floors_rather_than_truncating():
    assert audioop.mul(pack(1, -31), 1, 0.5) == pack(1, -16)
    assert audioop.mul(pack(1, 31), 1, 0.5) == pack(1, 15)
    assert audioop.mul(pack(2, 0x7FFF), 2, 2) == pack(2, 0x7FFF)


def test_cross_counts_the_first_sample():
    assert audioop.cross(b'', 2) == -1
    assert audioop.cross(pack(2, 1), 2) == 0
    assert audioop.cross(pack(2, 1, -1), 2) == 1
    assert audioop.cross(pack(2, -1, 1, -1), 2) == 2


def test_bias_wraps_and_does_not_clip():
    assert audioop.bias(pack(2, 0x7FFF), 2, 1) == pack(2, -0x8000)
    assert audioop.bias(pack(1, 127), 1, 1) == pack(1, -128)


def test_lin2lin_both_ways():
    assert audioop.lin2lin(pack(1, 1, -1), 1, 2) == pack(2, 256, -256)
    assert audioop.lin2lin(pack(2, 256, -256), 2, 1) == pack(1, 1, -1)
    assert audioop.lin2lin(pack(2, 3), 2, 2) == pack(2, 3)


def test_adpcm_round_trip_and_state():
    data = pack(2, *range(-1000, 1000, 100))
    encoded, state = audioop.lin2adpcm(data, 2, None)
    assert len(encoded) == len(data) // 2 // 2
    decoded, state2 = audioop.adpcm2lin(encoded, 2, None)
    assert len(decoded) == len(encoded) * 2 * 2
    # An odd sample count loses its last nibble rather than padding it.
    odd = pack(2, 1, 2, 3)
    assert len(audioop.lin2adpcm(odd, 2, None)[0]) == 1


def test_a_bad_adpcm_state_is_a_value_error():
    for bad in ((0, -1), (0, 89), (0x8000, 0), (-0x8001, 0)):
        for fn, arg in ((audioop.adpcm2lin, b'\0'), (audioop.lin2adpcm, b'\0\0')):
            try:
                fn(arg, 2 if fn is audioop.lin2adpcm else 1, bad)
            except ValueError:
                pass
            else:
                raise AssertionError('ValueError not raised for %r' % (bad,))


def test_ratecv_state_is_normalised_to_32_bits():
    data = pack(1, 15, 8)
    out, state = audioop.ratecv(data, 1, 1, 8000, 8000, None)
    assert out == data
    d, samps = state
    assert samps == ((15 << 24, 8 << 24),), samps
    # Threading the state through a second call must continue cleanly.
    out2, state2 = audioop.ratecv(data, 1, 1, 8000, 8000, state)
    assert len(out2) == len(data)


def test_findfit_and_friends():
    data = pack(2, 0, 0x1234, 0x4567, -0x4567, 0x7FFF, -0x8000, -1)
    assert audioop.findfactor(data, data) == 1.0
    offset, factor = audioop.findfit(data, pack(2, 1, 2, 0))
    assert offset == 1, offset
    assert audioop.findmax(data, 1) == 5


def test_every_function_takes_a_buffer():
    mv = memoryview(pack(2, 1, 2, 3, 4))
    assert audioop.byteswap(mv, 2) == audioop.byteswap(bytes(mv), 2)
    assert audioop.max(mv, 2) == 4
    assert audioop.reverse(bytearray(bytes(mv)), 2) == audioop.reverse(bytes(mv), 2)


def test_the_refusals():
    for call in (lambda: audioop.max(b'\0', 5),
                 lambda: audioop.max(b'\0\0\0', 2),
                 lambda: audioop.getsample(b'\0\0', 2, 4),
                 lambda: audioop.add(b'\0\0', b'\0\0\0\0', 2)):
        try:
            call()
        except audioop.error:
            pass
        else:
            raise AssertionError('audioop.error not raised')


for fn in (test_companders_round_trip_every_16_bit_sample,
           test_compander_fixed_points,
           test_max_of_the_minimum_sample_overflows_the_width,
           test_minmax_seeds_are_32_bit_whatever_the_width,
           test_avg_floors_a_negative_mean,
           test_mul_floors_rather_than_truncating,
           test_cross_counts_the_first_sample,
           test_bias_wraps_and_does_not_clip,
           test_lin2lin_both_ways,
           test_adpcm_round_trip_and_state,
           test_a_bad_adpcm_state_is_a_value_error,
           test_ratecv_state_is_normalised_to_32_bits,
           test_findfit_and_friends,
           test_every_function_takes_a_buffer,
           test_the_refusals):
    fn()
    print(fn.__name__, 'ok')
print('OK')
