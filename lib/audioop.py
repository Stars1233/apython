"""audioop - arithmetic over raw PCM fragments, in Python.

CPython ships this as a C extension.  Nothing in it needs to be C here: every
function is integer arithmetic over a bytes object, and the two things that
look like they need a table -- the u-law/A-law companders and IMA ADPCM --
are small enough to write out.

What it unblocks is not itself: `aifc` and `sunau` import it at module scope,
so its absence was 276 tests that never ran across three modules rather than
31 in this one.

Every result here is checked against CPython's own, exhaustively where the
domain allows it: tests/test_audioop.py walks all 65,536 16-bit sample values
through each compander and back.

Fragments are little-endian signed samples of width 1, 2, 3 or 4.  The
refusals are CPython's own sentences, because callers match on them.
"""


import math as _math
import warnings as _warnings

# PEP 594: CPython 3.12 warns on import and removes the module in 3.13.  The
# warning is part of the module's behaviour, not decoration -- test_audioop
# reaches it through `warnings_helper.import_deprecated`, and a module that
# does not warn is a module that test cannot import the way it means to.
_warnings.warn("'audioop' is deprecated and slated for removal in "
               "Python 3.13", DeprecationWarning, stacklevel=2)
del _warnings


class error(Exception):
    pass


_MAXVALS = {1: 0x7F, 2: 0x7FFF, 3: 0x7FFFFF, 4: 0x7FFFFFFF}
_MINVALS = {1: -0x80, 2: -0x8000, 3: -0x800000, 4: -0x80000000}


def _as_bytes(fragment):
    """Any buffer, seen as bytes.

    CPython takes a Py_buffer, so every one of these accepts a memoryview or
    a bytearray as readily as a bytes -- and test_audioop passes memoryviews
    to all of them.  One copy at the door is what makes the slicing below
    safe; a memoryview refuses a negative step.
    """
    if isinstance(fragment, bytes):
        return fragment
    return bytes(fragment)


def _check_size(size):
    if size not in (1, 2, 3, 4):
        raise error("Size should be 1, 2, 3 or 4")
    return size


def _check_params(length, size):
    _check_size(size)
    if length % size != 0:
        raise error("not a whole number of frames")


def _get_sample(fragment, size, index):
    """The signed sample at frame `index`."""
    start = index * size
    return int.from_bytes(fragment[start:start + size], 'little', signed=True)


def _put_sample(out, size, value):
    out += value.to_bytes(size, 'little', signed=True)


def _clip(value, size):
    """Saturate an already-integral value."""
    maxval = _MAXVALS[size]
    minval = _MINVALS[size]
    if value > maxval:
        return maxval
    if value < minval:
        return minval
    return value


def _fbound(val, size):
    """CPython's fbound: saturate, then floor.

    Two details that are not obvious and both show: the low edge is
    `val < minval + 1.0`, not `val < minval`; and the rounding is toward
    MINUS INFINITY rather than toward zero, so `mul(b'\xe1', 1, 0.5)` --
    -31 * 0.5 -- is -16 and not the -15 a C cast would give.
    """
    import math
    maxval = float(_MAXVALS[size])
    minval = float(_MINVALS[size])
    if val > maxval:
        val = maxval
    elif val < minval + 1.0:
        val = minval
    return int(math.floor(val))


def _samples(fragment, size):
    for i in range(0, len(fragment), size):
        yield int.from_bytes(fragment[i:i + size], 'little', signed=True)


def _build(values, size):
    out = bytearray()
    for v in values:
        out += v.to_bytes(size, 'little', signed=True)
    return bytes(out)


# ---------------------------------------------------------------------------
# Inspection
# ---------------------------------------------------------------------------

def getsample(fragment, width, index):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    if index < 0 or index >= len(fragment) // width:
        raise error("Index out of range")
    return _get_sample(fragment, width, index)


def max(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    biggest = 0
    for value in _samples(fragment, width):
        # The magnitude, NOT saturated: audioop.max of the minimum sample is
        # 128 for width 1, which is one more than any sample can hold.
        if value < 0:
            value = -value
        if value > biggest:
            biggest = value
    return biggest


def minmax(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    # The seeds are CPython's, and they are what an EMPTY fragment answers:
    # (0x7fffffff, -0x80000000) whatever the width.
    smallest = 0x7FFFFFFF
    biggest = -0x80000000
    for value in _samples(fragment, width):
        if value > biggest:
            biggest = value
        if value < smallest:
            smallest = value
    return smallest, biggest


def avg(fragment, width):
    """The mean, floored -- and computed in DOUBLE, as audioop.c does.

    Both halves of that matter: `floor` rather than truncation puts a
    negative mean one lower, and the double is what makes a wide sum lose
    its last bits the same way CPython's does.
    """
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    count = len(fragment) // width
    if count == 0:
        return 0
    total = 0.0
    for value in _samples(fragment, width):
        total += value
    import math
    return int(math.floor(total / float(count)))


def rms(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    count = len(fragment) // width
    if count == 0:
        return 0
    total = 0
    for value in _samples(fragment, width):
        total += value * value
    return _isqrt(total // count)


def _isqrt(n):
    if n <= 0:
        return 0
    root = int(n ** 0.5)
    # Correct the float estimate; the C version computes in double too but
    # the answer has to be the integer square root either way.
    while root * root > n:
        root -= 1
    while (root + 1) * (root + 1) <= n:
        root += 1
    return root


def cross(fragment, width):
    """How many times the signal crosses zero.  -1 for an empty fragment.

    The seed is 17 rather than 0 or 1, so the FIRST sample always counts as a
    change and cancels the -1; starting at 1 got the first frame wrong
    whenever it was non-negative.
    """
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    crossings = -1
    previous = 17
    for value in _samples(fragment, width):
        sign = 1 if value < 0 else 0
        if sign != previous:
            crossings += 1
        previous = sign
    return crossings


def _extreme_diffs(fragment, width):
    """The successive peak-to-peak differences, as audioop.c computes them.

    The derivative's SIGN is what marks an extreme -- a repeated sample is not
    a turning point, which is why equal values are skipped rather than ending
    a run -- and the difference is taken in unsigned 32-bit arithmetic, which
    is what makes it an absolute value without a branch.
    """
    if len(fragment) <= width:
        return
    samples = list(_samples(fragment, width))
    prevval = samples[0]
    prevdiff = 17                   # anything that is neither 0 nor 1
    prevextreme = 0
    prevextremevalid = False
    for val in samples[1:]:
        if val == prevval:
            continue
        diff = 1 if val < prevval else 0
        if prevdiff == (0 if diff else 1):
            if prevextremevalid:
                if prevval < prevextreme:
                    yield prevextreme - prevval
                else:
                    yield prevval - prevextreme
            prevextremevalid = True
            prevextreme = prevval
        prevval = val
        prevdiff = diff


def avgpp(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    total = 0.0
    n = 0
    for delta in _extreme_diffs(fragment, width):
        total += float(delta)
        n += 1
    if n == 0:
        return 0
    return int(total / float(n))


def maxpp(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    biggest = 0
    for delta in _extreme_diffs(fragment, width):
        if delta > biggest:
            biggest = delta
    return biggest


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------

def add(fragment1, fragment2, width):
    fragment1 = _as_bytes(fragment1)
    fragment2 = _as_bytes(fragment2)
    _check_params(len(fragment1), width)
    if len(fragment1) != len(fragment2):
        raise error("Lengths should be the same")
    out = bytearray()
    for a, b in zip(_samples(fragment1, width), _samples(fragment2, width)):
        out += _clip(a + b, width).to_bytes(width, 'little', signed=True)
    return bytes(out)


def bias(fragment, width, bias_):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    # CPython WRAPS here rather than clipping -- audioop_bias_impl adds in
    # unsigned arithmetic of the sample's own width -- so bias(b'\xff\x7f', 2, 1)
    # is b'\x00\x80' and not the saturated b'\xff\x7f'.
    mask = (1 << (width * 8)) - 1
    span = 1 << (width * 8)
    out = bytearray()
    for value in _samples(fragment, width):
        raw = (value + bias_) & mask
        if raw > _MAXVALS[width]:
            raw -= span
        out += raw.to_bytes(width, 'little', signed=True)
    return bytes(out)


def mul(fragment, width, factor):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    for value in _samples(fragment, width):
        out += _fbound(value * factor, width).to_bytes(width, 'little',
                                                       signed=True)
    return bytes(out)


def reverse(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    for i in range(len(fragment) // width - 1, -1, -1):
        out += fragment[i * width:(i + 1) * width]
    return bytes(out)


def byteswap(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    for i in range(0, len(fragment), width):
        out += fragment[i:i + width][::-1]
    return bytes(out)


def tomono(fragment, width, lfactor, rfactor):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    if (len(fragment) // width) % 2:
        raise error("not a whole number of frames")
    out = bytearray()
    samples = list(_samples(fragment, width))
    for i in range(0, len(samples), 2):
        value = samples[i] * lfactor + samples[i + 1] * rfactor
        out += _fbound(value, width).to_bytes(width, 'little', signed=True)
    return bytes(out)


def tostereo(fragment, width, lfactor, rfactor):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    for value in _samples(fragment, width):
        left = _fbound(value * lfactor, width)
        right = _fbound(value * rfactor, width)
        out += left.to_bytes(width, 'little', signed=True)
        out += right.to_bytes(width, 'little', signed=True)
    return bytes(out)


def lin2lin(fragment, width, newwidth):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    _check_size(newwidth)
    if width == newwidth:
        return bytes(fragment)
    out = bytearray()
    shift = (newwidth - width) * 8
    for value in _samples(fragment, width):
        if shift > 0:
            value <<= shift
        else:
            value >>= -shift
        out += value.to_bytes(newwidth, 'little', signed=True)
    return bytes(out)


# ---------------------------------------------------------------------------
# Correlation
# ---------------------------------------------------------------------------

def findfactor(fragment, reference):
    fragment = _as_bytes(fragment)
    reference = _as_bytes(reference)
    if len(fragment) & 1 or len(reference) & 1:
        raise error("Strings should be even-sized")
    if len(fragment) != len(reference):
        raise error("Samples should be same size")
    sum_ri_2 = 0.0
    sum_aij_ri = 0.0
    for a, r in zip(_samples(fragment, 2), _samples(reference, 2)):
        sum_ri_2 += float(r) * float(r)
        sum_aij_ri += float(a) * float(r)
    if sum_ri_2 == 0.0:
        return 0.0
    return sum_aij_ri / sum_ri_2


def findfit(fragment, reference):
    """Where in `fragment` the `reference` fits best, and by what factor.

    Transcribed from audioop.c: the figure minimised is
    `(sum_ri_2*sum_aij_2 - sum_aij_ri^2) / sum_aij_2`, and the running
    sum_aij_2 slides by one sample per step while sum_aij_ri is recomputed.
    An earlier version divided by sum_ri_2 instead and picked the wrong
    offset on half the inputs.
    """
    fragment = _as_bytes(fragment)
    reference = _as_bytes(reference)
    if len(fragment) & 1 or len(reference) & 1:
        raise error("Strings should be even-sized")
    cp1 = list(_samples(fragment, 2))
    cp2 = list(_samples(reference, 2))
    len1 = len(cp1)
    len2 = len(cp2)
    if len1 < len2:
        raise error("First sample should be longer")

    def _sum2(a, b, offset, n):
        total = 0.0
        for i in range(n):
            total += float(a[offset + i]) * float(b[i])
        return total

    def _cdiv(num, den):
        """C's division, which is where audioop.c does this arithmetic.

        A window of pure silence makes `sum_aij_2` zero -- it is a sum of
        squares -- and `sum_aij_ri` zero with it, so C evaluates 0.0/0.0 and
        gets a nan, which loses every `<` comparison and leaves the window
        unchosen.  Python raises ZeroDivisionError there instead, so leading
        silence (the normal shape of a recording) turned findfit into an
        exception.  The reference being silent divides by zero the same way
        at the end, and CPython really does answer nan for it.
        """
        if den == 0.0:
            return float('nan') if num == 0.0 else _math.copysign(
                float('inf'), num)
        return num / den

    sum_ri_2 = _sum2(cp2, cp2, 0, len2)
    sum_aij_2 = _sum2(cp1, cp1, 0, len2)
    sum_aij_ri = _sum2(cp1, cp2, 0, len2)

    result = _cdiv(sum_ri_2 * sum_aij_2 - sum_aij_ri * sum_aij_ri, sum_aij_2)
    best_result = result
    best_j = 0

    for j in range(1, len1 - len2 + 1):
        aj_m1 = float(cp1[j - 1])
        aj_lm1 = float(cp1[j + len2 - 1])
        sum_aij_2 = sum_aij_2 + aj_lm1 * aj_lm1 - aj_m1 * aj_m1
        sum_aij_ri = _sum2(cp1, cp2, j, len2)
        result = _cdiv(sum_ri_2 * sum_aij_2 - sum_aij_ri * sum_aij_ri,
                       sum_aij_2)
        if result < best_result:
            best_result = result
            best_j = j

    # Py_BuildValue's "f" takes a C DOUBLE and builds a Python float from it
    # unnarrowed, whatever its name suggests -- narrowing to single precision
    # here answered 8038.7998046875 where CPython says 8038.8.
    return best_j, _cdiv(_sum2(cp1, cp2, best_j, len2), sum_ri_2)


def findmax(fragment, length):
    fragment = _as_bytes(fragment)
    if len(fragment) & 1:
        raise error("Strings should be even-sized")
    if length < 0:
        raise error("Input sample should be longer")
    n = len(fragment) // 2
    if length > n:
        raise error("Input sample should be longer")
    frag = list(_samples(fragment, 2))
    result = 0.0
    for i in range(length):
        result += float(frag[i]) * float(frag[i])
    best_result = result
    best_offset = 0
    for offset in range(1, n - length + 1):
        out = float(frag[offset - 1])
        into = float(frag[offset + length - 1])
        result += into * into - out * out
        if result > best_result:
            best_result = result
            best_offset = offset
    return best_offset


# ---------------------------------------------------------------------------
# Rate conversion
# ---------------------------------------------------------------------------

def ratecv(fragment, width, nchannels, inrate, outrate, state,
           weightA=1, weightB=0):
    """Resample, with the state a caller threads through successive calls.

    Everything here happens in 32-BIT NORMALISED samples, which is what
    audioop.c's GETSAMPLE32 produces: a width-1 sample is shifted up 24 bits,
    a width-2 one up 16.  That is not an implementation detail a caller can
    ignore, because the state tuple carries those normalised values -- so a
    version that worked in native width produced the right audio and the
    wrong state, and a second call threaded from it drifted.

    The filter and the interpolation are both done in DOUBLE and truncated
    toward zero, exactly as the C does.
    """
    fragment = _as_bytes(fragment)
    _check_size(width)
    if nchannels < 1:
        raise error("# of channels should be >= 1")
    if width * nchannels > 1024:
        raise error("width * nchannels too big for a C int")
    if weightA < 1 or weightB < 0:
        raise error("weightA should be >= 1, weightB should be >= 0")
    if len(fragment) % (width * nchannels) != 0:
        raise error("not a whole number of frames")
    if inrate <= 0 or outrate <= 0:
        raise error("sampling rate not > 0")

    d = _gcd(inrate, outrate)
    inrate //= d
    outrate //= d
    d = _gcd(weightA, weightB)
    weightA //= d
    weightB //= d

    if state is None:
        d = -outrate
        prev_i = [0] * nchannels
        cur_i = [0] * nchannels
    else:
        if not isinstance(state, tuple):
            raise TypeError("state must be a tuple or None")
        d, samps = state
        if len(samps) != nchannels:
            raise error("illegal state argument")
        prev_i = [s[0] for s in samps]
        cur_i = [s[1] for s in samps]

    shift = (4 - width) * 8
    frames = len(fragment) // (width * nchannels)
    samples = list(_samples(fragment, width))
    out = bytearray()
    pos = 0
    while True:
        while d < 0:
            if pos >= frames:
                return (bytes(out),
                        (d, tuple((prev_i[c], cur_i[c])
                                  for c in range(nchannels))))
            for chan in range(nchannels):
                prev_i[chan] = cur_i[chan]
                cur_i[chan] = samples[pos * nchannels + chan] << shift
                cur_i[chan] = int((float(weightA) * float(cur_i[chan])
                                   + float(weightB) * float(prev_i[chan]))
                                  / (float(weightA) + float(weightB)))
            pos += 1
            d += outrate
        while d >= 0:
            for chan in range(nchannels):
                cur_o = int((float(prev_i[chan]) * float(d)
                             + float(cur_i[chan]) * float(outrate - d))
                            / float(outrate))
                out += (cur_o >> shift).to_bytes(width, 'little', signed=True)
            d -= inrate


def _gcd(a, b):
    while b:
        a, b = b, a % b
    return a


# ---------------------------------------------------------------------------
# The companders.  These are the tables, and they are the whole of them.
# ---------------------------------------------------------------------------

# G.711, transcribed from audioop.c rather than from the usual 16-bit
# formulation of it -- the two differ.  CPython normalises the sample to 32
# bits and then hands the encoder a 14-BIT value for u-law and a 13-bit one
# for A-law, and the segment is found by a linear search over an end table
# rather than computed from the exponent.  A 16-bit algorithm agrees with it
# on most inputs and not on 381 of the 65,536, all at segment boundaries.
_ULAW_BIAS = 0x84
_ULAW_CLIP = 32635
_SEG_UEND = (0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF)
_SEG_AEND = (0x1F, 0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF)


def _search(val, table):
    for i, limit in enumerate(table):
        if val <= limit:
            return i
    return len(table)


def _lin2ulaw_sample(pcm_val):
    """A 14-bit two's-complement value to one u-law byte."""
    if pcm_val < 0:
        pcm_val = -pcm_val
        mask = 0x7F
    else:
        mask = 0xFF
    if pcm_val > _ULAW_CLIP:
        pcm_val = _ULAW_CLIP
    pcm_val += _ULAW_BIAS >> 2
    seg = _search(pcm_val, _SEG_UEND)
    if seg >= 8:
        return (0x7F ^ mask) & 0xFF
    uval = (seg << 4) | ((pcm_val >> (seg + 1)) & 0x0F)
    return (uval ^ mask) & 0xFF


def _lin2alaw_sample(pcm_val):
    """A 13-bit two's-complement value to one A-law byte."""
    if pcm_val >= 0:
        mask = 0xD5
    else:
        mask = 0x55
        pcm_val = -pcm_val - 1
    seg = _search(pcm_val, _SEG_AEND)
    if seg >= 8:
        return (0x7F ^ mask) & 0xFF
    aval = seg << 4
    if seg < 2:
        aval |= (pcm_val >> 1) & 0x0F
    else:
        aval |= (pcm_val >> seg) & 0x0F
    return (aval ^ mask) & 0xFF


def _ulaw2lin_sample(byte):
    byte = ~byte & 0xFF
    sign = byte & 0x80
    exponent = (byte >> 4) & 0x07
    mantissa = byte & 0x0F
    sample = ((mantissa << 3) + _ULAW_BIAS) << exponent
    sample -= _ULAW_BIAS
    return -sample if sign else sample


def _alaw2lin_sample(byte):
    byte ^= 0x55
    t = (byte & 0x0F) << 4
    seg = (byte & 0x70) >> 4
    if seg == 0:
        t += 8
    elif seg == 1:
        t += 0x108
    else:
        t += 0x108
        t <<= seg - 1
    # `t` when the sign bit is SET, and the negative otherwise -- G.711's
    # A-law inverts every other bit, so this reads backwards and is right.
    return t if (byte & 0x80) else -t


def lin2ulaw(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    shift = (4 - width) * 8
    for value in _samples(fragment, width):
        out.append(_lin2ulaw_sample((value << shift) >> 18))
    return bytes(out)


def ulaw2lin(fragment, width):
    fragment = _as_bytes(fragment)
    _check_size(width)
    out = bytearray()
    for byte in fragment:
        out += _from16(_ulaw2lin_sample(byte), width)
    return bytes(out)


def lin2alaw(fragment, width):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    out = bytearray()
    shift = (4 - width) * 8
    for value in _samples(fragment, width):
        out.append(_lin2alaw_sample((value << shift) >> 19))
    return bytes(out)


def alaw2lin(fragment, width):
    fragment = _as_bytes(fragment)
    _check_size(width)
    out = bytearray()
    for byte in fragment:
        out += _from16(_alaw2lin_sample(byte), width)
    return bytes(out)


def _to16(value, width):
    """A sample of any width, seen as the 16-bit one the companders take."""
    if width == 1:
        return value << 8
    if width == 2:
        return value
    if width == 3:
        return value >> 8
    return value >> 16


def _from16(value, width):
    if width == 1:
        return _clip(value >> 8, 1).to_bytes(1, 'little', signed=True)
    if width == 2:
        return _clip(value, 2).to_bytes(2, 'little', signed=True)
    if width == 3:
        return _clip(value << 8, 3).to_bytes(3, 'little', signed=True)
    return _clip(value << 16, 4).to_bytes(4, 'little', signed=True)


# IMA ADPCM.  Two tables, both from the specification, both what CPython's
# audioop.c carries verbatim.
_STEP_SIZE = (
    7, 8, 9, 10, 11, 12, 13, 14, 16, 17,
    19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
    50, 55, 60, 66, 73, 80, 88, 97, 107, 118,
    130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
    337, 371, 408, 449, 494, 544, 598, 658, 724, 796,
    876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
    2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
    5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
    15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767,
)

_INDEX_TABLE = (-1, -1, -1, -1, 2, 4, 6, 8,
                -1, -1, -1, -1, 2, 4, 6, 8)


def lin2adpcm(fragment, width, state):
    fragment = _as_bytes(fragment)
    _check_params(len(fragment), width)
    if state is None:
        valpred = 0
        index = 0
    else:
        valpred, index = state
        if not (-0x8000 <= valpred < 0x8000) or not (0 <= index < 89):
            raise ValueError("bad state")

    out = bytearray()
    bufferstep = True
    outputbuffer = 0
    for raw in _samples(fragment, width):
        val = _to16(raw, width)
        step = _STEP_SIZE[index]
        diff = val - valpred
        if diff < 0:
            sign = 8
            diff = -diff
        else:
            sign = 0

        delta = 0
        vpdiff = step >> 3
        if diff >= step:
            delta = 4
            diff -= step
            vpdiff += step
        step >>= 1
        if diff >= step:
            delta |= 2
            diff -= step
            vpdiff += step
        step >>= 1
        if diff >= step:
            delta |= 1
            vpdiff += step

        if sign:
            valpred -= vpdiff
        else:
            valpred += vpdiff
        if valpred > 32767:
            valpred = 32767
        elif valpred < -32768:
            valpred = -32768

        delta |= sign
        index += _INDEX_TABLE[delta]
        if index < 0:
            index = 0
        elif index > 88:
            index = 88

        if bufferstep:
            outputbuffer = (delta << 4) & 0xF0
        else:
            out.append((delta & 0x0F) | outputbuffer)
        bufferstep = not bufferstep

    # No flush: an odd sample count loses its last nibble, which is what
    # CPython does -- the output is exactly len(fragment)//width//2 bytes.
    return bytes(out), (valpred, index)


def adpcm2lin(fragment, width, state):
    fragment = _as_bytes(fragment)
    _check_size(width)
    if state is None:
        valpred = 0
        index = 0
    else:
        valpred, index = state
        if not (-0x8000 <= valpred < 0x8000) or not (0 <= index < 89):
            raise ValueError("bad state")

    out = bytearray()
    bufferstep = False
    inputbuffer = 0
    for i in range(len(fragment) * 2):
        if bufferstep:
            delta = inputbuffer & 0x0F
        else:
            inputbuffer = fragment[i // 2]
            delta = (inputbuffer >> 4) & 0x0F
        bufferstep = not bufferstep

        step = _STEP_SIZE[index]
        index += _INDEX_TABLE[delta]
        if index < 0:
            index = 0
        elif index > 88:
            index = 88

        sign = delta & 8
        delta = delta & 7
        vpdiff = step >> 3
        if delta & 4:
            vpdiff += step
        if delta & 2:
            vpdiff += step >> 1
        if delta & 1:
            vpdiff += step >> 2

        if sign:
            valpred -= vpdiff
        else:
            valpred += vpdiff
        if valpred > 32767:
            valpred = 32767
        elif valpred < -32768:
            valpred = -32768

        out += _from16(valpred, width)
    return bytes(out), (valpred, index)
