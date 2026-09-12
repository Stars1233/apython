# hashlib's other eight digests: sha3_224/256/384/512, shake_128/256,
# blake2b and blake2s.
#
# Before lib/_sha3.py and lib/_blake2.py existed, `import hashlib` SUCCEEDED
# and then printed 64 lines of tracebacks to stderr -- eight rounds of
# logging.exception('code for hash %s was not found') -- after which
# hashlib.sha3_256 and hashlib.blake2b did not exist at all.  What made that
# worse than a plain absence is that `algorithms_available` is a static set in
# hashlib.py and went on advertising all fourteen, so a program that asked
# whether it could have sha3 was told yes and then failed.
#
# blake2 is not optional even once a C _hashlib exists: hashlib keeps
# `__block_openssl_constructor = {'blake2b', 'blake2s'}` and routes both to the
# builtin module unconditionally, because OpenSSL's BLAKE2 supports neither
# keying nor the tree parameters.
#
# The oracle is python3, whose _sha3 and _blake2 are C.  Only digests and the
# declared attributes are compared -- never the type names, which differ by
# design (CPython's come from _hashlib when OpenSSL is present), and never the
# arity-error wording, which is Argument Clinic's.

import hashlib

MSGS = [b"", b"abc", b"a" * 63, b"a" * 64, b"a" * 65, b"a" * 71, b"a" * 72,
        b"a" * 103, b"a" * 104, b"a" * 127, b"a" * 128, b"a" * 135,
        b"a" * 136, b"a" * 143, b"a" * 144, b"a" * 167, b"a" * 168,
        b"a" * 200, bytes(range(256))]

FIXED = ("sha3_224", "sha3_256", "sha3_384", "sha3_512")
XOF = ("shake_128", "shake_256")
BLAKE = ("blake2b", "blake2s")


print("== every algorithm is present and declares itself ==")
for name in FIXED + XOF + BLAKE:
    ctor = getattr(hashlib, name)
    h = ctor()
    print("%-10s name=%-10s digest_size=%-3d block_size=%d"
          % (name, h.name, h.digest_size, h.block_size))

print()
print("== the four constructor attributes blake2 callers read ==")
for name in BLAKE:
    ctor = getattr(hashlib, name)
    print("%-8s SALT_SIZE=%d PERSON_SIZE=%d MAX_KEY_SIZE=%d MAX_DIGEST_SIZE=%d"
          % (name, ctor.SALT_SIZE, ctor.PERSON_SIZE,
             ctor.MAX_KEY_SIZE, ctor.MAX_DIGEST_SIZE))

print()
print("== digests, over every block-boundary length that matters ==")
for name in FIXED + BLAKE:
    ctor = getattr(hashlib, name)
    acc = []
    for m in MSGS:
        acc.append(ctor(m).hexdigest())
    # One line per algorithm: the digests folded together, so a single wrong
    # block boundary shows up without 150 lines of output.
    print("%-10s %s" % (name, hashlib.sha256("".join(acc).encode()).hexdigest()))
    print("%-10s   abc -> %s" % ("", ctor(b"abc").hexdigest()))

print()
print("== the XOFs, at lengths either side of their rate ==")
for name in XOF:
    ctor = getattr(hashlib, name)
    for length in (0, 1, 16, 31, 32, 135, 136, 167, 168, 169, 400):
        print("%-10s len=%-4d %s" % (name, length, ctor(b"abc").hexdigest(length)))

print()
print("== incremental update must equal the one-shot ==")
for name in FIXED + BLAKE:
    ctor = getattr(hashlib, name)
    h = ctor()
    for chunk in (b"a" * 100, b"b" * 50, b"c", b"", b"d" * 200):
        h.update(chunk)
    one = ctor(b"a" * 100 + b"b" * 50 + b"c" + b"d" * 200).hexdigest()
    print("%-10s %s" % (name, h.hexdigest() == one))

print()
print("== copy() is independent, and digest() does not consume ==")
for name in FIXED + BLAKE:
    ctor = getattr(hashlib, name)
    h = ctor(b"ab")
    k = h.copy()
    h.update(b"c")
    k.update(b"Z")
    twice = ctor(b"abc")
    print("%-10s copy=%s %s  repeatable=%s  updatable_after=%s"
          % (name,
             h.hexdigest() == ctor(b"abc").hexdigest(),
             k.hexdigest() == ctor(b"abZ").hexdigest(),
             twice.hexdigest() == twice.hexdigest(),
             (twice.update(b"d") or twice.hexdigest())
             == ctor(b"abcd").hexdigest()))

print()
print("== blake2's whole parameter block ==")
for name in BLAKE:
    ctor = getattr(hashlib, name)
    salt = bytes(range(ctor.SALT_SIZE))
    person = bytes(range(ctor.PERSON_SIZE))
    tree = dict(fanout=2, depth=3, leaf_size=4096, node_offset=7,
                node_depth=1, inner_size=16)
    print("%-8s digest_size=1   %s" % (name, ctor(b"abc", digest_size=1).hexdigest()))
    print("%-8s digest_size=16  %s" % (name, ctor(b"abc", digest_size=16).hexdigest()))
    print("%-8s key             %s" % (name, ctor(b"abc", key=b"kk").hexdigest()))
    print("%-8s key max         %s" % (name, ctor(b"abc", key=bytes(range(ctor.MAX_KEY_SIZE))).hexdigest()))
    print("%-8s key, empty msg  %s" % (name, ctor(b"", key=b"kk").hexdigest()))
    print("%-8s salt            %s" % (name, ctor(b"abc", salt=salt).hexdigest()))
    print("%-8s short salt      %s" % (name, ctor(b"abc", salt=b"x").hexdigest()))
    print("%-8s person          %s" % (name, ctor(b"abc", person=person).hexdigest()))
    print("%-8s tree            %s" % (name, ctor(b"abc", **tree).hexdigest()))
    print("%-8s last_node       %s" % (name, ctor(b"abc", last_node=True).hexdigest()))
    print("%-8s everything      %s" % (name, ctor(b"abc", digest_size=16, key=b"kk",
                                                  salt=salt, person=person,
                                                  last_node=True, **tree).hexdigest()))
    # A keyed hash prepends a padded key BLOCK; doing that again on update or
    # copy is the classic way to get this wrong.
    h = ctor(key=b"kk")
    h.update(b"a")
    h.update(b"bc")
    print("%-8s keyed increment %s" % (name, h.hexdigest() == ctor(b"abc", key=b"kk").hexdigest()))
    print("%-8s keyed copy      %s" % (name, ctor(b"abc", key=b"kk").copy().hexdigest()
                                       == ctor(b"abc", key=b"kk").hexdigest()))

print()
print("== the refusals blake2 makes ==")
for name in BLAKE:
    ctor = getattr(hashlib, name)
    mx = ctor.MAX_DIGEST_SIZE
    for kw in ({"digest_size": 0}, {"digest_size": mx + 1},
               {"key": b"x" * (ctor.MAX_KEY_SIZE + 1)},
               {"salt": b"s" * (ctor.SALT_SIZE + 1)},
               {"person": b"p" * (ctor.PERSON_SIZE + 1)}):
        try:
            ctor(**kw)
        except ValueError as e:
            print("%-8s %-14s ValueError: %s" % (name, list(kw)[0], e))

print()
print("== update() refuses a str, as every other hash does ==")
for name in FIXED + XOF + BLAKE:
    try:
        getattr(hashlib, name)().update("text")
    except TypeError as e:
        print("%-10s TypeError: %s" % (name, e))

print()
print("== hashlib.new() reaches all fourteen, and usedforsecurity= is taken ==")
for name in sorted(hashlib.algorithms_guaranteed):
    h = hashlib.new(name, b"abc")
    out = h.hexdigest(16) if name.startswith("shake") else h.hexdigest()
    print("%-10s %s" % (name, out))
for name in FIXED + XOF + BLAKE:
    getattr(hashlib, name)(b"abc", usedforsecurity=False)
print("usedforsecurity=False accepted by all eight")

print()
print("== and what hashlib says it has ==")
print("guaranteed == available:",
      hashlib.algorithms_guaranteed == set(hashlib.algorithms_guaranteed)
      & hashlib.algorithms_available)
print("every guaranteed name is a real attribute:",
      sorted(n for n in hashlib.algorithms_guaranteed if not hasattr(hashlib, n)))
