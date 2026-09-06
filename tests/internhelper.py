# The second module the intern test needs.  A single file cannot see
# over-interning at all: everything in one .pyc already shares through
# marshal's FLAG_REF back-references, so the interesting question -- whether
# two SEPARATELY compiled modules end up with one object or two -- can only be
# asked across a module boundary.

NAME = "hello"                  # all name chars: shared
UNDERSCORED = "_private_name"   # all name chars: shared
DIGITS = "abc123"               # all name chars: shared
PHRASE = "hello world"          # a space: NOT shared
PUNCT = "hello!"                # punctuation: NOT shared
ACCENT = "ete"                  # rewritten below to carry an accent
EMPTY = ""

ACCENT = "été"        # non-ASCII: NOT shared


class Holder:
    def __init__(self):
        self.shared_attr = 1
        self.other = 2

    def read(self):
        # A second method reading an attribute the constructor wrote.  These
        # are different CompUnits, so before interning they held two different
        # objects for one name.
        return self.shared_attr + self.other


def make():
    return Holder()
