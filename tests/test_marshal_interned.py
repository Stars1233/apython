# A non-ASCII identifier reaches the .pyc as marshal's TYPE_INTERNED, which is
# a str that happened to be interned when it was written -- not a byte string.
# Decoding it as bytes made every such name come back as bytes, and which
# strings CPython interns varies between patch releases, so the same source
# worked on one machine and not another.
héllo = "wörld"


class Ünicode:
    café = 1
    naïve = 2

    def méthode(self):
        return "ok"


print(héllo, Ünicode.café, Ünicode.naïve, Ünicode().méthode())
print(sorted(k for k in Ünicode.__dict__ if not k.startswith("_")))
print([type(k).__name__ for k in Ünicode.__dict__ if not k.startswith("_")])
print(type(héllo).__name__, len(héllo), héllo[0], ord(héllo[1]))

# The same string as a value, and as a name, are equal and hash together.
d = {"café": 1}
print(d["café"], "café" in d, getattr(Ünicode, "café"))
print(hasattr(Ünicode, "naïve"), getattr(Ünicode, "naïve"))

# ord() on a non-ASCII constant: bytes would have raised TypeError here.
print(ord("é"), ord("日"), ord("€"))
