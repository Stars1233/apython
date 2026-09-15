"""A NameError whose name is long enough to fill the message buffer.

The message is built in a stack buffer, and the copy of the name into it was
unbounded.  `if True: ` + `a` * 256 is a valid statement, its NameError carries
a 256-character name, and the prefix and suffix around it ran past the frame --
the crash then landed in a DECREF of a frame field holding the tail of the
message rather than anywhere near the copy.
"""


def main():
    for n in (1, 10, 200, 255, 256, 257, 400):
        name = "a" * n
        try:
            exec("if True: " + name)
        except NameError as e:
            msg = str(e)
            print(n, msg.startswith("name '"), msg.endswith("' is not defined"),
                  len(msg))

    # A long attribute name goes through a different builder; check it too.
    class C:
        pass

    for n in (200, 300):
        try:
            getattr(C(), "b" * n)
        except AttributeError as e:
            print("attr", n, "object has no attribute" in str(e))

    # And an unbound local, which is the third message with a name in it.
    def f():
        print(zzzzzzzz)  # noqa: F821
        zzzzzzzz = 1     # noqa: F841

    try:
        f()
    except UnboundLocalError as e:
        print("unbound:", "zzzzzzzz" in str(e))

    print("alive")


main()
