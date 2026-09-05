# _csv, which is Python here rather than C.
#
# CPython's csv.py is written against a C _csv: Dialect validates, the
# reader is a state machine over the characters of each line, and the
# writer joins the fields back with the quoting the dialect asks for.  None
# of that needs assembly, so it is a lib/ module -- and this file drives it
# directly, because tests/ has no csv.py of its own.  The higher layers
# (DictReader, DictWriter, Sniffer) are CPython's own file and are exercised
# by make check-stdlib.

import _csv as csv
import io

def rt(rows, **kw):
    out = io.StringIO()
    w = csv.writer(out, **kw)
    w.writerows(rows)
    s = out.getvalue()
    back = list(csv.reader(io.StringIO(s), **kw))
    return s, back

CASES = [
    ([[1, 2, 3]], {}),
    ([["a,b", "c"]], {}),
    ([['q"x', "y"]], {}),
    ([["", ""]], {}),
    ([[""]], {}),
    ([[None, 1]], {}),
    ([["a\nb", "c"]], {}),
    ([["a", "b"]], {"delimiter": "|"}),
    ([["a", "b"]], {"quoting": csv.QUOTE_ALL}),
    ([["a", 1, 2.5]], {"quoting": csv.QUOTE_NONNUMERIC}),
    ([["a|b"]], {"delimiter": "|", "escapechar": "\\", "quoting": csv.QUOTE_NONE}),
    ([["a", "b"]], {"lineterminator": "\n"}),
    ([[" a ", "b"]], {"skipinitialspace": True}),
    ([['x"y']], {"doublequote": False, "escapechar": "\\"}),
]
for rows, kw in CASES:
    try:
        print(rows, kw, "->", rt(rows, **kw))
    except Exception as e:
        print(rows, kw, "->", type(e).__name__, e)

print(sorted(csv.list_dialects()))
print(csv.field_size_limit())
csv.register_dialect("pipe", delimiter="|")
print(sorted(csv.list_dialects()))
print(list(csv.reader(io.StringIO("a|b\n"), "pipe")))
csv.unregister_dialect("pipe")
try:
    csv.get_dialect("pipe")
except csv.Error as e:
    print("gone:", e)

for bad in ({"delimiter": "ab"}, {"delimiter": None}, {"quoting": 99},
            {"quotechar": None}, {"lineterminator": None}):
    try:
        csv.reader(io.StringIO("a\n"), **bad)
        print(bad, "-> accepted")
    except (TypeError, csv.Error) as e:
        print(bad, "->", type(e).__name__)

r = csv.reader(io.StringIO("a,b\nc,d\n"))
for row in r:
    print(r.line_num, row)

