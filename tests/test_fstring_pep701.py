"""PEP 701: the f-string grammar Python 3.12 actually has.

Before 3.12 a replacement field could not contain the quote character that
opened the f-string, so the token ended at the first unescaped copy of it and a
plain scan found it.  3.12 lifted that, along with the bans on backslashes,
comments and newlines inside a field, and on nesting f-strings.  Finding the
end of the token therefore means understanding the whole grammar of what is
inside it, which is what src/compiler/fstrscan.asm does.

Every case here failed to compile before that file existed.  The ones that
already worked are here too, because the scanner that finds the new cases is
the same one that has to keep finding the old ones -- a format spec is a
different language from an expression (`#` and quotes are literal in it, and
there is no `{{` escape), and `:` only starts one at bracket depth zero, which
is why `f"{d[1:2]}"` still means what it says.

This file is run from source by `make check-source`, which is the run that
matters: a .pyc hands the interpreter bytecode CPython's tokenizer produced,
and never reaches ours at all.
"""

v = 42
w = 5
d = {"a": 7, "}": 9, "{": 8}
cmd = ["a", "b"]


def show(label, value):
    print(label, "=>", repr(value))


print("--- the quote that opened the f-string, reused inside ---")
show("join", f"{" ".join(cmd)!r}")
show("subscript", f"{d["a"]}")
show("dict display", f"{ {"a": 1}["a"] }")
show("conditional", f"{ "yes" if v else "no" }")
show("lambda", f"{ (lambda: "q")() }")
show("comprehension", f"{ [c for c in "ab"] }")
show("nested subscript", f"{ {"k": [1, 2]}["k"][0] }")
show("brace in a string", f"{ "}" }")
show("open brace in a string", f"{ "{" }")
show("hash in a string", f"{ "#" }")
show("a key that is a brace", f"{ d["}"] }")


print("--- nested f-strings ---")
show("one deep", f"{f"{v}"}")
show("mixed quotes", f"{f'{v}'}")
show("two deep", f"{f"{f"{v}"}"}")
show("with a spec", f"{f"{v:>{w}}"}")


print("--- a newline inside a field ---")
show("arithmetic", f"{1 +
2}")
show("a call", f"{
  max(
    1,
    2,
  )
}")
show("still one token", f"{v}{
v}")


print("--- a comment inside a field ---")
show("comment", f"{ v # the answer
}")
show("comment then more", f"{ [1,  # first
                             2] }")


print("--- backslashes inside a field ---")
show("escaped quote", f"{ "a\"b" }")
show("newline in a nested string", f"{ "\n".join(["a", "b"])!r }")
show("raw nested", f"{ r"a\b" }")
show("bytes nested", f"{ b"ab" }")


print("--- triple quotes, inside and out ---")
show("outer triple", f"""{ "x" }""")
show("nested triple", f"{ '''y''' }")
show("outer triple, inner double", f"""{ "z" }""")


print("--- format specs are their own language ---")
show("nested width", f"{v:{w}}")
show("conv then nested", f"{v!r:>{w}}")
show("two nested", f"{v:{w}{"d"}}")
show("precision", f"{v!s:{w}.{w}}")
show("alternate form", f"{v:#x}")
show("literal braces in a spec", f"{v:{{w}}}" if False else "skipped")
show("subscript then spec", f"{d["a"]!r:{w}}")


print("--- what still means what it used to ---")
show("slice", f"{ [1,2,3][1:2] }")
show("dict literal", f"{ {1: 2} }")
show("set", f"{ {1, 2} }")
show("doubled braces", f"{{lit}}")
show("named escape", f"\N{BULLET}")
show("named escape and fields", f"{v}\N{BULLET}{v}")
show("raw named escape", rf"\N{v}")
show("debug", f"{v=}")
show("debug with a subscript", f"{d["a"]=}")
show("empty spec", f"{v:}")
show("just text", f"no fields here")
show("adjacent", f"{v}" "plain" f"{w}")


print("--- errors are still errors ---")
for src in ('f"{"',
            'f"{v"',
            'f"unterminated',
            'f"{v!z}"',
            'f"}"',
            'f"{}"'):
    try:
        compile(src, "<t>", "eval")
        print(src, "=> compiled - wrong")
    except SyntaxError as e:
        print(src, "=> SyntaxError")

print("done")
