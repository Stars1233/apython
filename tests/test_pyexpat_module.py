# pyexpat's module surface: the constants, the two submodules, the exception
# and the parser object -- everything except parsing, which the next commit
# adds.
#
# In 3.12 there is no fallback for this module.  `xml.etree` has no non-expat
# builder (`SimpleXMLTreeBuilder` was removed in 3.9), `xml.sax`'s only
# registered parser is `expatreader`, and `xml.dom.minidom`, `plistlib` and
# `xmlrpc.client` all reach it too -- so before this, `plistlib` and
# `xmlrpc.client` raised ModuleNotFoundError on import and `ET.fromstring`
# raised `ImportError: No module named expat; use SimpleXMLTreeBuilder
# instead`, naming a class that has not existed for three releases.
#
# Two things are deliberately NOT compared against python3.  The number of
# XML_ERROR_* names, because CPython's pyexpat transcribes them from the
# header it was BUILT against and this reads them from the library that is
# LINKED -- 45 here, 43 in the system python3, which is a difference between
# two CPython builds rather than between CPython and us.  And the expat
# version, for the same reason: only its shape is asserted.

import pyexpat
import types

print("== the version comes from the linked library ==")
print("EXPAT_VERSION looks right:", pyexpat.EXPAT_VERSION.startswith("expat_"))
print("version_info is three ints:",
      isinstance(pyexpat.version_info, tuple),
      len(pyexpat.version_info) == 3,
      all(isinstance(n, int) for n in pyexpat.version_info))
print("and agrees with the string:",
      pyexpat.EXPAT_VERSION == "expat_%d.%d.%d" % pyexpat.version_info)
print("native_encoding:", pyexpat.native_encoding)

print()
print("== the exception is one class under two names ==")
print("error is ExpatError:", pyexpat.error is pyexpat.ExpatError)
print("is an Exception:", issubclass(pyexpat.ExpatError, Exception))
# The three attributes are set on the instance when the PARSER raises; a
# hand-built one has none of them, in CPython too.
e = pyexpat.ExpatError("boom")
print("a hand-built one has no .code:",
      [hasattr(e, n) for n in ("code", "lineno", "offset")])
print("and is just an Exception:", str(e))

print()
print("== the errors submodule ==")
print("is a module:", isinstance(pyexpat.errors, types.ModuleType))
for name in ("XML_ERROR_SYNTAX", "XML_ERROR_NO_ELEMENTS",
             "XML_ERROR_INVALID_TOKEN", "XML_ERROR_UNCLOSED_TOKEN",
             "XML_ERROR_TAG_MISMATCH", "XML_ERROR_DUPLICATE_ATTRIBUTE",
             "XML_ERROR_JUNK_AFTER_DOC_ELEMENT", "XML_ERROR_UNDEFINED_ENTITY",
             "XML_ERROR_UNKNOWN_ENCODING", "XML_ERROR_NOT_STANDALONE",
             "XML_ERROR_UNBOUND_PREFIX", "XML_ERROR_FINISHED"):
    print("%-34s %r" % (name, getattr(pyexpat.errors, name)))

print()
print("== codes and messages are each other's inverse ==")
codes = pyexpat.errors.codes
messages = pyexpat.errors.messages
print("both are dicts:", isinstance(codes, dict), isinstance(messages, dict))
print("same size:", len(codes) == len(messages))
print("round trip, for every entry:",
      all(codes[messages[c]] == c for c in messages))
print("and the named constants are the keys of codes:",
      pyexpat.errors.XML_ERROR_SYNTAX in codes,
      codes[pyexpat.errors.XML_ERROR_SYNTAX] == 2)

print()
print("== ErrorString comes from the library, not a table here ==")
print("ErrorString(2):", repr(pyexpat.ErrorString(2)))
print("agrees with errors:", pyexpat.ErrorString(2) == pyexpat.errors.XML_ERROR_SYNTAX)
print("every code in messages agrees:",
      all(pyexpat.ErrorString(c) == messages[c] for c in messages))
print("an unknown code:", repr(pyexpat.ErrorString(9999)))
print("code 0 has no message:", repr(pyexpat.ErrorString(0)))

print()
print("== the model submodule ==")
print("is a module:", isinstance(pyexpat.model, types.ModuleType))
for name in sorted(n for n in dir(pyexpat.model) if n.startswith("XML_")):
    print("%-18s %d" % (name, getattr(pyexpat.model, name)))

print()
print("== the param-entity-parsing constants ==")
print(pyexpat.XML_PARAM_ENTITY_PARSING_NEVER,
      pyexpat.XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE,
      pyexpat.XML_PARAM_ENTITY_PARSING_ALWAYS)

print()
print("== a parser, and what it says about itself ==")
p = pyexpat.ParserCreate()
print("type:", type(p).__name__)
print("XMLParserType is its type:", pyexpat.XMLParserType is type(p))
print("intern is a real dict:", isinstance(p.intern, dict), p.intern == {})
print("buffer_text:", p.buffer_text, type(p.buffer_text).__name__)
print("buffer_size:", p.buffer_size)
print("buffer_used:", p.buffer_used)
print("ordered_attributes:", p.ordered_attributes,
      type(p.ordered_attributes).__name__)
print("specified_attributes:", p.specified_attributes,
      type(p.specified_attributes).__name__)
print("namespace_prefixes:", p.namespace_prefixes,
      type(p.namespace_prefixes).__name__)
print("positions before any parse:",
      p.ErrorCode, p.ErrorLineNumber, p.ErrorColumnNumber, p.ErrorByteIndex)
print("current before any parse:",
      p.CurrentLineNumber, p.CurrentColumnNumber, p.CurrentByteIndex)

print()
print("== all 22 handlers start as None and read back as themselves ==")
HANDLERS = ("StartElementHandler", "EndElementHandler",
            "ProcessingInstructionHandler", "CharacterDataHandler",
            "UnparsedEntityDeclHandler", "NotationDeclHandler",
            "StartNamespaceDeclHandler", "EndNamespaceDeclHandler",
            "CommentHandler", "StartCdataSectionHandler",
            "EndCdataSectionHandler", "DefaultHandler", "DefaultHandlerExpand",
            "NotStandaloneHandler", "ExternalEntityRefHandler",
            "StartDoctypeDeclHandler", "EndDoctypeDeclHandler",
            "EntityDeclHandler", "XmlDeclHandler", "ElementDeclHandler",
            "AttlistDeclHandler", "SkippedEntityHandler")
print("count:", len(HANDLERS))
print("all default to None:", all(getattr(p, h) is None for h in HANDLERS))


def make(tag):
    def handler(*args):
        return tag
    return handler


funcs = {h: make(h) for h in HANDLERS}
for h in HANDLERS:
    setattr(p, h, funcs[h])
print("each reads back as the exact object set:",
      all(getattr(p, h) is funcs[h] for h in HANDLERS))
# expatbuilder saves a handler, replaces it, and puts the original back.
saved = p.StartElementHandler
p.StartElementHandler = None
print("cleared:", p.StartElementHandler)
p.StartElementHandler = saved
print("restored to the same object:", p.StartElementHandler is saved)
for h in HANDLERS:
    setattr(p, h, None)
print("all clearable:", all(getattr(p, h) is None for h in HANDLERS))

print()
print("== the flags are settable and come back as bools ==")
for flag in ("buffer_text", "ordered_attributes", "specified_attributes",
             "namespace_prefixes"):
    setattr(p, flag, 1)
    a = getattr(p, flag)
    setattr(p, flag, 0)
    b = getattr(p, flag)
    print("%-22s %r/%r  %s/%s"
          % (flag, a, b, type(a).__name__, type(b).__name__))

print()
print("== buffer_size is validated ==")
p.buffer_size = 4096
print("settable:", p.buffer_size)
for bad in (0, -1, "x", None):
    try:
        p.buffer_size = bad
        print("%-6r ACCEPTED" % bad)
    except (TypeError, ValueError) as e:
        print("%-6r %s: %s" % (bad, type(e).__name__, e))

print()
print("== an unknown attribute is refused, in both directions ==")
# That refusal is load-bearing: xmlrpc.client and expatreader both do
# `try: parser.buffer_text = True / except AttributeError: pass`.
for name in ("nosuchattr", "ErrorCode", "CurrentLineNumber", "intern",
             "buffer_used"):
    try:
        setattr(p, name, 1)
        print("set %-20s ACCEPTED" % name)
    except AttributeError:
        print("set %-20s AttributeError" % name)
try:
    p.nosuchattr
except AttributeError as e:
    print("get nosuchattr:", e)

print()
print("== the private slots are read-only ==")
# `PxHandle.handlers` BORROWS its references and `_handlers` is the only
# owner, so rebinding it dropped the last reference while the core kept
# calling the pointer -- a use-after-free that reached a recycled object and
# called it.  `_h` is the handle the core dereferences.  CPython's parser has
# no such attribute to rebind at all, so refusing is also what it does.
ro = pyexpat.ParserCreate()
ro.StartElementHandler = lambda n, a: None
for name in ("_handlers", "_h", "_intern", "_buffer_text", "_nosuch"):
    try:
        setattr(ro, name, None)
        print("%-14s ACCEPTED" % name)
    except AttributeError:
        print("%-14s AttributeError" % name)
print("and it still parses:", ro.Parse("<a/>", True))

print()
print("== namespace_separator: absent, empty, and one character ==")
# An EMPTY separator is not the same as None: it still builds a
# namespace-aware parser, joining uri and local name with a NUL.
for label, kwargs in (("absent", {}),
                      ("None", {"namespace_separator": None}),
                      ("empty", {"namespace_separator": ""}),
                      ("space", {"namespace_separator": " "}),
                      ("brace", {"namespace_separator": "}"})):
    out = []
    np = pyexpat.ParserCreate(**kwargs)
    np.StartElementHandler = lambda n, a: out.append(n)
    np.Parse('<r xmlns="urn:u"><c/></r>', True)
    print("%-8s %r" % (label, out))
for bad in ("ab", "abc"):
    try:
        pyexpat.ParserCreate(namespace_separator=bad)
        print("%-6r ACCEPTED" % bad)
    except ValueError as e:
        print("%-6r ValueError: %s" % (bad, e))

print()
print("== ParserCreate's arguments ==")
print("with an encoding:", type(pyexpat.ParserCreate("utf-8")).__name__)
print("with a separator:", type(pyexpat.ParserCreate(None, " ")).__name__)
print("with both:", type(pyexpat.ParserCreate("utf-8", "}")).__name__)
shared = {}
q = pyexpat.ParserCreate(None, None, shared)
print("intern is the dict passed in:", q.intern is shared)
for args in ((7,), (None, "ab"), (None, 7)):
    try:
        pyexpat.ParserCreate(*args)
        print("%-14s ACCEPTED" % str(args))
    except (TypeError, ValueError) as e:
        print("%-14s %s" % (str(args), type(e).__name__))

# The `xml` package itself is not exercised here: it is not in lib/, so this
# file has to pass with nothing but our own modules on the path.  That the six
# consumers now import -- `plistlib` and `xmlrpc.client` used to raise
# ModuleNotFoundError, and `ET.fromstring` used to name a builder removed in
# 3.9 -- is what `make check-stdlib` measures, against a real CPython Lib/.
#
# One thing about that import path IS asserted here, because it constrains
# this module rather than the package: `xml.parsers.expat` is
# `from pyexpat import *` followed by bare references to `model` and `errors`,
# so both must be plain module-level names with no leading underscore, and the
# core must be bound under one.
print()
print("== what a star import from here has to expose ==")
_star = {}
exec("from pyexpat import *", _star)
for name in ("ParserCreate", "ErrorString", "ExpatError", "error", "errors",
             "model", "XMLParserType", "EXPAT_VERSION", "version_info",
             "native_encoding", "XML_PARAM_ENTITY_PARSING_ALWAYS"):
    print("%-38s %s" % (name, name in _star))
print("%-38s %s" % ("the core stays private", "_core" not in _star))

print()
print("== many parsers can exist at once, and freeing reuses slots ==")
kept = [pyexpat.ParserCreate() for _ in range(50)]
print("all distinct:", len({id(x) for x in kept}) == 50)
print("each has its own intern:", len({id(x.intern) for x in kept}) == 50)
del kept
print("released without complaint")
