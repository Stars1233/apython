# The declaration handlers, the markup handlers and the namespace handlers --
# thirteen of the twenty-two, all built from three macros in
# src/modules/pyexpat_cb.asm because their bodies differ only in how many
# strings arrive and which of them are interned.
#
# That interning split is CPython's and it is not arbitrary: a processing
# instruction's TARGET repeats across a document and its DATA does not, an
# element name repeats and an attribute value does not.  Interning the second
# kind wastes memory instead of saving it, and `parser.intern` is observable,
# so the split is part of the contract rather than a detail.
#
# The document below is one pass over everything at once: an XML declaration
# with all three fields, a doctype with an external id and an internal subset,
# element, attlist, entity and notation declarations, a processing
# instruction, a comment, a namespace declaration, a CDATA section and an
# entity reference.  Comparing the whole event stream against python3 is
# worth more than thirteen separate cases, because the ORDER is most of what
# can go wrong.

import pyexpat

DOC = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
       '<!DOCTYPE root SYSTEM "sys.dtd" [\n'
       '<!ELEMENT root (a|b)*>\n'
       '<!ATTLIST root id ID #REQUIRED cls CDATA "dflt">\n'
       '<!ENTITY ent "expanded">\n'
       '<!ENTITY ext SYSTEM "e.xml">\n'
       '<!NOTATION note SYSTEM "n.dtd">\n'
       '<!ENTITY pic SYSTEM "p.gif" NDATA note>\n'
       ']>\n'
       '<root id="r1" xmlns:x="http://x/"><?pi-target pi data?>'
       '<!-- a comment --><x:child/><![CDATA[raw <stuff>]]>&ent;</root>')

ALL = ("XmlDeclHandler", "StartDoctypeDeclHandler", "EndDoctypeDeclHandler",
       "ElementDeclHandler", "AttlistDeclHandler", "EntityDeclHandler",
       "NotationDeclHandler", "UnparsedEntityDeclHandler",
       "ProcessingInstructionHandler", "CommentHandler",
       "StartCdataSectionHandler", "EndCdataSectionHandler",
       "StartNamespaceDeclHandler", "EndNamespaceDeclHandler",
       "SkippedEntityHandler", "StartElementHandler", "EndElementHandler",
       "CharacterDataHandler", "DefaultHandler")


def run(doc, names, **flags):
    log = []
    p = pyexpat.ParserCreate(**flags)
    for name in names:
        def handler(*args, _n=name):
            log.append((_n, args))
        setattr(p, name, handler)
    try:
        p.Parse(doc, True)
    except pyexpat.ExpatError as e:
        log.append(("ExpatError", str(e)))
    return log


print("== the whole stream, in order ==")
# ElementDeclHandler, EntityDeclHandler and DefaultHandler are not in this
# list: they arrive in a later commit, and asking for one that is not wired
# yet would still register the attribute but never fire, which would make this
# file's output depend on the order the commits landed rather than on the
# parser.
WIRED = tuple(n for n in ALL
              if n not in ("ElementDeclHandler", "EntityDeclHandler",
                           "DefaultHandler"))
for event in run(DOC, WIRED):
    print(event)

print()
print("== the XML declaration's three fields ==")
for doc in ('<?xml version="1.0"?><a/>',
            '<?xml version="1.0" encoding="iso-8859-1"?><a/>',
            '<?xml version="1.0" standalone="no"?><a/>',
            '<?xml version="1.1" encoding="UTF-8" standalone="yes"?><a/>'):
    print(run(doc, ("XmlDeclHandler",)))

print()
print("== a doctype, with and without the ids and the subset ==")
for doc in ('<!DOCTYPE r><r/>',
            '<!DOCTYPE r SYSTEM "s.dtd"><r/>',
            '<!DOCTYPE r PUBLIC "pub" "s.dtd"><r/>',
            '<!DOCTYPE r [<!ELEMENT r EMPTY>]><r/>'):
    print(run(doc, ("StartDoctypeDeclHandler", "EndDoctypeDeclHandler")))

print()
print("== an attlist declaration fires once per ATTRIBUTE ==")
print(run('<!DOCTYPE r [<!ATTLIST r a CDATA #IMPLIED b ID #REQUIRED '
          'c CDATA "d" e (x|y) "x">]><r/>', ("AttlistDeclHandler",)))

print()
print("== notation and unparsed-entity declarations ==")
print(run('<!DOCTYPE r [<!NOTATION n SYSTEM "n.dtd">'
          '<!NOTATION p PUBLIC "pp" "p.dtd">'
          '<!ENTITY e SYSTEM "e.gif" NDATA n>]><r/>',
          ("NotationDeclHandler", "UnparsedEntityDeclHandler")))

print()
print("== processing instructions and comments ==")
print(run('<?before?><!--c1--><r><?inside data?><!--c2--></r><?after?><!--c3-->',
          ("ProcessingInstructionHandler", "CommentHandler")))
print("a PI with no data:", run('<r><?bare?></r>',
                                ("ProcessingInstructionHandler",)))

print()
print("== CDATA boundaries bracket the text between them ==")
print(run('<r>before<![CDATA[in <side> &not;]]>after</r>',
          ("StartCdataSectionHandler", "EndCdataSectionHandler",
           "CharacterDataHandler")))

print()
print("== namespace declarations, with and without a separator ==")
NSDOC = ('<r xmlns="http://d/" xmlns:p="http://p/">'
         '<p:c/><child/></r>')
print("no separator:")
for e in run(NSDOC, ("StartNamespaceDeclHandler", "EndNamespaceDeclHandler",
                     "StartElementHandler", "EndElementHandler")):
    print("  ", e)
print("separator ' ':")
for e in run(NSDOC, ("StartNamespaceDeclHandler", "EndNamespaceDeclHandler",
                     "StartElementHandler", "EndElementHandler"),
             namespace_separator=" "):
    print("  ", e)
print("an xmlns='' reset gives a None uri:")
print(run('<r xmlns="http://d/"><c xmlns=""/></r>',
          ("StartNamespaceDeclHandler",)))

print()
print("== namespace_prefixes adds the prefix as a third field ==")
for flag in (False, True):
    log = []
    p = pyexpat.ParserCreate(namespace_separator=" ")
    p.namespace_prefixes = flag
    p.StartElementHandler = lambda n, a: log.append(n)
    p.Parse('<p:r xmlns:p="http://p/"><p:c/></p:r>', True)
    print("namespace_prefixes=%-5s %s" % (flag, log))

print()
print("== a skipped entity ==")
# An undeclared entity in a document with an EXTERNAL subset that was not
# read is "skipped" rather than an error.
sk = pyexpat.ParserCreate()
sk.SkippedEntityHandler = lambda *a: print("  skipped:", a)
sk.DefaultHandlerExpand = lambda d: None
try:
    sk.Parse('<!DOCTYPE r SYSTEM "s.dtd"><r>&undeclared;</r>', True)
    print("  parsed")
except pyexpat.ExpatError as e:
    print("  ExpatError:", e)

print()
print("== the interning split: names repeat, values do not ==")
p = pyexpat.ParserCreate()
targets = []
datas = []
p.ProcessingInstructionHandler = lambda t, d: (targets.append(t),
                                               datas.append(d))
p.Parse('<r><?t one?><?t two?><?t one?></r>', True)
print("targets identical:", targets[0] is targets[1] is targets[2])
print("data not interned:", datas[0] is not datas[2], datas[0] == datas[2])
print("target is in intern:", "t" in p.intern)
print("data is not:", "one" not in p.intern)

print()
print("== a handler raising from a declaration aborts the parse ==")
for name in ("XmlDeclHandler", "StartDoctypeDeclHandler",
             "NotationDeclHandler", "CommentHandler",
             "ProcessingInstructionHandler", "StartCdataSectionHandler",
             "StartNamespaceDeclHandler", "AttlistDeclHandler"):
    p = pyexpat.ParserCreate()

    def thrower(*a):
        raise LookupError(name)

    setattr(p, name, thrower)
    try:
        p.Parse(DOC, True)
        print("%-30s NOT RAISED" % name)
    except LookupError as e:
        print("%-30s LookupError: %s" % (name, e))
    except pyexpat.ExpatError as e:
        # Some of these never fire on this document, in which case libexpat's
        # own complaint about the unread external subset is what comes out.
        print("%-30s did not fire" % name)

print()
print("== all thirteen can be set and cleared without parsing ==")
p = pyexpat.ParserCreate()
for name in WIRED:
    setattr(p, name, lambda *a: None)
print("set:", all(getattr(p, n) is not None for n in WIRED))
for name in WIRED:
    setattr(p, name, None)
print("cleared:", all(getattr(p, n) is None for n in WIRED))
print("and a parse with none of them still works:",
      pyexpat.ParserCreate().Parse("<r><a/></r>", True))
