# __doc__ is in every class's own dict, None when the body wrote no docstring.
#
# type_dict_set_doc puts it there during CPython's PyType_Ready, and
# type_from_parts did not -- so the name was simply absent from a class that had
# no docstring, and the MRO walk answered with a BASE's:
#
#   class W: "base doc"
#   class D(W): pass
#   D.__doc__   ->  'base doc' here, None in CPython
#
# which also means inspect.getdoc's own inheritance walk was looking at an
# answer it had not chosen.


def doc_of(C):
    return repr(C.__doc__), "__doc__" in C.__dict__, repr(C.__dict__.get("__doc__"))


class Documented:
    "a docstring"


class Plain:
    pass


class SubOfDocumented(Documented):
    pass


class SubOfPlain(Plain):
    pass


class SubDocumented(Documented):
    "its own"


class Explicit:
    __doc__ = "assigned, not a docstring"


class ExplicitNone:
    __doc__ = None


for name, C in (("documented", Documented), ("plain", Plain),
                ("sub of documented", SubOfDocumented),
                ("sub of plain", SubOfPlain),
                ("sub with its own", SubDocumented),
                ("__doc__ assigned", Explicit),
                ("__doc__ = None", ExplicitNone)):
    print("%-20s %s" % (name, doc_of(C)))

print("--- three-argument type() agrees ---")
for name, ns in (("bare", {}), ("with doc", {"__doc__": "d"}),
                 ("with None", {"__doc__": None})):
    print("%-10s %s" % (name, doc_of(type("T", (), ns))))

print("--- a subclass of a builtin ---")


class SubInt(int):
    pass


class SubList(list):
    "listy"


print("int  :", doc_of(SubInt))
print("list :", doc_of(SubList))
print("int's own doc is not inherited:", SubInt.__doc__ is None)

print("--- a metaclass-built class too ---")


class Meta(type):
    pass


class ViaMeta(metaclass=Meta):
    pass


class ViaMetaDoc(metaclass=Meta):
    "meta doc"


print("bare :", doc_of(ViaMeta))
print("doc  :", doc_of(ViaMetaDoc))

print("--- it is assignable, and the entry follows ---")
Plain.__doc__ = "set later"
print("after set:", doc_of(Plain))

print("--- and an instance reads the class's ---")


class Inst:
    "instance sees this"


print("instance:", repr(Inst().__doc__))

print("--- __slots__ does not change it ---")


class Slotted:
    __slots__ = ("a",)


class SlottedDoc:
    "slotted"
    __slots__ = ("a",)


print("slots    :", doc_of(Slotted))
print("slots doc:", doc_of(SlottedDoc))

print("done")
