# A package whose __all__ names a submodule its own body never imports.
__all__ = ["leaf", "here"]

here = "in the package"
