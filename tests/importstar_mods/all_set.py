"""__all__ as a set: not a sequence, so CPython refuses to index it."""
__all__ = {"a"}
a = 1
