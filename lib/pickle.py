# pickle - minimal stub for apython
# Serialization is not yet implemented. This stub provides
# constants so tests can import pickle and check HIGHEST_PROTOCOL.

HIGHEST_PROTOCOL = 5
DEFAULT_PROTOCOL = 5


class PicklingError(Exception):
    pass


class UnpicklingError(Exception):
    pass


def dumps(obj, protocol=None):
    raise NotImplementedError("not implemented in apython")


def loads(data):
    raise NotImplementedError("not implemented in apython")


def dump(obj, file, protocol=None):
    raise NotImplementedError("not implemented in apython")


def load(file):
    raise NotImplementedError("not implemented in apython")
