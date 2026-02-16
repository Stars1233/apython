class _PatchObject:
    def __init__(self, target, attribute, new):
        self._target = target
        self._attribute = attribute
        self._new = new
        self._old = None

    def __enter__(self):
        self._old = getattr(self._target, self._attribute)
        setattr(self._target, self._attribute, self._new)
        return self._new

    def __exit__(self, *args):
        setattr(self._target, self._attribute, self._old)
        return False

    def __call__(self, func):
        # Used as decorator
        def wrapper(*args, **kwargs):
            with self:
                return func(*args, self._new, **kwargs)
        wrapper.__name__ = func.__name__
        wrapper.__unittest_skip__ = getattr(func, '__unittest_skip__', False)
        wrapper.__unittest_skip_why__ = getattr(func, '__unittest_skip_why__', '')
        return wrapper

class _Patch:
    def object(self, target, attribute, new=None):
        return _PatchObject(target, attribute, new)

patch = _Patch()
