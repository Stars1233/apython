"""_frozen_importlib_external - the name importlib._bootstrap_external answers to.

The other half of what `lib/_frozen_importlib.py` explains: CPython freezes
`importlib._bootstrap_external` under this name too, and `zipimport` imports
it at module scope before it does anything else.

Same arrangement, same reason, and the same thing an alias cannot skip.
`importlib/__init__.py` calls `_set_bootstrap_module` and sets
`_bootstrap._bootstrap_external` only on its except-ImportError branch, so
once this module exists that wiring has to happen here instead.  Both steps
are idempotent: either of the two alias modules may be imported first, and
`importlib` itself may have run before either.
"""

import sys

# Block this name for the duration of the re-entry, exactly as
# lib/_frozen_importlib.py does and for the same reason: importlib's __init__
# probes for it while this module is still half-built, and binding the
# half-built alias is how `_pack_uint32` came to be missing from it.
sys.modules[__name__] = None
try:
    from importlib import _bootstrap, _bootstrap_external
finally:
    del sys.modules[__name__]

# For the order where importlib ran first and took its else-branch instead.
if getattr(_bootstrap_external, '_bootstrap', None) is None:
    _bootstrap_external._set_bootstrap_module(_bootstrap)
if getattr(_bootstrap, '_bootstrap_external', None) is None:
    _bootstrap._bootstrap_external = _bootstrap_external

sys.modules[__name__] = _bootstrap_external
