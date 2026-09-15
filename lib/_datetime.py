"""_datetime - AN ALIAS FOR THE PURE-PYTHON IMPLEMENTATION, NOT A C ACCELERATOR.

CPython ships `_datetime` as a C extension and the module above it --
datetime -- prefers it over the Python code in the same file.  There is
no C accelerator here; the Python half is the whole implementation, and it
works.

This module exists for one reason: `test.support.import_fresh_module`
returns **None** when a module named in its `fresh` list cannot be imported,
and a test file that then does `module.__dict__` dies on
"'NoneType' object has no attribute '__dict__'" while its body is still
executing.  That takes every test in the file with it -- test_datetime is
3,513 of them -- and the code under test is the pure implementation, which is
present and correct.

So importing this succeeds and gives back the same objects the pure module
defines.  Nothing here is faster than what it re-exports, and no number
measured through it should be read as native speed.  DIVERGENCES.md records
the arrangement.
"""

# The names ARE re-exported here, and they have to be.  Lib/datetime.py is a
# dispatcher:
#
#     try:
#         from _datetime import *
#         from _datetime import __doc__
#     except ImportError:
#         from _pydatetime import *
#         from _pydatetime import __doc__
#
# and `from _datetime import __doc__` SUCCEEDS for any module at all -- every
# module has one.  So an empty _datetime is not a no-op: it wins the
# try-branch and leaves `datetime` with no classes in it.  That is the
# half-implemented-is-worse shape exactly, and worse than not being here.
#
# _pydatetime is the pure implementation, in its own module, and it does not
# import this one -- so re-exporting from it is neither circular nor a second
# copy.  If it is not there, this module refuses to import, which is the
# signal datetime.py is written to read.
from _pydatetime import *                                       # noqa: F401,F403
from _pydatetime import __doc__                                 # noqa: F401
from _pydatetime import (date, datetime, time, timedelta,        # noqa: F401
                         timezone, tzinfo, MINYEAR, MAXYEAR, UTC)
