# A package for tests/test_cli.py to run with -m.  Importing it must be
# visible, because `-m climod` runs climod/__main__.py and CPython imports
# the package first to find it.
IMPORTED = True
