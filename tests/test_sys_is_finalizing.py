# sys.is_finalizing(), which asyncio asks before it touches its selector.
#
# unix_events.py's loop.close() opens with `if not sys.is_finalizing()`, and
# threading, logging and concurrent.futures all branch on it before releasing
# anything they might no longer own.  There was no flag at all: main's teardown
# ran the shutdown collection -- which is where every __del__ in a cycle runs
# -- with nothing recording that the interpreter was coming apart.
import sys

print(sys.is_finalizing())
print(type(sys.is_finalizing()).__name__)
print(sys.is_finalizing() is False)

for bad in ((1,), (1, 2)):
    try:
        sys.is_finalizing(*bad)
    except TypeError as e:
        print("TypeError:", type(e).__name__)

# It is answered the same way twice; nothing latches it early.
print(sys.is_finalizing(), sys.is_finalizing())
