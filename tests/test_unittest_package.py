# `unittest`, CPython's own package -- and what had to come with it.
#
# What was here was four files and a fifth of the surface: no TestSuite, no
# loader, no TextTestRunner, no subTest, no IsolatedAsyncioTestCase, and a
# mock.py that was `patch.object` and nothing else.  The one that costs most
# is mock: MagicMock, AsyncMock, patch by string target, patch.dict,
# create_autospec, sentinel, ANY, call and the assert_* family are what a
# modern test suite is written on, and they are why test_asyncio and
# test_unittest could not run.
#
# Vendoring it pulled in a dependency closure, and each of those is a module
# a program written for CPython expects to have: traceback, linecache,
# inspect, dis, opcode, token, tokenize, textwrap, difflib, pprint,
# dataclasses, weakref, _weakrefset, fnmatch, argparse, gettext, locale,
# shutil, pkgutil, typing, importlib and threading.
#
# threading is the interesting one.  Threads are deferred in this tree, and
# CPython's threading.py runs unchanged on lib/_thread.py's single-threaded
# stand-in: everything one thread can observe works, and Thread.start()
# raises rather than pretending.  That is the shape a deferred feature should
# have, so it is checked here rather than assumed.
import unittest
from unittest.mock import (ANY, AsyncMock, MagicMock, Mock, PropertyMock,
                           call, create_autospec, mock_open, patch, sentinel)

# --- the package's own surface ----------------------------------------
print("__all__ complete:", [n for n in unittest.__all__
                            if not hasattr(unittest, n)])
print("the pieces:", unittest.TestCase.__name__, unittest.TestSuite.__name__,
      unittest.TestLoader.__name__, unittest.TextTestRunner.__name__,
      unittest.TestResult.__name__, unittest.IsolatedAsyncioTestCase.__name__)


class Sample(unittest.TestCase):
    def setUp(self):
        self.value = 1

    def test_passes(self):
        self.assertEqual(self.value, 1)

    def test_subtests(self):
        for i in range(3):
            with self.subTest(i=i):
                self.assertLess(i, 3)

    def test_assertions(self):
        self.assertIn(1, [1])
        self.assertNotIn(2, [1])
        self.assertIsInstance(1, int)
        self.assertAlmostEqual(0.1 + 0.2, 0.3)
        self.assertRegex("abc", "b")
        self.assertCountEqual([1, 2], [2, 1])
        with self.assertRaises(ValueError):
            raise ValueError
        with self.assertRaisesRegex(ValueError, "boo"):
            raise ValueError("boo")
        with self.assertWarns(UserWarning):
            import warnings
            warnings.warn("w")

    @unittest.skip("a reason")
    def test_skipped(self):
        self.fail("never runs")

    @unittest.expectedFailure
    def test_expected_failure(self):
        self.fail("expected")


suite = unittest.defaultTestLoader.loadTestsFromTestCase(Sample)
import io

result = unittest.TextTestRunner(stream=io.StringIO(), verbosity=0).run(suite)
print("ran:", result.testsRun, "ok:", result.wasSuccessful(),
      "skipped:", len(result.skipped),
      "expected failures:", len(result.expectedFailures))

# --- a failure is reported, with a message ----------------------------
class Failing(unittest.TestCase):
    def test_fails(self):
        self.assertEqual(1, 2)


res = unittest.TextTestRunner(stream=io.StringIO(), verbosity=0).run(
    unittest.defaultTestLoader.loadTestsFromTestCase(Failing))
print("a failure:", len(res.failures), "1 != 2" in res.failures[0][1])

# --- mock -------------------------------------------------------------
m = MagicMock()
m.method(1, key="v")
m.method.assert_called_once_with(1, key="v")
print("MagicMock:", m.method.call_count, m.method.call_args == call(1, key="v"))
print("ANY and sentinel:", ANY == object(), sentinel.TOKEN is sentinel.TOKEN)
print("magic methods:", len(m), bool(m), int(m), float(m), str(m)[:9])
m.reset_mock()
print("reset:", m.method.call_count)

mm = Mock(return_value=7)
print("return_value:", mm(), mm.call_count)
mm.side_effect = [1, 2]
print("side_effect:", mm(), mm())
mm.side_effect = ValueError("raised by side_effect")
try:
    mm()
    print("side_effect raise: NOT RAISED")
except ValueError as exc:
    print("side_effect raise:", exc)


class Real:
    attribute = 1

    def method(self, a, b=2):
        return a + b


spec = create_autospec(Real)
spec.method(1)
print("autospec call:", spec.method.call_args)
try:
    spec.method()
    print("autospec arity: NOT ENFORCED")
except TypeError:
    print("autospec arity: TypeError")

target = Real()
with patch.object(target, "method", return_value=99):
    print("patch.object:", target.method(1))
print("restored:", target.method(1))

import os

with patch("os.sep", "!"):
    print("patch by name:", os.sep)
print("restored:", os.sep)

d = {"a": 1}
with patch.dict(d, {"b": 2}, clear=False):
    print("patch.dict:", sorted(d))
print("restored:", sorted(d))

with patch("os.getpid") as pid:
    pid.return_value = 4
    print("patched a function:", os.getpid())

handle = mock_open(read_data="line1\nline2\n")
with patch("builtins.open", handle):
    with open("anything") as f:
        print("mock_open:", f.read())

prop = PropertyMock(return_value=5)
Holder = type("Holder", (), {})
Holder.prop = prop
print("PropertyMock:", Holder().prop)

# --- the async half ---------------------------------------------------
import asyncio

am = AsyncMock(return_value=3)
print("AsyncMock:", asyncio.run(am()), am.await_count)
print("asyncio.iscoroutinefunction:", asyncio.iscoroutinefunction(am))


# IsolatedAsyncioTestCase imports, and does not RUN here.  It drives its
# coroutines through asyncio.Runner -- one loop kept open across asyncSetUp,
# the test and asyncTearDown, reachable as runner.get_loop() -- and this
# tree's loop is assembly that owns its own lifecycle: _asynciocore.run()
# creates a loop, runs one coroutine on it and tears it down, with no way to
# hold one open.  A Runner whose run() started a fresh loop each time would
# pass a test that shares nothing across those three and fail confusingly for
# any that does, so there is none; DIVERGENCES.md records it.
print("IsolatedAsyncioTestCase imports:",
      unittest.IsolatedAsyncioTestCase.__name__,
      issubclass(unittest.IsolatedAsyncioTestCase, unittest.TestCase))


async def main():
    await asyncio.sleep(0)
    return 17


print("the loop this tree does have:", asyncio.run(main()))

# --- threading, on one thread -----------------------------------------
import threading

lock = threading.RLock()
with lock:
    pass
event = threading.Event()
event.set()
cond = threading.Condition()
with cond:
    pass
sem = threading.Semaphore(2)
sem.acquire()
sem.release()
print("locks and primitives:", event.is_set(), threading.current_thread().name,
      threading.main_thread() is threading.current_thread())
local = threading.local()
local.x = 1
print("thread-local:", local.x)
# Starting a thread either works or refuses -- what it must never do is
# silently run nothing, which is the failure mode a stand-in invites.  On
# CPython it runs; here _thread.start_new_thread raises.  The property is the
# same on both, so that is what is compared.
ran = []
try:
    started = threading.Thread(target=lambda: ran.append(1))
    started.start()
    started.join()
    honest = ran == [1]
except RuntimeError:
    honest = ran == []
print("Thread.start runs or refuses, never pretends:", honest)

# --- the rest of the closure imports and works ------------------------
import argparse
import dataclasses
import difflib
import fnmatch
import inspect
import linecache
import pprint
import shutil
import textwrap
import traceback
import typing
import weakref


@dataclasses.dataclass
class Point:
    x: int
    y: int = 0


print("dataclasses:", Point(1), dataclasses.astuple(Point(1, 2)))
print("inspect:", inspect.signature(lambda a, b=1: None),
      inspect.isfunction(print))
print("textwrap:", textwrap.fill("a b c d", width=3).split("\n"))
print("difflib:", difflib.get_close_matches("appel", ["apple", "ape"]))
print("fnmatch:", fnmatch.fnmatch("a.py", "*.py"))
print("pprint:", pprint.pformat({"b": 1, "a": 2}))
print("typing:", typing.Optional[int], typing.List[int])
print("traceback:", traceback.format_exception_only(ValueError, ValueError("v")))


class Held:
    pass


held = Held()
ref = weakref.ref(held)
print("weakref:", ref() is held)
parser = argparse.ArgumentParser(prog="p", add_help=False)
parser.add_argument("--n", type=int, default=3)
print("argparse:", parser.parse_args(["--n", "5"]).n, parser.parse_args([]).n)
print("shutil:", callable(shutil.copyfile), shutil.which("sh") is not None)
print("survived")
