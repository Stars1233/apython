# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

~~- **`repr()` of a float was not always the shortest form.**  The search
  tried `"%.*g"` at rising precision and took the first that read back, which
  finds the shortest of the forms *glibc* produces -- and at an exact
  half-way case glibc rounds to even, where it is the other neighbour that
  round-trips.  `repr(2.0**-24)` came out with seventeen digits where CPython
  prints sixteen.  About one ordinary value in a hundred.~~  Each precision
  is tried twice now, as rendered and with the last digit carried up by one,
  which is the same pair CPython's own dtoa searches.
  `tests/test_float_repr_ties.py`.

~~- **`asyncio.gather` did not gather.**  It built one task per coroutine and
  handed back the LIST of them, with a TODO in the source saying the awaiting
  was still to do -- so `await asyncio.gather(...)` awaited a list, which is
  not awaitable, and answered None.  And an exception that reached the root
  task was dropped by `eventloop_run`, which read only `.result`:
  `asyncio.run(main())` where main raises answered None and the exception was
  never seen again.~~  `tests/test_async_gather.py`.

  Two more came out of it.  `ready_enqueue` zeroed `.next` unconditionally,
  so enqueuing a task that was ALREADY queued cut the ready list off after
  it and the loop then sat waiting for something that could not arrive.  And
  a `list_new` called with a stale register for its capacity argument turns
  into a zeroing loop over about 700 million words -- an "intermittent hang"
  whose length depended on ASLR.

~~- **`__context__` is not set across a generator resume.**~~  `gen_send`
  CLEARED `current_exception` before resuming the frame, so an exception
  raised inside the generator had nothing to chain to: `next(it)` from inside
  an except block produced one with `__context__` of None.  `await` goes
  through `gen_send` too, so every awaited exception lost its context the
  same way.  `tests/test_exc_context.py`.

  What is still open is the third shape: `await SOME_TASK` where the task
  raised.  `current_exception` is a single global and task switching does not
  follow it, so the exception the awaiting coroutine was handling is not
  what is current when the task's exception is re-raised.  The fix is a
  per-task exception state; it was tried, and it needs `task_step` to tell a
  raise from a `return` out of an except block, which it currently cannot.

~~- **Six pieces of the ordinary protocol were missing outright.**  `range`
  had no `__eq__`, no `__hash__`, no `index`/`count` and no `start`/`stop`/
  `step` -- and no `tp_dict` at all, so none of `__len__`, `__iter__` or
  `__getitem__` could be asked for by name.  `iter(callable, sentinel)`, the
  two-argument form, did not exist.  Neither did `slice.indices(length)`,
  `complex.__bool__` by name, or `AttributeError(name=, obj=)` /
  `ImportError(name=, path=)` -- whose keywords were silently folded into
  `.args`.~~  `tests/test_range_protocol.py`, `tests/test_iter_sentinel.py`,
  `tests/test_slice_indices.py`, `tests/test_exception_kwargs.py`.

  Two of them were wrong in a way the entries did not say.  `range(3) ==
  range(3)` was **False**, not merely unimplemented, because with no
  `tp_richcompare` the comparison fell back to identity -- so
  `{range(3), range(0, 3, 1)}` held two elements where CPython holds one.
  And an exception's keyword arguments did not just go unread: they landed in
  `.args`, so `AttributeError("x", name="n").args` was a 2-tuple.

- **`super` is not an object.**  The zero- and two-argument forms both work
  written as `super(...).attr`, because the compiler emits `LOAD_SUPER_ATTR`
  for exactly that shape and the opcode does the MRO walk itself.  What does
  not exist is a super *object*: `s = super()` and `s = super(B, self)` both
  raise "object is not callable", because `super_type` is a placeholder with
  no `tp_call`, no `tp_new` and no fields -- a stand-in that
  `LOAD_SUPER_ATTR` pops and discards.  Anything that stores one, passes one,
  or reaches it through `getattr` fails.  `super(B, B).m` also answers a
  bound method where CPython answers the plain function.

  The fix is a real three-field type and a `tp_getattr` doing the walk that
  `op_load_super_attr` already does inline, plus the frame introspection the
  zero-argument form needs: CPython reads the `__class__` cell and the first
  positional argument out of the calling frame.

- **Three types CPython refuses to let you subclass are subclassable here.**
  `bool`, `memoryview` and `range` are all final in CPython -- they lack
  `Py_TPFLAGS_BASETYPE` -- and `class B(bool): pass` is
  "type 'bool' is not an acceptable base type".  Here all three are accepted,
  and the instances that come out are not obviously wrong, merely not
  something CPython would have let you make.  A `TYPE_FLAG_BASETYPE` test in
  `type_from_parts` is the whole of it; the work is auditing which static
  types should carry the flag.

~~- **Three holes that each blocked a stdlib module.**  `str(bytes,
  encoding[, errors])` did not exist -- `str(b, "utf-8")` answered "str()
  takes at most 1 argument" -- which is what kept `glob` and `fnmatch` out,
  through `re/_parser.py`.  `str.maketrans({...})`, the one-argument dict
  form, did not exist either, and `pathlib` builds its table that way.  And
  `%`-formatting did not understand `*`: `"%.*g" % (3, x)` reported "not all
  arguments converted during string formatting", which is what `timeit`
  runs into.~~  `glob`, `json` and `pathlib` import now, and the floor in
  `tests/stdlib_floor.txt` records it.  `tests/test_str_decode.py`,
  `tests/test_maketrans.py`, `tests/test_percent_star.py`.

  Two of the three turned out to be wider than the entry said.  The `*`
  bug was not only precision: `%*d` took no width either, and `%*.*f` took
  neither.  And `maketrans`'s two- and three-argument forms never checked
  that they had been handed strings, so `str.maketrans("ab", 1)` read the
  int as a `PyStrObject`.

~~- **set's method forms took exactly one argument.**  `union`,
  `intersection` and `difference` are variadic in CPython -- `s.difference(a,
  b)` is `(s - a) - b`, and the no-argument form is a copy -- and all three
  raised `union() takes exactly one argument` for anything else, so
  `set().union(*parts)`, the ordinary way to flatten a list of sets, did not
  work.  `update` was worse: it took the first source and silently ignored
  the rest.  And `intersection_update`, `difference_update` and
  `symmetric_difference_update` did not exist at all, so the only way to
  narrow a set in place was `s &= t`, which until the commit before this one
  did not narrow it in place either.~~  Found by an audit, not by a test.
  The bodies stay two-operand and a fold walks the arguments;
  `tests/test_set_variadic.py`.

- **`str.encode` and `bytes.decode` know only utf-8, ascii and latin-1.**
  Any other name is a LookupError, where CPython would find the codec through
  the registry; reaching it from the interpreter would mean calling Python
  from a builtin method.

- **The `_abc` registry and caches hold strong references.**  CPython uses
  weak ones, so a class registered against an ABC can be collected and the
  ABC's caches shrink; here a registered class lives as long as the ABC.
  Registries are process-lifetime and small in practice.  Revisit if
  `_weakref` lands.

- **`posix` is a subset, and a deliberate one.**  The file, directory and
  process calls `os.py` and `os.path` reach for are there, along with
  `environ`, `stat_result`, `error` and the O_*/W* constants -- enough that
  CPython's own `os.py` imports and works.  What is not: `scandir` and
  `DirEntry`, ~~`chdir`, `chown`, `utime`, `truncate`,
  `dup2`,~~ `fork`, `execv`, ~~`link`,~~ and the whole `*at` family.
  ~~`chmod` takes no file descriptor, where CPython's does.~~  The struck
  eight, plus `fsync` and `fchmod`, are there now
  (`tests/test_posix_more.py`); the entry named `ftruncate` among them, which
  was already present -- it is `truncate`, the path-taking form, that was
  missing.  Their arity messages are CPython's argument-clinic wording, and
  the same helper corrected `unlink` and `rmdir`, which had the old one.
  `_have_functions` is an empty list, which is the honest answer -- no `dir_fd=` support -- and
  os.py reads it to build `supports_dir_fd`.

- **Missing C modules**, in rough order of how many stdlib modules each
  blocks: ~~`_struct`,~~ `_socket`, ~~`_random`, `_contextvars`, `_tokenize`,~~
  `_ast`, `_imp`, ~~`binascii`, `_string`,~~ then a long tail of one apiece.
  (`_io` is not among them: `src/iomod.asm` supplies
  `_iocore` and `lib/_io.py` assembles both halves under the name `_io`.
  Neither are `math` and `_collections`, which are there now.)
  `make check-stdlib` gives the current figure -- 107 of 196, up from 78.

  The struck ones are in `lib/` now, with `_operator` (`_compare_digest`,
  which `hmac` imports directly and which has no fallback) and `atexit`.
  `_tokenize` is the notable one: it is a real tokenizer, and its token
  stream is identical to CPython's over all 163 of CPython's own `Lib/*.py`
  that CPython itself can tokenize.  Its one deliberate difference is that an
  f-string comes out as a single STRING token, as it did through 3.11, rather
  than the FSTRING_START/MIDDLE/END triple 3.12 splits it into.


  ~~`math` itself is short of `dist`, `prod`, `isclose`, `perm`, `ulp` and
  `nan`/`inf` parsing corners;~~ all five are there now
  (`tests/test_math_more.py`); `nan`/`inf` parsing already matched.  `dist`
  hands its coordinate differences to the same routine `hypot` uses, so the
  two always agree with each other -- and it inherits the rounding note
  below.  And `gamma`, `lgamma`, the n-ary `hypot` and
  `sumprod` round differently from CPython's, which uses its own Lanczos
  approximation and double-double arithmetic where these use glibc and a
  Neumaier sum.  `fsum` is exact: it is Shewchuk's algorithm, as CPython's
  is.  `tests/test_math.py` says which is which.

- **Weak references keep no per-object slot.**  The links live in a side
  table keyed by the referent's address rather than in the object, so
  `tp_weaklistoffset` does not exist and `__weakref__` is not an attribute.
  Everything observable through `_weakref` works; a C extension expecting the
  slot would not.  ~~And because there is no offset, there was nothing to be
  zero: every type was weak-referenceable, where CPython refuses `ref([])`,
  `ref(1)`, `ref(None)` and most other builtins.  The refusal is
  load-bearing -- `WeakValueDictionary` relies on it to reject a value whose
  death it could never observe -- so accepting them turned a TypeError at the
  call into a dictionary that quietly never dropped anything.~~
  `weakref_referenceable` now answers the same question from the type, by
  CPython's rule: a class is given the word unless it declares `__slots__`
  without naming `__weakref__`, or its layout base keeps its value inline and
  variable-sized.  `tests/test_weakref_types.py`.

~~- **A few names are still short of CPython's `dir()`.**  `int` and `float`
  are missing the in-place forms; `set` is missing `__iand__` and `__ior__`,
  deliberately -- it has no `nb_inplace_*` slots, so `s &= t` degrades to the
  binary form, and a by-name `__iand__` that did not mutate in place would be
  a wrong answer rather than a missing name.~~  Both halves were wrong.
  CPython's `int` and `float` have no in-place forms either, so there was
  nothing missing there.  And `set` was not short a *name*: it was short the
  behaviour.  `s &= t` computed the right contents into a new set and rebound
  the name, so every other reference to the same set went on seeing the old
  value -- an ordinary aliasing bug, filed here as a design choice.  set now
  has real `nb_iand`/`nb_ior`/`nb_isub`/`nb_ixor` that mutate, and the four
  dunders to match.  frozenset keeps none of them, as CPython's does not.
  `tests/test_set_inplace.py`.

- **`collections.deque` is list-backed, and two itertools functions
  materialise.**  CPython's deque is a block-linked list, so `appendleft` and
  `popleft` are O(1) there and O(n) here; `itertools.groupby` materialises
  each group rather than sharing the source iterator, and `tee` materialises
  the source.  Every observable answer matches for a finite iterable.

- **bytearray's read-only methods copy.**  bytes keeps its data inline and
  bytearray keeps it out of line, so the shared method bodies cannot read a
  bytearray directly; each wrapper builds a temporary bytes, runs the bytes
  body and releases it.  Correct, and cheap for a scratch buffer, but it is
  an allocation per call -- worth threading a (pointer, length) pair through
  the bodies if bytearray ever becomes hot.

- **The regex engine matches CPython on all 831 checked answers.**
  `make check-re` runs `tests/re_differential.py` under both interpreters and
  ratchets against `tests/re_floor.txt`; it needs `$CPYTHON_LIB`, because `re`
  is a Python module and so comes from a real stdlib.  Three things outside
  that pattern set are still open:

  - A malformed replacement template raises `IndexError` or `ValueError`
    where CPython raises `re.error`.  `re.error` is defined in Python, so
    constructing one from the engine would mean importing `re` from `_sre`.
  - A nested unbounded repeat (`(a*)*b`) recurses until the limit.
  - `bytes` patterns are unsupported: `sre_state_init` hardcodes
    `is_bytes = 0`, and nothing reads the field.  A bytes SUBJECT is refused
    with CPython's own message now; a bytes PATTERN is the missing half.

- **C code here cannot catch a Python exception.**  `raise_exception`
  tail-jumps into `eval_exception_unwind`, which resumes the eval loop from
  saved globals rather than returning through the C stack, so a `call` to a
  slot that raises never comes back.  `str.translate` gets around it by
  reaching a heaptype table through `dunder_call_2`, which does return; the
  general limit stands, and is why the `bytes %` leak below cannot be fixed
  by catching.

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler, so a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.
  The entry above is the other half of why.

- **A classmethod on a builtin type reprs as its bare name.**  Ordinary
  methods, slot wrappers and getsets all name themselves and their owner now;
  `int.from_bytes`, `float.fromhex` and `str.maketrans` are wrapped in a
  classmethod object, which `type_stamp_methods` skips.  CPython's repr for
  those carries an address -- `<built-in method from_bytes of type object at
  0xa3cf20>` -- which this tree does not print for anything, so there is
  nothing to match exactly.

- **`complex()` of a string does not accept Unicode spaces or Unicode digits.**
  CPython runs `_PyUnicode_TransformDecimalAndSpaceToASCII` first, so
  `complex("\u30001+2j")` parses there; here any byte past ASCII is a
  malformed string.  ASCII whitespace, brackets, underscores, `inf` and `nan`
  all behave as CPython's do.

- **`_thread` is a single-threaded stand-in.**  `lib/_thread.py` gives
  `get_ident` a constant, makes locks uncontended, and raises from
  `start_new_thread`.  Everything in the stdlib that only takes a lock works;
  anything that expects a second thread does not.

- **A `__slots__` instance still carries a dict WORD it can never use.**  The
  suppression works now, but `tp_dictoffset` is left pointing at a word that
  is always NULL: eight bytes per instance, and the reason every reader has to
  ask about `TYPE_FLAG_HAS_SLOTS` as well as about `tp_dictoffset`.  Zeroing
  the offset means moving the slots down by one word and teaching the dealloc
  and traverse walks where the header now ends.

- **A heaptype's layout base is the widest of its bases, not CPython's solid
  base.**  `class C(A, B)` where A and B are unrelated builtin subclasses of
  different layouts is accepted and laid out as the wider one, where CPython
  raises "multiple bases have instance lay-out conflict".

- **Cyclic garbage is not finalized at shutdown.**  CPython runs `__del__` on
  the cycles still alive when the interpreter exits; apython does not, so
  those finalizers never run.  `tests/test_del_and_gc_state.py` records this
  divergence in its recorded transcript deliberately -- see the note there.

- **The default `repr` names the type but not the address.**  CPython answers
  `<set_iterator object at 0x7f...>`; a type with no `tp_repr` answers
  `<set_iterator>` here, and a plain class instance answers `<instance>`
  rather than `<__main__.C object at 0x...>`.  Nothing in the tree formats a
  pointer, and deliberately: an address cannot match CPython's, and every
  test is a diff against it.  What was wrong and is fixed is the NULL --
  `obj_repr` used to answer a NULL Value with no exception set, so `print()`
  silently skipped the argument and `repr(iter({1}))` handed its own caller a
  missing argument.

- **Traceback carets are two jobs, not one.**  The `.pyc` path already has
  the column fields and merely steps over them (`code_addr2line`), so the
  renderer half is decode-only.  But apython's own compiler emits **no**
  columns at all: `Instr` has no column field, the AST carries `col` but no
  `end_col`, and `asm_linetable` writes only the line-only forms 13 and 15.
  Doing the `.pyc` half alone would make `check-source` differ from CPython
  on any test that lets an exception escape, so the two halves have to land
  together.

- **Traceback rendering has no caret line.**  CPython underlines the failing
  expression (`^^^^^^^^`, and `~~^~~` for binary operators and subscripts)
  using the column fields of the location table and, for the anchor forms, a
  tokenizer over the source segment.  Everything else in the report --
  frames, line numbers, source lines, the repeated-frame elision, the
  `__cause__` / `__context__` preamble -- matches; only the caret line is
  missing.

## Missing pieces

These are absences rather than wrong answers — the interpreter raises rather
than lying — but they are ordinary Python that does not work:

- **The unwinder's one-reference invariant is real; the drop it caused is
  not reproducible.**  `raise_exception_obj` takes over its caller's
  reference rather than adding one, so `current_exception` usually holds
  exactly one -- and `eval_exception_unwind` runs arbitrary finalizers while
  that is true, releasing the value stack before it is done with the global.
  This file used to record a concrete drop: `current_exception` pointing at
  an object whose `ob_refcnt` was already 0, reproducible with an `async for`
  over a hand-written `__anext__` that raises.  It no longer happens.  That
  repro is clean, valgrind is clean over the async suite, and a gdb watch for
  `current_exception != 0 && ob_refcnt <= 0` at `instance_dealloc`'s entry
  gets no hits across the corpus.  `gen_dealloc` held the pending exception
  borrowed in a register across `gen_dealloc_close`, which is the shape that
  could produce it, and takes a reference now.  `instance_dealloc`'s refcount
  check stays: it is two instructions, and the invariant it guards is one a
  future caller can break again.

- **`__context__` is not chained across an `await` into another coroutine.**
  `gen_send` saves and clears `current_exception` around the awaited frame,
  so a raise inside it has nothing to chain to -- an exception raised while
  the awaiting frame is handling another gets `__context__` of None where
  CPython gives the outer one.  Chaining WITHIN a frame is right, coroutines
  included.  It is not specific to tasks: a plain `await coro()` loses it
  too.

- **`asyncio.gather` returns the tasks rather than their results.**  It
  creates and enqueues one task per argument and hands back the list, so
  `await asyncio.gather(a(), b())` is None instead of `[1, 2]`.  A real
  gather needs an awaitable that collects, the shape
  `wait_for_awaitable_type` has.

- **`bytes % args` leaks its temporary when the format is malformed.**  The
  work is done by handing a decoded copy of the format and the arguments to
  `str_mod`, and `str_mod` RAISES for a wrong argument count -- a raise
  abandons the C stack, so neither the copy nor the converted arguments are
  released.  Putting them somewhere the unwinder frees would be worse: an
  argument's `__str__` can run Python, and a raise caught inside it would
  free a buffer `str_mod` is still reading.

- **`bytes.decode`'s ascii arm raises a fixed message.**  `str()` of a
  Unicode error renders its five fields now, so an exception raised from
  `lib/_codecs.py` reads as CPython's does.  The asm sites still pre-render a
  one-argument exception instead.  The utf-8 *decode* arm and both *encode*
  arms build CPython's wording by hand, character, position, range and all;
  ~~the ascii decode arm still says only "byte not in range for this
  encoding"~~ -- it names the codec, the byte and the position now, and every
  decode error carries CPython's five fields (`encoding`, `object`, `start`,
  `end`, `reason`), which are how an error handler knows what to replace.
  `tests/test_decode_errors.py`.
  Raising a real five-argument exception from asm needs a way to call an
  exception type with five arguments, which `exc_new` does not offer.

- **A memoryview with a step other than 1 is not a view.**  `mv[::2]` and
  `mv[::-1]` raise NotImplementedError.  CPython answers with a
  non-contiguous view, which needs a stride the object does not carry -- and
  a stride would have to be honoured by every reader: `tobytes`, iteration,
  `bytes()`, comparison, `hex`, `tolist`, and the write path.  Nothing in
  `_pyio` asks for one, so the field is not there yet.

- **A cleanup that raises is reported in one line.**  A `finally` in a dropped
  generator, and a `__del__`, both report an exception they cannot propagate;
  CPython prints the object's repr and a full traceback where these print
  "Exception ignored in __del__" and "Exception ignored in: generator
  cleanup".  The behaviour either side of the message is the same.

- The `re` wrapper module.  The `_sre` engine underneath is complete, but
  without a shipped `re.py` an `import re` finds CPython's, which needs
  `enum` and `types`.

## Robustness

- **`re.fullmatch(r'([a-z]*)+', 'abc1')` exhausts the regex recursion limit**
  where CPython answers None.  Seven of the eight patterns in
  `tests/re_differential.py`'s fullmatch block agree; this is the eighth.
  The zero-width guard now saves and restores `last_pos` the way CPython's
  `save_last_ptr` does, which fixed the sibling `(a*)*` against `'aab'`, so
  what is left is a second bound this engine does not have -- CPython's
  MAX_UNTIL also restores `state->ptr` and the mark stack on the failure
  path, and reaches the tail iteratively rather than one C frame per
  iteration.

~~- **`set.__contains__(frozenset(...), x)` is accepted** where CPython
  raises.  ... this tree does not draw that distinction yet.~~

~~- **A descriptor's arity is checked before its receiver.**  CPython asks
  which object it was handed first ...~~

  Both are fixed, and the entries badly understated the first one.  Nothing
  checked an unbound descriptor's receiver at all: `list.append((1, 2), 9)`
  read a tuple's header as a list's and tried to grow it -- "Fatal: out of
  memory", from a two-element tuple.  `builtin_func_call` is the single
  funnel every builtin method goes through and `func_owner` was already
  recorded there for the repr, so one check covers all of them, before the
  arity as CPython checks it.  `func_kind` picks between the two wordings.
  `tests/test_descriptor_receiver.py`.

  The arity messages went with it: "function takes at most N arguments" --
  with a literal N -- and "expected exactly one argument" are now CPython's
  counted forms.  What is left is per-method: a builtin registered with no
  argument counts still accepts extras silently (`str.upper("a", 1)`), and
  CPython's own wordings there are inconsistent between clinic-generated and
  hand-written methods.

- **asyncio `Task`s and `wait_for` wrappers are not GC-tracked.**  A `Task`
  holds its coroutine, which holds a frame, whose locals can hold the task --
  an ordinary cycle that never collects.  Code objects were in the same state
  and are tracked now.

  The mechanical part is small -- `gc_alloc` + `gc_track` in `task_new`,
  `gc_dealloc` in `task_dealloc`, `TYPE_FLAG_HAVE_GC` and a traverse/clear
  pair.  It was tried, and it is not enough: the tree holds a live `Task`
  through **four** raw pointers, and the moment tasks become collectable each
  of them can be left dangling.  The ready queue (`ready_enqueue`) and
  `EventLoop.root_task` are dealt with -- the queue owns what it holds now,
  and the root is released when the loop finishes -- and two are not:

  - `TimerEntry.task` in the poll backend's min-heap
    (`src/pyo/eventloop_poll.asm`), and the io_uring SQE's `user_data`
    (`src/pyo/eventloop_iouring.asm`);
  - `AsyncTask.waiters[]`, appended raw by `task_add_waiter` and dropped en
    masse when the task completes.

  Each needs to take a reference, or the collector needs to treat it as a
  root.  Without that, a `Task` collected while it is sleeping or being
  awaited corrupts the heap: `asyncio.run()` after a collected task cycle
  segfaults inside an unrelated allocation.

- **`gc` has no `get_objects` and no debug flags.**  The module answers about
  the collector -- `collect`, `enable`/`disable`/`isenabled`, the counts, the
  thresholds, `garbage` and `callbacks` -- but this collector keeps no list of
  tracked objects it could hand back, and has no debug output to switch on.
  `gc.collect()` also counts a two-object cycle as one where CPython counts
  two: clearing the first drops the second by refcount before the sweep
  reaches it.


- **`s += x` in a loop is O(n^2)**: `str_concat` always allocates, and
  `src/opcodes/arith.asm` routes `NB_INPLACE_ADD` to the same `sq_concat`, so
  each step copies the whole accumulated string.  CPython's ceval resizes in
  place when the left operand's refcount is 1.  Measured, though, the two are
  level: repeated appends cost the same here as under CPython 3.12, because
  that optimization does not fire for the ordinary module-level accumulator
  either.  Doing it would make apython faster than
  CPython on this shape rather than close a gap, and it needs the eval loop to
  give up its stack reference before the concat, so it is recorded rather than
  done.

- **Crafted `.pyc` and `_sre` bytecode are trusted.**  Marshal validates
  offsets but not types, so a `co_consts` slot holding an int is
  dereferenced by `eval_frame`; `frame_new` adds two `.pyc` fields in 32
  bits; `sre_match` bounds the opcode but not its operands.  This is an
  attacker model rather than breakage, and is a separate track.

## Test infrastructure

Two tests compare against a recorded transcript in `tests/expected/` rather
than against CPython, because CPython cannot serve as an oracle for them:

- `test_sre.py` feeds hand-written SRE bytecode to `_sre.compile()`, a
  private API that does not validate its input; CPython segfaults on the
  group pattern it uses.
- `test_del_and_gc_state.py` compares stderr wording that legitimately
  differs, and its transcript also freezes the shutdown-finalization gap
  above.  The asserts, not the transcript, are what establish correctness.

Both are self-asserting.  Any *new* recorded-oracle test needs the same
justification, or it risks blessing a divergence instead of catching it.

## Style debt

Everything here assembles and runs.  These are places the tree does not follow
STYLE.md, listed so the gap is a known quantity rather than a surprise to
whoever copies a neighbouring file.  Counts are deliberately absent -- grep
gives a current one, and a number written here is wrong by the next commit.

- **Frames whose `rsp` is not 16-byte aligned at a `call`.**  Every
  `XX_FRAME equ` now carries the arithmetic in a trailing comment, and writing
  those out is what made the scale visible: a good many say
  `not 16-aligned`, and `src/compiler/lint.py`'s `check_alignment` flags several
  times that number again in functions whose frame size is a literal rather
  than a named constant.  Mostly harmless -- the SysV requirement bites at a
  `call` into libc, and most of these never reach one -- but `builtin_int_fn`,
  `builtin_round_fn` and `builtin_pow_fn` all reach GMP, whose float paths use
  aligned SSE.  This is the debt that keeps `check_alignment` scoped to
  `src/compiler/` plus `src/main.asm` instead of running tree-wide.

- **`src/pyo/bytes.asm` is 178k, against CLAUDE.md's 100k limit for a
  hand-written file.**  It holds both bytes and bytearray, and the natural
  seam is exactly that -- bytearray's own methods and its resizable storage
  are the half that grew.  `src/methods/init.asm` is the next one up at 90k,
  and its 320 registration sites are four open-coded instructions apiece;
  one row each would take it to about 45k.

- **Functions with no separator or docblock at all**, and, among those that
  have one, docblocks with no `->` signature line.  The signature is the only
  part of a function's contract that nothing checks, so its absence is a real
  gap rather than a cosmetic one.  This is the one item here a script cannot
  finish: writing a signature means reading what the function actually returns.
