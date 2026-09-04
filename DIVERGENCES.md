# Deliberate divergences

Places apython answers differently from CPython 3.12 **on purpose**.  Each was
weighed and each was chosen; none is a defect waiting for a patch, and none
belongs in `bugs.md`, which is the list of what is still wrong.

Every entry says what the difference is, why the choice was made, and what
changing it would cost -- so that a later decision to revisit one starts from
the reasoning rather than from scratch.

## Object layout and introspection

- **No managed dicts, so the layout attributes differ for any class that has
  an instance `__dict__`.**  CPython 3.12 keeps a class's instance dict in a
  slot before the object rather than in the object, and reports that as
  `__dictoffset__ == -1` and `__weakrefoffset__ == -32`; the header itself is
  then 16 bytes.  Here the dict is a word inside the instance, so
  `class A: pass` reports a positive `__dictoffset__` and a header one word
  wider.  A `__slots__` class has no dict of its own and matches CPython
  exactly.

  Nothing in Python reads these except code that is measuring the layout, and
  a managed dict is a whole allocation strategy -- the values are honest about
  the layout apython actually has.

- **Weak references live in a side table, not in the object, so
  `__weakrefoffset__` is 0 everywhere.**  The links are kept in one dict keyed
  by the referent's address and `obj_dealloc` consults it, which works and is
  tested; what it cannot do is answer CPython's question, because there is no
  offset to report.

  Putting the head in the object is not the 95-line edit it looks like.  The
  `PyTypeObject` field is the easy part.  The word itself has to go in the
  *instances* of all seventeen static types CPython lets you weak-reference --
  set, frozenset, type and both metatypes, function, builtin, method,
  generator, coroutine, async generator, module, memoryview, code, file,
  BytesIO, Task -- which means a field on each of those structs, a zeroing
  store at each of their forty-odd allocation sites, a release in each
  dealloc, and, because `type` is one of them, a second new `PyTypeObject`
  field and another pass over all 94 tables.  A missed zeroing store is a
  garbage pointer walked as a list.  The whole of it buys one number that
  still would not match CPython's, since the basicsizes it is an offset into
  differ anyway.

- **A frame object is a snapshot, so `f_lineno` is where the frame was when
  it was taken.**  CPython's is a live view onto a frame that is still
  running, and reports where it is when the attribute is READ:
  `f = sys._getframe()` on one line and `f.f_lineno` on the next answers the
  second line there and the first here.  Everything that reads it immediately
  -- which is every use in the stdlib -- agrees.  Making it live means the
  frame object holding the PyFrame rather than copying it, and the PyFrame
  outliving the call.

## The platform surface

- **`posix` is a subset, and a deliberate one.**  The file, directory and
  process calls `os.py` and `os.path` reach for are there, along with
  `environ`, `stat_result`, `error` and the O_*/W* constants -- enough that
  CPython's own `os.py` imports and works.  What is not: `scandir` and
  `DirEntry`, `fork`, `execv`, and the whole `*at` family.
  `_have_functions` is an empty list, which is the honest answer -- no
  `dir_fd=` support -- and os.py reads it to build `supports_dir_fd`.

- **`_thread` is a single-threaded stand-in.**  `lib/_thread.py` gives
  `get_ident` a constant, makes locks uncontended, and raises from
  `start_new_thread`.  Everything in the stdlib that only takes a lock works;
  anything that expects a second thread does not.

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler, so a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.
  The other half of why is in `bugs.md`: the codecs the interpreter can spell
  itself are utf-8, ascii and latin-1, and `surrogateescape` is not among the
  error handlers any of them accept.

## Performance, where the answer is already right

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

- **One builtin function type where CPython has four.**  CPython separates
  `builtin_function_or_method`, `method_descriptor`, `wrapper_descriptor` and
  a classmethod's bound form; here they are one type with a `func_kind` field,
  which is enough to repr all four the way CPython does but not enough to
  answer `type()` the way CPython does.  `type(int.from_bytes)` is `method`
  rather than `builtin_function_or_method`, and `type(list.append)` is
  `builtin_function_or_method` rather than `method_descriptor`.  Everything
  that asks *what* a descriptor is -- `hasattr(f, '__get__')`, `__set__`,
  the repr -- gets CPython's answer; only the type's name differs.

- **A class's `__dict__` does not carry `__dict__`, `__weakref__` or
  `__doc__`, and does carry `__qualname__`.**  CPython's `type_new` adds the
  first two as getset descriptors, sets `__doc__` to None when the body has
  no docstring, and moves `__qualname__` out of the dict onto the type.  Here
  the two descriptors do not exist -- they are the instance-dict and weakref
  layout recorded above -- `__doc__` is answered by a fallback rather than
  stored, and `__qualname__` stays in the dict because there is no field on
  the type to move it to.  Every one of those attributes reads correctly
  through the class; it is only `sorted(C.__dict__)` that differs.

- **A dict does not share its key table, so `gc.get_referents` reports its
  keys.**  CPython gives a dict whose keys are all strings a shared key table
  that the dict does not own, so its `tp_traverse` does not visit the keys and
  `gc.get_referents({"a": 1})` answers with the values alone.  Here the keys
  are the dict's own and are reported, which is what the traverse honestly
  walks.  Sharing key tables is a whole second dict layout, and the number it
  would buy is a report about the layout rather than about the program.

- **`gc.freeze()` moves nothing.**  CPython's permanent generation exists so
  that a program can freeze everything alive after startup and stop the
  collector dirtying those pages -- which matters to a forking server and to
  nothing else.  There is no permanent generation here; `freeze()` runs a
  full collection, `get_freeze_count()` stays at zero, and `unfreeze()` has
  nothing to undo, which is exactly what a program calling the pair in
  sequence would see either way.

## Interpreter structure

- **C code here cannot catch a Python exception.**  `raise_exception`
  tail-jumps into `eval_exception_unwind`, which resumes the eval loop from
  saved globals rather than returning through the C stack, so a `call` to a
  slot that raises never comes back.  `str.translate` gets around it by
  reaching a heaptype table through `dunder_call_2`, which does return; the
  general limit stands, and is why the `bytes %` leak recorded in `bugs.md`
  cannot be fixed by catching.

## Test oracles

Three tests compare against a recorded transcript in `tests/expected/` rather
than against CPython, because CPython cannot serve as an oracle for them:

- `test_sre.py` feeds hand-written SRE bytecode to `_sre.compile()`, a
  private API that does not validate its input; CPython segfaults on the
  group pattern it uses.
- `test_traceback_carets.py` and `test_unraisable.py` both let an exception
  be reported on stderr, and the report names the file: CPython absolutizes
  the path of a script it runs directly and a run from a `.pyc` does not.
  That is now the only difference between them and CPython.  stdout is
  block-buffered here as it is there, and flushed before an uncaught
  exception's report, so the two streams interleave the same way; every
  other line of both matches CPython 3.12 byte for byte.

Any *new* recorded-oracle test needs the same justification, or it risks
blessing a divergence instead of catching it.

## A generic class has no `Generic` base

`class C[T]` gives C its `__type_params__`, and everything a program can ask
about the parameters answers what CPython's does.  What it does not do is put
`Generic[T]` in the class's bases: CPython threads the parameter tuple through
a cell so the class BODY can see it and pass `Generic[T]` as an extra base, so
`C.__mro__` there is `(C, Generic, object)` and here it is `(C, object)`.

Nothing in this tree consumes `Generic` -- it exists in `lib/_typing.py` for
the intrinsic that builds `Generic[T]`, and nothing subscripts it for a
purpose -- so the cell and the extra base would be machinery with no reader.
`typing.Protocol` and the parts of `typing` that walk a generic's MRO would
need it; they are not here either.

Changing it means the cell: a `.type_params` cellvar in the wrapper scope,
`LOAD_CLOSURE` into the class body's own closure, and `INTRINSIC_SUBSCRIPT_GENERIC`
between the body and the `__build_class__` call.

## A builtin method bound to an instance is a `method`

`"x".upper` is a `builtin_function_or_method` in CPython and a `method` here.
Both are callable, both carry `__self__`, both repr as
`<built-in method upper of str object at 0x...>`, and both answer the same
`__name__` and `__qualname__`; what differs is `type()`.

CPython binds a builtin by making a copy of the descriptor with its receiver
stored inside it, which is why the type does not change.  Here the ordinary
bound-method object does the work, which is one object type instead of two
and one calling convention instead of two -- `method_call` prepends `im_self`
and dispatches through `im_func`'s `tp_call`, exactly as it does for a Python
function.

Changing it means a `func_self` field on `PyBuiltinObject`, a bound/unbound
distinction in `builtin_func_call`, and a second repr; the gain is the name
`type()` prints.  This is the same choice recorded above about having one
builtin callable type where CPython has four.

## `subprocess` cannot change the child's credentials, and says so

`_posixsubprocess.fork_exec` takes CPython's twenty-two arguments.  The ones
that decide what the child sees are honoured: the executable list, the three
pipes, the working directory, the environment, `close_fds`,
`start_new_session` and `preexec_fn`.  `restore_signals` is honoured by
having nothing to restore -- this interpreter installs no handlers, so a
child starts with the dispositions it inherited, which is what the flag asks
for.

The five to do with credentials -- `uid`, `gid`, `gids`, `umask` and
`process_group` -- raise `NotImplementedError`.  That is the divergence, and
it is deliberate: there are no setuid, setgid or setpgid syscalls here, and a
caller who passed `user=` to drop privileges must not be handed a child that
quietly kept them.  Refusing to run is the safe answer; running as root when
asked not to is not.

## A comprehension at module or class scope keeps a frame

PEP 709 inlines a list, set or dict comprehension into the block it is
written in, and that is what happens here inside a function or a lambda:
`sys._getframe().f_code.co_name` answers the enclosing function's name, a
traceback grows no extra entry, and `[super().m() for _ in r]` works because
`__class__` is the method's own free variable.  A generator expression is not
inlined -- PEP 709 does not inline one either, because its body runs later
from a frame of its own.

At MODULE and CLASS scope the comprehension still becomes a nested function.
Inlining needs the target to be a fast local, and CPython gets that by giving
one name two storages at once: for

    x = 5
    y = [x for x in range(3)]

its module code object has `x` in co_varnames AND in co_names -- a global for
the outer binding, a fast slot for the comprehension -- and decides which at
each USE.  This symbol table classifies a name once per scope, so it cannot
say both; expressing it would mean a per-use classification, which is a
larger change than the one effect it buys.  What is left is the co_name a
program reads from inside a module-level comprehension, and one extra
traceback entry through one.
