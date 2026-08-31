# Assembly Style Guide

Rules for writing x86-64 NASM in this codebase.  Read CLAUDE.md first for
architecture context (register convention, struct layouts, build commands);
the two overlap deliberately, and where both speak they agree.

This guide covers `src/` and `compiler/` both.  Where the two differ, the
difference is called out; the compiler's own rules are collected under
[`compiler/` differs](#compiler-differs).

## What is mechanically enforced

NASM is invoked with **no warning flags** (`-f elf64 -I include/ -I compiler/
-g -F dwarf`).  It will not catch a 4-byte/8-byte field mismatch, a misaligned
frame, or a clobbered callee-saved register.  `compiler/lint.py` is the only
net, and it runs inside `make check`.

| Rule | Check | Severity |
|------|-------|----------|
| A `resd 1` field is read into a **32-bit** register | `check_field_widths` | error |
| `(frame + 8*pushes) % 16 == 0` in any function containing a `call` | `check_alignment` | error |
| A tail `jmp` to another global function comes only from `DEF_FUNC_BARE` | `check_tailjumps` | error |
| No `DEF_FUNC*` while a data section is current | `check_section` | error |
| Every `ret` pops an exact mirror of the entry pushes | `check_callee_saved` | error |
| `rbx`, `r12`-`r15` are never written without being pushed first | `check_saved_writes` | error |

**Scope:** lint reads `compiler/*.asm` and `src/main.asm` — under half the
`.asm` files.  Everywhere else in `src/` these six rules are convention only, and older
files predate them.  Write new code to the rules regardless; the lint boundary is
a limit on detection, not on what is correct.

Lint's reach depends on structure, so the layout rules below are load-bearing:
`DEF_FUNC` and `END_FUNC` must sit flush at column 0, and a function missing its
`END_FUNC` is invisible to four of the six checks.

## File Structure

Order within every `.asm` file:

1. Header comment — what the file is, and the invariant it keeps
2. `%include` directives, in dependency order (`macros.inc` first)
3. `extern` declarations, grouped by category, one blank line between groups
4. `section .text`
5. Function definitions, each preceded by its own frame-layout `equ` block
6. `section .data` / `.rodata` / `.bss`

```nasm
; frobnicate.asm - Frobnication subsystem for apython
;
; Every frobnicator leaves exactly one value on the stack.  The depth checker
; in assemble.asm verifies it, so an emitter cannot get this wrong silently.

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern str_from_cstr

extern raise_exception
extern exc_TypeError_type

section .text
```

In `compiler/`, `compiler.inc` is always the last include, and the file's last
line is a bare `ASM_INIT`.

A one-line header is fine for a small file, but the mature files state the
invariant that makes the file reviewable — see `compiler/codegen.asm` and
`compiler/comperr.asm`.  That is the form to imitate.

Two relaxations the codebase uses on purpose:

- **`extern` next to first use.**  Declaring an `extern` mid-file, immediately
  above the function that needs it, is accepted and common in the large opcode
  files.  Keep the top-of-file block for the ones used throughout.
- **Interleaved data.**  Data does not have to be last; a function may be
  followed by the `.rodata` it uses and then more code.  The rule is not "data
  last" but **always write an explicit `section .text` before a function**.
  Do not rely on `ASM_INIT` to put you back — lint does not model it, and a
  function emitted while `.rodata` is current links fine and then faults on its
  own `push rbp`.

## Opcode Handler Files

Repeat the register convention comment block at the top of every
`src/opcodes/*.asm` file:

```nasm
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer (Value[], one 64-bit word per slot)
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = free
;
; co_names is accessed via the LOAD_CO_NAMES macro (reads a global).
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.
```

## Naming

| Kind | Convention | Examples |
|------|-----------|----------|
| Global functions | `snake_case` | `type_call`, `str_from_cstr` |
| Local labels | `.dot_prefixed` | `.fail`, `.done`, `.not_found` |
| Constants / `equ` | `UPPER_SNAKE` | `TAG_PTR`, `OBJ_HEADER_SIZE` |
| Frame-layout `equ` | `XX_FIELD` (2-3 char prefix + field) | `CL_NARGS`, `LA_OBJ` |
| Macros | `UPPER_SNAKE` | `VPUSH`, `DECREF_REG`, `DEF_FUNC` |
| Struct fields | `.dot_prefixed` | `.ob_refcnt`, `.tp_call` |

Local labels are scoped to the preceding global label. Use descriptive
names: `.check_overflow` not `.L1` — the tree has zero `.L1`-style labels.

The settled names for the common exits are `.fail` (error), `.done` and `.ret`.
Some files in `src/pyo/` and the opcode files additionally prefix by function
initials — `.dl_probe`, `.dl_miss` inside `dict_lookup` — which NASM's scoping
makes redundant but which reads better in a GDB disassembly.  Either is fine;
be consistent within a file.

## Function Definitions

Always use the `DEF_FUNC` / `END_FUNC` macros. Never write a raw `global`
declaration plus a bare `label:` — the macros emit the ELF size metadata that
GDB needs for backtraces and `disassemble`.

```nasm
;; ============================================================================
;; func_name(rdi = arg, rsi = arg) -> rax = return value, 0 on error
;; What this function does, and anything a caller must know.
;; ============================================================================
DEF_FUNC func_name, FR_FRAME
    ; ...
    leave
    ret
END_FUNC func_name
```

**Five function forms:**

| Macro | Emits | Use when |
|-------|-------|----------|
| `DEF_FUNC name` | `global` + `push rbp` / `mov rbp,rsp` | Normal function |
| `DEF_FUNC name, N` | Same + `sub rsp, N` | Function with N bytes of locals |
| `DEF_FUNC_BARE name` | `global` + label, **no prologue** | Opcode handler, leaf function, or any function that tail-jumps |
| `DEF_FUNC_LOCAL name` | Prologue but **no `global`** | File-local helper (no ELF size, so invisible to GDB by name) |
| `DEF_FUNC_LOCAL name, N` | Same + `sub rsp, N` | ditto, with locals |

`END_FUNC name` must name the same symbol, or the size expression fails to link.

**A tail `jmp` to another global function is legal only from `DEF_FUNC_BARE`.**
A `DEF_FUNC` has already pushed `rbp`; returning through the callee's
`leave; ret` tears down the wrong frame.  Tail calls are preferred over
`call`+`ret` when nothing needs doing after the call — just declare the caller
bare.

Opcode handlers use `DEF_FUNC_BARE` and end with `DISPATCH`:

```nasm
DEF_FUNC_BARE op_example
    ; ecx = arg
    ; ...
    DISPATCH
END_FUNC op_example
```

A handler that needs frame locals uses `DEF_FUNC op_example, XX_FRAME` instead
and ends `leave` then `DISPATCH` — about a quarter of them do.  `DISPATCH`
never returns, so the `leave` must come first.

**Skip the CACHE words before dispatching.**  Advance `rbx` by `2*N` bytes for
an opcode with N trailing CACHE entries, with a comment naming the count:

```nasm
    add rbx, 18            ; skip 9 CACHE entries
    DISPATCH
```

The counts are listed in CLAUDE.md and in `include/opcodes.inc`.  The
`CACHE_*` constants there are currently unreferenced — every handler hardcodes
the byte count — so the comment is the only thing tying the two together.  Get
this wrong and execution resumes in the middle of an instruction.

## Section Separators

Heavy separator for every function/handler, exactly 79 columns (`;; ` and 76
`=`):

```nasm
;; ============================================================================
;; function_name(rdi = what) -> rax = what
;; Brief description.
;; ============================================================================
```

Use double semicolons (`;;`) for the separator block and its text. Use single
semicolons for inline comments.  Give the signature in the C-like form above,
with the register each argument arrives in, and say what `rax` means on the
way out — including what a failure looks like.

## Comments

**Inline comments** — single semicolon, aligned near column 32:

```nasm
    mov rdi, [r12 + PyFrame.code]   ; rdi = code object
    lea rbx, [rdi + PyCodeObject.co_code]
    VPOP rax                        ; rax = return value
```

Comment the *why* and the *what*, not the instruction mnemonic. Bad:
`; move rax to rdi`. Good: `; rdi = callable object for tp_call`.

**When to comment:**
- Every register assignment that establishes a new binding
- Every non-obvious branch condition
- Phase transitions in multi-step algorithms (`; === Phase 2: bind kwargs ===`)
- Not every line — skip when the code is self-evident

## Formatting

- **Lowercase** everything: instructions, registers, directives
- **4-space indentation** for instructions under labels
- Labels flush left (column 0) — including `DEF_FUNC` and `END_FUNC`
- Operands separated by comma-space: `mov rax, rbx`
- Hex constants: `0x` prefix, lowercase digits: `0x8080`, `0xff`
- One instruction per line, no continuations
- Blank line between logical blocks within a function
- One blank line between functions (separator comment provides visual break)

## Named Frame-Layout Constants

**Never use raw numeric offsets** like `[rbp-8]` or `[rsp+32]`. Define named
`equ` constants and reference them symbolically.

Put the block immediately above the function it describes, between the
docblock and the `DEF_FUNC` — that is where two thirds of them live, and it
keeps a frame layout next to the only code that can be wrong about it:

```nasm
;; ============================================================================
;; op_store_attr - Store an attribute
;; ============================================================================
SA_OBJ    equ 8
SA_VAL    equ 16
SA_NAME   equ 24
SA_FRAME  equ 24            ; + 1 push = 32
DEF_FUNC op_store_attr, SA_FRAME
    mov [rbp - SA_OBJ], rdi
    mov rsi, [rbp - SA_NAME]
```

A top-of-file block under `; --- Named frame-layout constants ---` is the
alternative, and suits a file whose handlers share a layout.  Either way, keep
a constant next to the code that uses it.

Convention: 2-3 letter handler prefix + field name. Always include an
`XX_FRAME equ N` constant for the `DEF_FUNC` size argument, and **give it a
trailing comment showing the alignment arithmetic** — `; + 1 push = 32`.  It is how a reader checks the
alignment rule below without re-counting the pushes.

`rsp`-relative scratch, in push-based layouts, is conventionally written raw
(`[rsp + 8]`); it is `rbp`-relative frames that must be named.

Derive an offset that sits below a struct rather than hand-picking it —
`CS_UNIT equ 48 + CompUnit_size` — or the struct silently overlaps the scalar
slots above it the first time it grows.

Older files in `src/` still use raw offsets; they are legacy, not a pattern to
copy.  Do not add more, and prefer converting a function you are already
editing.

## Struct Field Access

Always use named struct fields from `.inc` files. Never hardcode byte offsets:

```nasm
; Good:
mov rax, [rdi + PyObject.ob_type]
mov rcx, [rax + PyTypeObject.tp_call]

; Bad:
mov rax, [rdi + 8]
mov rcx, [rax + 64]
```

**A field declared `resd 1` is four bytes and must be read into a 32-bit
register.** The 64-bit form assembles silently and ORs the next field in as the
high half:

```nasm
mov edx, [rsi + Token.len]      ; correct — zero-extends into rdx
mov rdx, [rsi + Token.len]      ; wrong — picks up Token.col too
```

This is lint's headline check.  It sees loads only, so a *store* through a
64-bit register, or a `lea`/`imul`/`shl` on such a field, is on you.

The check derives its field list from `resd 1` declarations, so **declaring a
4-byte field `resq` to avoid the fuss silently disables the check** for it.
Declare the width the field actually is.

## Header Files

```nasm
; frobnicate.inc - Frobnicator layout

%ifndef FROBNICATE_INC
%define FROBNICATE_INC

struc PyObject
    .ob_refcnt: resq 1    ; +0: reference count (int64)
    .ob_type:   resq 1    ; +8  pointer to type object
endstruc

%endif ; FROBNICATE_INC
```

- Include guard on every `.inc`, named for the file, echoed on the `%endif`
- Every struct field carries its byte offset in a trailing comment
- NASM's generated `Name_size` is the canonical `tp_basicsize`; do not
  hand-count a struct's length
- Declare each field at its real width (see above)

## Addressing Globals

**Every reference to a global symbol is rip-relative.**  There is no
`default rel`, so write `[rel …]` explicitly, every time:

```nasm
lea rdi, [rel exc_TypeError_type]
mov rax, [rel eval_co_names]
```

The tree has thousands of these and no absolute references.  A bare `[symbol]` is
an absolute 32-bit displacement; it happens to link under `-no-pie` and is
still wrong here.

## Stack Alignment

The SysV ABI wants `rsp` 16-byte aligned at every `call`.  After `DEF_FUNC`'s
`push rbp`, a `sub rsp, N` and P register pushes, that means:

```
(N + 8*P) % 16 == 0
```

**Pad the frame, not the push list** — the pushes are there because the values
are needed.  Much of `src/` predates this rule and violates it harmlessly, but
anything that reaches libc must obey it: `compiler/` calls `strtod`, and glibc's
float paths use aligned SSE stores.

Two mechanics worth knowing:

- `; lint: pushes=N` on the `DEF_FUNC` line overrides the counted push run, for
  a function whose alignment is set up on a path lint cannot see.  See
  `compiler/arena.asm`.
- A frame size written as plain arithmetic is checked; one written in terms of
  a struct size (`CS_UNIT equ 48 + CompUnit_size`) silently opts the function
  out.  Such a function needs the arithmetic done by hand.

## Register Safety

**The cardinal rule:** never hold live values in caller-saved registers
(`rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`-`r11`) across any `call` or any
refcount macro that can deallocate.

Two safe patterns for preserving values across calls:
1. Push/pop onto the machine stack
2. Store into `[rbp - XX_FIELD]` frame locals

```nasm
; Safe: save before call, restore after
    push rax
    push rdx
    call some_function
    pop rdx
    pop rax

; Also safe: use frame locals
    mov [rbp - SA_OBJ], rdi
    call some_function
    mov rdi, [rbp - SA_OBJ]
```

**The callee-saved side has two rules of its own**, both enforced:

- Never write `rbx`, `r12`, `r13`, `r14` or `r15` without pushing it first.
  This applies inside `DEF_FUNC_BARE` too.  `main` holds argc and argv in r14
  and r15 across the whole compile, so a scratch r14 in an emitter hands back a
  different argv and the crash lands somewhere unrelated.
- Every `ret` must pop an exact mirror of the entry pushes, in order — the
  error paths at the bottom of the function included.  A function with local
  subroutines (`call .label`) is exempt, because those inner `ret`s are not
  function returns.

**Eval loop registers** (`rbx`, `r12`, `r13`, `r14`) hold interpreter state and
must never be repurposed within an opcode handler.  `r15` is free **for opcode
handlers in `src/`**; in `compiler/` it is an ordinary callee-saved register and
must be saved like the rest.

## Stack Macros

Use the provided macros for value stack operations. Never manipulate `r13`
with raw arithmetic unless implementing a new stack macro.

| Macro | Purpose |
|-------|---------|
| `VPUSH reg` | Push a Value already in encoded form |
| `VPUSH_PTR reg` | Push a heap pointer (a pointer is its own Value) |
| `VPUSH_INT reg, scratch` | Push an int64, boxing it if it exceeds ±2^50 |
| `VPUSH_FLOAT reg, scratch` | Push raw double bits |
| `VPUSH_NONE` | Push None |
| `VPUSH_BOOL reg` | Push a bool (0 or 1); clobbers `reg` |
| `VPUSH_NULL` | Push the NULL Value (CALL's empty callable slot) |
| `VPOP reg` | Pop one Value |
| `VPEEK reg` | Read TOS without popping (TOS only — there is no depth form) |
| `VUNDROP n` | Re-advance `r13` over n intact slots (deopt paths) |
| `FRAME_PUSH_NONE frame, scratch` | Push None onto *another* frame's stack |
| `FRAME_PUSH_VALUE frame, val, scratch` | Same, arbitrary Value |
| `SPUSH_PTR reg` | One-argument array on the machine stack for `tp_call`; 16 bytes, so `rsp` stays aligned.  Caller does `add rsp, 16` |
| `SAVE_FAT_RESULT` / `RESTORE_FAT_RESULT` | Park a `(rax, rdx)` pair.  **Every `rsp`-relative offset shifts by 16 in between** |
| `SAVE_EVAL_REGS` / `RESTORE_EVAL_REGS` | Push/pop `rbx, r12-r15`.  Five pushes, so alignment flips |

`VPUSH_INT`, `VPUSH_FLOAT` and `VPUSH_VAL` clobber **both** operands, and their
scratch register must not be `rax`.  `VPUSH_INT` can call `val_from_i64_p` on
the overflow path, so it clobbers caller-saved registers too.

## Value Macros

The Value encoding is described in CLAUDE.md and `valuebox.md`; these are
the macros that implement it, all in `include/value.inc`.

**Classify** — each takes the value and a scratch register, which must differ:

| Macro | Branch after it |
|-------|-----------------|
| `V_TEST_PTR v, scratch` | `jbe` = a real non-NULL pointer, `ja` = not |
| `V_IS_INT v, scratch` | `jae` = int immediate |
| `V_IS_FLOAT v, scratch` | `jb` = float |
| `V_TEST_PTR_M m64, scratch` | as `V_TEST_PTR`, on a memory operand |
| `V_TEST_INT_M m64, scratch` | as `V_IS_INT`, on a memory operand |
| `V_TEST_F64_M m64, scratch` | as `V_IS_FLOAT`, on a memory operand |
| `V_TAG_OF tag, v` | Derive the legacy tag; `v` may be memory and is preserved |

**Convert:**

| Macro | Notes |
|-------|-------|
| `V_FROM_F64 bits, scratch` | In place; canonicalises tag-space NaNs |
| `V_TO_F64 v` | In place; caller must already know it is a float |
| `V_FROM_I64 i, scratch, ovf_label` | Branches to `ovf_label`; **caller boxes** |
| `V_TO_I64 v` | In place |
| `V_PACK_I64 i, scratch` | Boxes on overflow — **contains a call** |
| `V_PACK pay, tag` | `(payload, tag)` -> Value; may reach `V_PACK_I64`'s call |
| `V_UNPACK v, tag` | Value -> `(payload, tag)` |

`V_PACK` and `V_UNPACK` are the most-used macros in the tree.  They are
scaffolding from the single-word migration, but they are not rare and not
deprecated at the boundary: use them where a converted function meets one that
still speaks `(payload, tag)`, and do not use them *inside* code that is already
Value-native.  A value that is packed at a function's exit must not also be
packed by a tail `jmp` target, and a call site must not decode a result its
callee already handed over as a Value — both show up as a value off by exactly
2^48 or by `V_INT_BIAS`, not as a crash.

`V_PACK`, `V_PACK_I64` and `VPUSH_VAL` clobber their second operand, which must
not be `rax`.  `V_PACK` on a TAG_SMALLINT outside ±2^50 allocates; the reference
it returns is owned.

## Refcounting Macros

There are two mechanisms: the out-of-line functions and the inline macros.

| Form | Use when |
|------|----------|
| `call obj_decref` / `call obj_incref` | The pointer is already in `rdi`.  NULL-safe, out of line.  This is the default, and the overwhelming majority of decref sites |
| `INCREF reg` | A known non-NULL heap pointer, inline |
| `DECREF_REG reg` | A known heap pointer, inline; clobbers `rdi` |
| `INCREF_V value, scratch` | A Value: no-op unless it holds a pointer |
| `DECREF_V value, scratch` | A Value; NULL-safe |
| `XDECREF_V value, scratch` | Same as `DECREF_V`, spelled for clarity at NULL-able sites |

`DECREF reg` still exists but is all but unused; prefer `call obj_decref` or
`DECREF_REG`.

**`INCREF` does not check anything.**  It is an unconditional
`inc qword [reg + PyObject.ob_refcnt]` — the `%%skip:` label inside it is dead,
left over from when it guarded against SmallInts.  Handing it an immediate int
or a NULL writes through a non-pointer.  Use `INCREF_V` when the operand is a
Value whose kind you have not established, or `call obj_incref`, which at least
checks for NULL.

**Every `DECREF` form contains `call obj_dealloc`** on the refcount-zero path —
`DECREF`, `DECREF_REG`, `DECREF_V`, `XDECREF_V` and the `_VAL` shims alike.
All caller-saved registers die there.  Only plain `DECREF` saves anything, and
all it saves is `rdi`; `DECREF_REG`, `DECREF_V`, `XDECREF_V`, `DECREF_VAL` and
`XDECREF_VAL` load the operand straight into `rdi` and clobber it.

`INCREF_VAL` / `DECREF_VAL` / `XDECREF_VAL` are the `(payload, tag)` shims;
prefer the `_V` forms in Value-native code.

## Other Macros

| Macro | Notes |
|-------|-------|
| `DISPATCH` | Decode and jump.  Clobbers `rax`, `rcx`, `rdx`; advances `rbx` by 2; never returns |
| `CSTRING reg, "text"` | Inline rodata string.  **Always leaves you in `.text`** — using it from a data section relocates what follows |
| `LOAD_CO_NAMES reg` | `co_names` data pointer |
| `RET_NULL` | Error return: `xor eax,eax` / `xor edx,edx` |
| `RET_NONE` | Owned None in `rax` **and `edx = TAG_PTR`**; clobbers `rdx` |
| `RET_BOOL_RAX` | 0/1 in `rax` -> owned bool + `edx = TAG_PTR` |
| `LOAD_NONE dst` | Owned None into `dst`; clobbers only `dst` |
| `IS_NONE v, scratch` | ZF=1 means None; `v` may be memory |
| `MRO_NEXT walker, origin` | **Calls `type_mro_next`** — all caller-saved die.  Breaks if `walker` is `rdi` or `origin` is `rsi` |
| `DUNDER_EXC_SAVE slot` | Snapshot `current_exception` before a call |
| `DUNDER_RAISED slot, label` | Jump to `label` if it changed |
| `REQUIRE_LIST_TYPE t, scratch, fail` | `t` is a **PyTypeObject\***, not the object.  Likewise `_TUPLE_`, `_DICT_`, `_STR_`, `_INT_` |
| `LOAD_INST_DICT dst, inst, none_label` | Instance dict, honouring `TP_DICT_AT_TAIL` |
| `STORE_INST_DICT inst, val, scratch, none_label` | The mirror |
| `C_RECURSION_ENTER label` / `C_RECURSION_LEAVE` | Native recursion guard; ENTER clobbers `rax` |
| `INT_NEED_MPZ ptr` | Promote a compact `PyIntObject` in place |
| `ENTRY_CLASSIFY entry, empty, tomb` | Dict probe; falls through when occupied |
| `VISIT_V v, scratch` / `VISIT_PTR p` | GC traverse — **`call r14`**, so `r14` holds the callback and caller-saved die |

`DUNDER_EXC_SAVE` / `DUNDER_RAISED` exist because `current_exception` is also
the exception *being handled*: it stays set for the length of an `except` block,
so `cmp qword [rel current_exception], 0` cannot mean "did that call raise?".

## Macro Hazards

- **`macros.inc` only includes `value.inc`.**  Anything touching a struct --
  `INCREF`/`DECREF`, `REQUIRE_*`, `FRAME_PUSH_*` -- needs `object.inc` too.
  Omit it and the field offsets silently resolve to 0.
- **Singletons are not declared for you.**  A file using `VPUSH_NONE`,
  `RET_NONE`, `LOAD_NONE`, `IS_NONE` or `VPUSH_BOOL` must `extern
  none_singleton` / `bool_true` / `bool_false` itself.
- **Do not inline a macro's expansion by hand.**  Every one of these exists
  because the open-coded version was got wrong at least once.

## Addressing Idioms

**Localsplus indexing** (one Value per slot):

```nasm
mov rdi, [r12 + rcx*8 + PyFrame.localsplus]        ; the local's Value
```

**Forward bytecode jumps** (instruction words -> bytes = x2):

```nasm
lea rbx, [rbx + rcx*2]     ; advance IP by arg words
```

**co_names / co_consts lookup** (pointer array, 8 bytes/entry):

```nasm
mov rax, [r14 + rcx*8]     ; co_consts[arg]
LOAD_CO_NAMES rsi
mov rsi, [rsi + rcx*8]     ; co_names[arg]
```

A `co_consts` slot holds a **Value**, not necessarily a pointer: `class C: 42`
puts an immediate int there, and reading `ob_type` off one dereferences the
number.  Classify before dereferencing.

## Error Handling

Place error paths after the main logic, at the end of the function. Jump
forward to them from the main flow:

```nasm
DEF_FUNC_BARE op_example
    ; ... main logic ...
    test rax, rax
    jz .fail
    ; ... success path ...
    DISPATCH

.fail:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "descriptive error message"
    call raise_exception        ; does not return
END_FUNC op_example
```

`raise_exception` does not return. No `ret` or `DISPATCH` needed after it.

An error path is still a return path: if the function pushed callee-saved
registers, the error exit pops them in the same mirrored order as the success
exit.

## `compiler/` differs

The source compiler is a peer subsystem to `src/`, with its own error protocol
and its own lifetimes.  All six lint checks apply to it, and only to it.

**Never call `raise_exception` from `compiler/`.**  It tail-jumps into
`eval_exception_unwind`, which calls `fatal_error` when there is no live
interpreter frame — and `./apython foo.py` compiles before any frame exists.
Record the error with `comp_error()` and return 0/NULL; the driver turns it into
a pending exception after every buffer is freed.  `comp_error` itself returns 0
so a failing path can `jmp` into it and be done.

The exception is `evalexec.asm` — `compile()`, `exec()` and `eval()` are called
*from* a running frame, so raising there is correct.  The rule is about the
paths reachable from `main`.

**The first error wins.**  The parser keeps running after one is recorded, in a
panic mode where the token cursor reports ENDMARKER forever, so loops terminate
without every call site checking.  The message the user sees must be the first
thing that actually went wrong, not the last confused thing the parser said.

**`CompUnit.names` and `.consts` hold borrowed references** — the object arena
owns them.  Interning a string at the call site and releasing it leaves a
dangling pointer whose symptom is a wild jump inside `dict_lookup` at run time.
Go in through `comp_intern_cstr` / `comp_intern_keep`.

**`r15` is not free here.**  See [Register Safety](#register-safety).

## Data Sections

```nasm
section .rodata
method_name_cstr: db "__init__", 0

section .data
align 8
global my_type
my_type:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq my_name_str          ; tp_name
    ; ... remaining type slots ...
```

- A static type object is a flat run of `dq` lines in `PyTypeObject` order, one
  per slot, each with a trailing `; tp_field` comment.  The tree uses no
  `istruc`/`at`/`iend`; do not introduce it
- Align type objects and tables to 8 bytes
- Null-terminate all C strings
- Group related string constants together
- Use `CSTRING reg, "text"` macro for inline rodata strings in code
- `global` is right for a data symbol; it is only functions that get it from
  `DEF_FUNC`

Always follow a data section with an explicit `section .text` before the next
function.

## Encoding Micro-Optimizations

Prefer shorter encodings when semantically equivalent:

| Prefer | Over | Why |
|--------|------|-----|
| `xor eax, eax` | `mov rax, 0` | 2 bytes vs 7, breaks dep chains |
| `test eax, eax` | `test rax, rax` | 2 bytes vs 3 (when 32-bit safe) |
| `test reg, reg` | `cmp reg, 0` | Shorter, same flags |
| `movzx eax, byte [m]` | `movzx rax, byte [m]` | Shorter, same result |
| `lea` | `shl` + `add` | No flags clobber, often fewer insns |
| `inc` / `dec` | `add 1` / `sub 1` | 1 byte shorter (no partial-flag stall on Haswell+) |

## What to Avoid

- **Raw offsets** — always use struct fields and named frame constants
- **A 64-bit read of a `resd 1` field** — silently ORs in the next field
- **A bare `[symbol]`** — every global reference is `[rel symbol]`
- **Caller-saved values across calls** — will be silently clobbered
- **Writing a callee-saved register you did not push**
- **A return path that pops something other than what the entry pushed**
- **A tail `jmp` from a function that pushed `rbp`** — use `DEF_FUNC_BARE`
- **A function emitted while a data section is current** — links fine, then
  faults on its own `push rbp`
- **Fall-through between functions** — every function is entered via `call` or `jmp`
- **Magic numbers** — define as `equ` or `%define` with descriptive names
- **`global` + bare label for a function** — use `DEF_FUNC`, which emits the
  ELF size metadata GDB needs
- **Missing `END_FUNC`** — breaks GDB function boundaries, and blinds lint to
  the rest of the file
- **`VPUSH` when the type is known** — use the typed push to avoid branches
- **Deleting a load but keeping its guard** — a `test`/`jz` left behind reads a
  stale register, and fails silently
