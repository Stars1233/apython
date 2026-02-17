# Assembly Style Guide

Rules for writing x86-64 NASM in this codebase. Read CLAUDE.md first for
architecture context (register convention, struct layouts, build commands).

## File Structure

Order within every `.asm` file:

1. Header comment (one or two lines describing the file)
2. `%include` directives (`macros.inc` first, then alphabetical)
3. Named frame-layout constants (`equ` block, if any)
4. `extern` declarations (grouped by category, one blank line between groups)
5. `section .text`
6. Function definitions
7. `section .data` / `.rodata` / `.bss` at end

```nasm
; frobnicate.asm - Frobnication subsystem for apython

%include "macros.inc"
%include "object.inc"
%include "types.inc"

; --- Named frame-layout constants ---
FR_OBJ     equ 8
FR_NAME    equ 16
FR_FRAME   equ 16

extern ap_malloc
extern str_from_cstr

extern raise_exception
extern exc_TypeError_type

section .text
```

## Opcode Handler Files

Repeat the register convention comment block at the top of every
`opcodes_*.asm` file:

```nasm
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = co_names tuple data pointer (&tuple.ob_item[0])
;
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.
```

## Naming

| Kind | Convention | Examples |
|------|-----------|----------|
| Global functions | `snake_case` | `type_call`, `str_from_cstr` |
| Local labels | `.dot_prefixed` | `.done`, `.not_found`, `.error` |
| Constants / `equ` | `UPPER_SNAKE` | `TAG_PTR`, `OBJ_HEADER_SIZE` |
| Frame-layout `equ` | `XX_FIELD` (2-3 char prefix + field) | `CL_NARGS`, `LA_OBJ` |
| Macros | `UPPER_SNAKE` | `VPUSH`, `DECREF_REG`, `DEF_FUNC` |
| Struct fields | `.dot_prefixed` | `.ob_refcnt`, `.tp_call` |

Local labels are scoped to the preceding global label. Use descriptive
names: `.check_overflow` not `.L1`.

## Function Definitions

Always use the `DEF_FUNC` / `END_FUNC` macros. Never write raw `global`
declarations or manual prologues.

```nasm
;; ============================================================================
;; func_name(arg_description) -> return_type
;; What this function does in one or two lines.
;; ============================================================================
DEF_FUNC func_name, FR_FRAME
    ; ...
    leave
    ret
END_FUNC func_name
```

**Three function forms:**

| Macro | Use when |
|-------|----------|
| `DEF_FUNC name` | Normal function (pushes rbp, sets up frame) |
| `DEF_FUNC name, N` | Function with N bytes of local stack space |
| `DEF_FUNC_BARE name` | Opcode handler or leaf function (no prologue) |

Opcode handlers use `DEF_FUNC_BARE` and end with `DISPATCH`:

```nasm
DEF_FUNC_BARE op_example
    ; ecx = arg
    ; ...
    DISPATCH
END_FUNC op_example
```

## Section Separators

Heavy separator for every function/handler:

```nasm
;; ============================================================================
;; function_name - Brief description
;; ============================================================================
```

Use double semicolons (`;;`) for the separator block. Use single semicolons
for inline comments.

## Comments

**Inline comments** — single semicolon, aligned near column 40:

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
- Labels flush left (column 0)
- Operands separated by comma-space: `mov rax, rbx`
- Hex constants: `0x` prefix, lowercase digits: `0x8080`, `0xff`
- One instruction per line, no continuations
- Blank line between logical blocks within a function
- One blank line between functions (separator comment provides visual break)

## Named Frame-Layout Constants

**Never use raw numeric offsets** like `[rbp-8]` or `[rsp+32]`. Define named
`equ` constants and reference them symbolically.

```nasm
; --- Named frame-layout constants ---

; op_store_attr frame layout
SA_OBJ    equ 8
SA_VAL    equ 16
SA_NAME   equ 24
SA_FRAME  equ 24

; In handler:
DEF_FUNC op_store_attr, SA_FRAME
    mov [rbp - SA_OBJ], rdi
    mov rsi, [rbp - SA_NAME]
```

Convention: 2-3 letter handler prefix + field name. Always include an
`XX_FRAME equ N` constant for the `DEF_FUNC` size argument.

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

## Register Safety

**The cardinal rule:** Never hold live values in caller-saved registers
(`rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`-`r11`) across any `call`
instruction or `DECREF`/`DECREF_REG`/`DECREF_VAL` macro invocation.

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

**Eval loop registers** (`rbx`, `r12`-`r15`) are callee-saved and hold
interpreter state. They survive calls automatically but must never be
repurposed within opcode handlers.

## Stack Macros

Use the provided macros for value stack operations. Never manipulate `r13`
with raw arithmetic unless implementing a new stack macro.

| Macro | Purpose |
|-------|---------|
| `VPUSH reg` | Push with auto-classification (SmallInt/NULL/PTR) |
| `VPUSH_PTR reg` | Push known heap pointer (TAG_PTR) |
| `VPUSH_INT reg` | Push known SmallInt (TAG_SMALLINT) |
| `VPUSH_NONE` | Push None |
| `VPUSH_BOOL reg` | Push bool (0 or 1) |
| `VPUSH_VAL pay, tag` | Push pre-classified 128-bit value |
| `VPOP reg` | Pop payload into reg |
| `VPOP_VAL pay, tag` | Pop payload + tag |
| `VPEEK reg` | Read TOS payload without popping |

Prefer typed pushes (`VPUSH_PTR`, `VPUSH_INT`) over `VPUSH` when the
type is statically known — they avoid branching.

## Refcounting Macros

| Macro | Use when |
|-------|----------|
| `INCREF reg` | Known heap pointer (64-bit, skips SmallInt) |
| `DECREF reg` | Known heap pointer (saves/restores rdi) |
| `DECREF_REG reg` | Known heap pointer (does NOT save rdi) |
| `XDECREF reg` | Possibly NULL heap pointer |
| `INCREF_VAL pay, tag` | 128-bit fat value |
| `DECREF_VAL pay, tag` | 128-bit fat value (clobbers rdi + caller-saved) |
| `XDECREF_VAL pay, tag` | 128-bit fat value, NULL-safe |

`DECREF_REG` and `DECREF_VAL` contain `call obj_dealloc` which **clobbers
all caller-saved registers** when the refcount reaches zero.

## Addressing Idioms

**Localsplus indexing** (16 bytes/slot = ×8 × ×2 via LEA):

```nasm
lea rdx, [rcx*8]                                  ; slot * 8
mov rdi, [r12 + rdx*2 + PyFrame.localsplus]        ; payload
mov r9,  [r12 + rdx*2 + PyFrame.localsplus + 8]    ; tag
```

**Forward bytecode jumps** (instruction words → bytes = ×2):

```nasm
lea rbx, [rbx + rcx*2]     ; advance IP by arg words
```

**co_names / co_consts lookup** (pointer array, 8 bytes/entry):

```nasm
mov rax, [r14 + rcx*8]     ; co_consts[arg]
mov rsi, [r15 + rcx*8]     ; co_names[arg]
```

## Error Handling

Place error paths after the main logic, at the end of the function. Jump
forward to them from the main flow:

```nasm
DEF_FUNC_BARE op_example
    ; ... main logic ...
    test rax, rax
    jz .error
    ; ... success path ...
    DISPATCH

.error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "descriptive error message"
    call raise_exception        ; does not return
END_FUNC op_example
```

`raise_exception` does not return. No `ret` or `DISPATCH` needed after it.

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

- Align type objects and tables to 8 bytes
- Null-terminate all C strings
- Group related string constants together
- Use `CSTRING reg, "text"` macro for inline rodata strings in code

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
- **Caller-saved values across calls** — will be silently clobbered
- **Fall-through between functions** — every function is entered via `call` or `jmp`
- **Magic numbers** — define as `equ` or `%define` with descriptive names
- **`global` without `DEF_FUNC`** — the macros emit ELF size metadata for GDB
- **Missing `END_FUNC`** — breaks GDB function boundaries
- **`VPUSH` when type is known** — use typed push to avoid branches
