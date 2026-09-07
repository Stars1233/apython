#!/usr/bin/env python3
"""Static checks over the assembly, for bug classes that are invisible at
assembly time and expensive to find at runtime.

Most checks run over every hand-written .asm in the tree; the rest are scoped
to src/compiler plus src/main.asm, because the rest of src/ predates the
alignment rule and would drown the signal.  See STYLE.md for which is which.

The two that started it both bit during development:

  1. Reading a 4-byte struct field with a 64-bit `mov`.  NASM assembles it
     happily; it silently picks up the next field as the high half.  A
     `mov rdx, [rsi + Token.len]` read the length OR'd with the column, and
     turned into a multi-gigabyte memcpy.

  2. A call made with rsp not 16-byte aligned.  The SysV ABI requires it and
     glibc's floating-point paths (strtod, which the number scanner uses) do
     use aligned SSE stores.  After DEF_FUNC's `push rbp`, `sub rsp, N` and P
     register pushes, alignment holds when (N + 8*P) is a multiple of 16.

The tree-wide six are there on the opposite grounds: the tree already had zero
violations of each, so turning them on cost nothing and keeps it that way.  The
exception was the 4-byte field check, which found one real 8-byte read of
SRE_PatternObject.flags the moment include/sre.inc was added to its field list.

Run standalone, or as part of `make check`.
"""
import re, sys, glob, os

# .../src/compiler/lint.py -> the repo root is three levels up.
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
R64 = (r'\b(?:rax|rbx|rcx|rdx|rsi|rdi|rbp|rsp|r8|r9|r10|r11|r12|r13|r14|r15)\b')

def dword_fields(paths):
    """Struct fields declared `resd 1`, as "Struct.field"."""
    out = set()
    for p in paths:
        src = open(p).read()
        for m in re.finditer(r'struc\s+(\w+)(.*?)endstruc', src, re.S):
            st, body = m.group(1), m.group(2)
            for f in re.finditer(r'^\s*\.(\w+):\s*resd\s+1\s*(?:;.*)?$', body, re.M):
                out.add("%s.%s" % (st, f.group(1)))
    return out

def check_field_widths(files, fields):
    bad = []
    for path in files:
        for n, line in enumerate(open(path), 1):
            code = line.split(';')[0]
            m = re.match(r'\s*(?:mov|add|sub|or|and|cmp|test)\s+(%s)\s*,\s*\[([^\]]+)\]' % R64, code)
            if not m:
                continue
            for fld in fields:
                if re.search(r'\b%s\b' % re.escape(fld), m.group(2)):
                    bad.append((path, n, "64-bit read of 4-byte field %s" % fld, code.strip()))
    return bad

def check_tailjumps(files):
    """A DEF_FUNC pushes rbp; tail-jumping out of one leaks that frame.

    `DEF_FUNC f / ... / jmp g / END_FUNC` returns through g's `leave; ret`,
    which pops the wrong thing and lands on a corrupted stack.  Tail-jumps are
    fine, but only from DEF_FUNC_BARE.

    A jump to a function that never returns is not a tail call and does not
    have this problem: nothing comes back through the caller's frame, because
    nothing comes back at all.  Those targets are listed in NORETURN.
    """
    bad = []
    for path in files:
        src = open(path).read()
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL)?)\s+(\w+)(?:\s*,[^\n]*)?$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, body = m.group(2), m.group(3)
            for jm in re.finditer(r'^\s*jmp\s+([a-z_][a-z0-9_]*)\s*(?:;.*)?$', body, re.M):
                target = jm.group(1)
                if target in NORETURN:
                    continue
                # A jump to a global function, not a local label or a register.
                if re.search(r'^(DEF_FUNC(_BARE|_LOCAL)?)\s+%s\b' % re.escape(target),
                             "\n".join(open(p).read() for p in files), re.M) \
                   or target in ('buf_grow','buf_push_u32','buf_push_ptr','str_new_heap',
                                 'cg_has_star','asm_loc_varint','asm_effect_var'):
                    bad.append((path, 0,
                                "%s tail-jumps to %s but pushed rbp" % (name, target),
                                "use DEF_FUNC_BARE for a tail-jump"))
    return bad

# Functions that never return to their caller.  Jumping to one of these is an
# unwind or an abort, not a tail call, so the caller's frame is irrelevant --
# eval_exception_unwind alone accounts for 34 of the 39 jumps in src/.
NORETURN = frozenset((
    'eval_exception_unwind',
    'raise_exception',
    'raise_exception_obj',
    'raise_type_error_with_name',
    'raise_no_attribute',
    'fatal_error',
))

def check_section(files):
    """A function defined while a data section is current lands in that section.

    NASM is happy to emit code into .rodata, and it links; the fault comes when
    something calls it and the CPU refuses to execute non-executable memory.
    The symptom is a SIGSEGV on the function's own `push rbp`, which looks like
    stack corruption and is not.
    """
    bad = []
    for path in files:
        section = 'text'
        for n, line in enumerate(open(path), 1):
            code = line.split(';')[0].strip()
            m = re.match(r'section\s+\.(\w+)', code)
            if m:
                section = m.group(1)
                continue
            if re.match(r'(DEF_FUNC|DEF_FUNC_BARE|DEF_FUNC_LOCAL)\b', code) \
               and section != 'text':
                bad.append((path, n,
                            "function defined while section .%s is current" % section,
                            "add `section .text` before it"))
    return bad

CALLEE_SAVED = ('rbx', 'r12', 'r13', 'r14', 'r15')

def check_callee_saved(files):
    """A return path that does not restore every callee-saved register.

    The SysV ABI makes rbx and r12-r15 the caller's to keep, and the whole
    interpreter relies on it: main holds argc and argv in r14 and r15 across
    the compile, and the eval loop keeps the frame, the stack top and the
    consts pointer there.  A compiler function that pushes r14 and returns
    down a path that forgets to pop it hands main a different argv, and the
    crash lands in sys.argv construction with nothing pointing back here.

    The check is exact rather than statistical: every function in this tree
    opens with a run of pushes and closes each return with the mirroring run
    of pops, so the pops directly above a `ret` must reverse the entry pushes.
    A local subroutine inside a function -- one that adjusts rsp and returns
    without touching the saved registers -- is not a function return and is
    skipped.
    """
    bad = []
    for path in files:
        src = open(path).read()
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL|_BARE)?)\s+(\w+)(?:\s*,[^\n]*)?$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, body = m.group(2), m.group(3)
            code = [l.split(';')[0].strip() for l in body.splitlines()]
            code = [c for c in code if c and not c.endswith(':')]
            entry = []
            for c in code:
                pm = re.match(r'push\s+(%s)$' % '|'.join(CALLEE_SAVED), c)
                if pm:
                    entry.append(pm.group(1))
                elif c.startswith('push '):
                    continue                    # a scratch push, not a save
                else:
                    break
            if not entry:
                continue
            if re.search(r'^\s*call\s+\.', body, re.M):
                continue                        # has local subroutines: their
                                                # `ret`s are not function returns
            for i, c in enumerate(code):
                if not re.match(r'ret\b', c):
                    continue
                got = []
                j = i - 1
                while j >= 0:
                    prev = code[j]
                    om = re.match(r'pop\s+(%s)$' % '|'.join(CALLEE_SAVED), prev)
                    if om:
                        got.append(om.group(1))
                    elif prev == 'leave' or re.match(r'(mov|add|sub|xor|lea|test|cmp|movzx|movsxd|or|and)\b', prev):
                        pass                    # the result being set up, or the frame torn down
                    else:
                        break
                    j -= 1
                if got != entry:
                    bad.append((path, 0,
                                "%s returns without restoring %s"
                                % (name, ", ".join(r for r in entry if r not in got) or "them in order"),
                                "entry pushes %s; this return pops %s"
                                % (" ".join(entry), " ".join(reversed(got)) or "nothing")))
                    break
    return bad

def check_saved_writes(files):
    """A function that writes a callee-saved register it never pushed.

    The mirror image of the check above: not a missing pop but a missing save.
    It costs the caller the same register either way.
    """
    bad = []
    for path in files:
        src = open(path).read()
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL|_BARE)?)\s+(\w+)(?:\s*,[^\n]*)?$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, body = m.group(2), m.group(3)
            saved = set(re.findall(r'^\s*push\s+(%s)\s*$' % '|'.join(CALLEE_SAVED),
                                   body, re.M))
            for wm in re.finditer(
                    r'^\s*(?:mov|lea|add|sub|xor|or|and|inc|dec|movzx|movsxd|imul|shl|shr|pop|not|neg)'
                    r'\s+(%s)\s*(?:,|$)' % '|'.join(CALLEE_SAVED), body, re.M):
                reg = wm.group(1)
                if reg not in saved:
                    line = body[:wm.start()].count('\n')
                    bad.append((path, 0,
                                "%s writes %s without saving it" % (name, reg),
                                wm.group(0).strip()))
                    break
    return bad

def check_alignment(files):
    # Opcode handlers are reached by `jmp` from the dispatcher, not by `call`,
    # so they are entered 16-byte ALIGNED and want the OPPOSITE parity from an
    # ordinary function.  check_handler_alignment below is what judges them;
    # applying the ordinary rule here as well would demand both at once.
    handlers = set(re.findall(r'^\s*dq\s+(op_\w+)', open('src/eval.asm').read(),
                              re.M))
    bad = []
    for path in files:
        src = open(path).read()
        # Every plain `NAME equ <arithmetic>` in the file, so a prologue's own
        # `sub rsp, SOME_SIZE` can be evaluated below.
        consts = {}
        for cm in re.finditer(r'^(\w+)\s+equ\s+(.+?)\s*(?:;.*)?$', src, re.M):
            try:
                consts[cm.group(1)] = eval(cm.group(2), {"__builtins__": {}},
                                           dict(consts))
            except Exception:
                pass
        # `.` matches a newline under re.S, so a trailing `;.*` comment used to
        # swallow the rest of the file: a DEF_FUNC whose declaration carried a
        # comment took everything to the last END_FUNC as its body, and 87 of
        # 290 functions were never examined at all.  The other checks in this
        # file already use [^\n]* for the same reason.
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL)?)\s+(\w+)(?:\s*,\s*([^\s;]+))?[^\n]*$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, frame, body = m.group(2), m.group(3), m.group(4)
            if name in handlers:
                continue        # judged by check_handler_alignment instead
            if '%' in m.group(0).split('\n')[0]:
                continue        # inside a %macro: the name is a parameter, and
                                # its prologue may be conditional
            if not re.search(r'^\s*call\s', body, re.M):
                continue
            n = 0
            if frame:
                if frame.isdigit():
                    n = int(frame)
                else:
                    e = re.search(r'^%s\s+equ\s+(.+?)\s*(?:;.*)?$' % re.escape(frame), src, re.M)
                    if not e:
                        continue                    # defined elsewhere; skip
                    expr = e.group(1)
                    try:
                        n = eval(expr, {"__builtins__": {}}, {})   # plain arithmetic only
                    except Exception:
                        continue                    # symbolic (struct sizes); skip
            # Count pushes before the first non-push instruction.  A function
            # whose pushes are on a branch rather than in the prologue can say
            # so on its declaration; arena_alloc is the only one that needs to.
            ann = re.search(r';[^\n]*\blint:\s*pushes=(\d+)', m.group(0).split('\n')[0])
            if ann:
                p = int(ann.group(1))
            else:
                # Pushes and a `sub rsp` may come in either order -- a few
                # prologues carve their slots first -- so both are counted
                # until the first instruction that is neither.
                p = 0
                for line in body.strip().splitlines():
                    s = line.split(';')[0].strip()
                    if not s or s.startswith('%'):
                        continue        # a preprocessor directive, not code
                    if s.startswith('push '):
                        p += 1
                    elif re.match(r'^sub\s+rsp\s*,', s):
                        continue        # measured as `extra` below
                    else:
                        break
            # A prologue that carves its own space after the pushes counts
            # too, and a few do: pyc_read_file reserves a struct stat that way
            # and addresses it relative to rbp, so its DEF_FUNC frame is 0 and
            # its rsp is nonetheless aligned.  Growing the DEF_FUNC frame of
            # such a function is not the fix -- it moves rsp without moving
            # the rbp-relative buffer, which then overlaps the saved
            # registers.
            extra = 0
            for line in body.strip().splitlines():
                s = line.split(';')[0].strip()
                if not s or s.startswith('%'):
                    continue
                if s.startswith('push '):
                    continue        # already counted above
                sub = re.match(r'^sub\s+rsp\s*,\s*(.+)$', s)
                if not sub:
                    break
                try:
                    extra += eval(sub.group(1), {"__builtins__": {}},
                                  dict(consts))
                except Exception:
                    extra = None
                    break
            if extra is None:
                continue                # symbolic; not ours to judge
            if (n + 8 * p + extra) % 16:
                bad.append((path, 0,
                            "rsp misaligned at calls in %s (frame %d + %d pushes%s)"
                            % (name, n, p, " + sub rsp, %d" % extra if extra else ""),
                            "make the frame %d bytes"
                            % (n + (16 - (n + 8 * p + extra) % 16))))
    return bad

REGS = frozenset(
    'rax rbx rcx rdx rsi rdi rbp rsp r8 r9 r10 r11 r12 r13 r14 r15 '
    'eax ebx ecx edx esi edi ebp esp r8d r9d r10d r11d r12d r13d r14d r15d '
    'ax bx cx dx si di al bl cl dl sil dil r8b r9b r10b r11b r12b r13b r14b r15b'
    .split())

def check_rel(files):
    """Every reference to a global symbol must be rip-relative.

    There is no `default rel`, so a bare `[symbol]` assembles as an absolute
    32-bit displacement.  It happens to link under -no-pie and is still wrong.
    The tree has thousands of `[rel ...]` and no absolute references; this
    keeps it that way.
    """
    bad = []
    pat = re.compile(r'\[\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\]')
    # A usage string may well contain "[option]"; strings are not operands.
    lit = re.compile(r'"[^"]*"|\'[^\']*\'|`[^`]*`')
    for path in files:
        for n, line in enumerate(open(path), 1):
            code = lit.sub('', line).split(';')[0]
            if 'rel ' in code or code.lstrip().startswith('%'):
                continue
            for m in pat.finditer(code):
                sym = m.group(1)
                if sym in REGS or '.' in sym or sym.isdigit():
                    continue
                bad.append((path, n, "bare [%s] is an absolute reference" % sym,
                            "write [rel %s]" % sym))
    return bad

def check_exports(files):
    """A `global X` with no definition of X in the same file.

    NASM does not complain: it emits nothing at all for the symbol, so the
    export silently does not exist and every reader is told a name is available
    that is not.  These arise when a data label is deleted and its `global` is
    left behind -- four dunder-name strings in src/dunder.asm went that way.
    """
    bad = []
    for path in files:
        src = open(path).read()
        exported = re.findall(r'^\s*global\s+(\w+)\s*$', src, re.M)
        if not exported:
            continue
        defined = set(re.findall(r'^(\w+):', src, re.M))
        defined |= set(re.findall(r'^DEF_FUNC(?:_BARE|_LOCAL)?\s+(\w+)', src, re.M))
        defined |= set(re.findall(r'^(\w+)\s+equ\s', src, re.M))
        for g in sorted(set(exported) - defined):
            bad.append((path, 0, "global %s, but %s is never defined here" % (g, g),
                        "define it, or drop the export"))
    return bad

def check_frame_offsets(files):
    """A raw `[rbp - 8]` where a named frame constant belongs.

    A hand-picked offset silently overlaps the slot above it the first time a
    struct in the same frame grows, and the symptom is one field reading as
    garbage.  `rsp`-relative scratch is conventionally written raw and is not
    checked; it is `rbp` frames that must be named.
    """
    bad = []
    lit = re.compile(r'"[^"]*"|\'[^\']*\'|`[^`]*`')
    pat = re.compile(r'\[\s*rbp\s*[-+]\s*\d+\s*\]')
    for path in files:
        for n, line in enumerate(open(path), 1):
            code = lit.sub('', line).split(';')[0]
            m = pat.search(code)
            if m:
                bad.append((path, n, "raw frame offset %s" % m.group(0),
                            "name it with an equ constant"))
    return bad

# Files whose contents a generator writes.  CLAUDE.md's size cap is about what
# a person has to read and edit, so these are exempt -- gen_unicodename.py's
# output is one table and nobody navigates it by hand.
GENERATED = {
    'src/compiler/tables.asm',
    'src/compiler/unicodename.asm',
    'src/compiler/unicodecase.asm',
    'src/compiler/prule.asm',
    'src/dtoa_tables.asm',
}

SIZE_CAP = 100 * 1024


def check_file_size(files):
    """No hand-written .asm over CLAUDE.md's cap.

    class.asm reached 116k holding the metatype, the instance, the bound
    method and the builtin-subclass constructors, and the cost was not
    aesthetic: nothing in it could be found without grep, and the seams
    between those four were invisible until someone went looking for them.
    """
    bad = []
    for path in files:
        if path in GENERATED:
            continue
        n = os.path.getsize(path)
        if n > SIZE_CAP:
            bad.append((path, 0,
                        "%d bytes, over the %dk cap for a hand-written file"
                        % (n, SIZE_CAP // 1024),
                        "split it along a seam it already has"))
    return bad


ALIGN_FLOOR = os.path.join(ROOT, 'tests', 'align_floor.txt')
DOCBLOCK_FLOOR = 'tests/docblock_floor.txt'


def docblock_debt(path):
    """(functions with no docblock, docblocks with no `->` signature line).

    A docblock is the heavy separator block immediately above the DEF_FUNC,
    reached past whatever frame-layout `equ` constants sit between the two --
    STYLE.md puts them there on purpose, so they must not break the
    association.
    """
    lines = open(path).read().split('\n')
    nodoc = nosig = 0
    for i, L in enumerate(lines):
        if not re.match(r'^DEF_FUNC(?:_LOCAL|_BARE)?\s+\w+', L):
            continue
        j = i - 1
        # Walk back over the frame-layout constants STYLE.md puts between the
        # docblock and the DEF_FUNC, and over a single-semicolon comment on
        # one of them -- an `equ` that needs explaining is still part of the
        # layout block, not a docblock of its own.
        while j >= 0 and (re.match(r'^\w+\s+equ\s', lines[j])
                          or not lines[j].strip()
                          or re.match(r'^\s*;[^;]', lines[j])
                          or lines[j].strip() == ';'
                          or re.match(r'^\s*(?:extern|global|align)\s', lines[j])):
            j -= 1
        if j < 0 or not lines[j].startswith(';;'):
            nodoc += 1
            continue
        k = j
        while k >= 0 and lines[k].startswith(';;'):
            k -= 1
        if '->' not in '\n'.join(lines[k + 1:j + 1]):
            nosig += 1
    return nodoc, nosig


def check_docblocks(files):
    """A ratchet, not a rule: no file may lose ground against the floor.

    The signature line is the only part of a function's contract that nothing
    else checks -- a wrong register in one is invisible until someone writes a
    caller from it -- so its absence is a real gap rather than a cosmetic one.
    Writing one means reading what the function actually returns, which is why
    this is a floor being paid down rather than an error from the start.  Lower
    it with `python3 src/compiler/lint.py --record-docblocks` in the commit
    that earns it.
    """
    floor = {}
    try:
        for line in open(DOCBLOCK_FLOOR):
            line = line.split('#')[0].strip()
            if line:
                p, n = line.rsplit(None, 1)
                floor[p] = int(n)
    except FileNotFoundError:
        return []
    bad = []
    for path in files:
        if path in GENERATED:
            continue
        n = sum(docblock_debt(path))
        want = floor.get(path, 0)
        if n > want:
            bad.append((path, 0,
                        "%d function(s) with no docblock or no `->` signature,"
                        " floor is %d" % (n, want),
                        "write the missing ones, or lower the floor knowingly"))
    return bad


def record_docblocks(files):
    """Rewrite the floor file from what the tree is today."""
    rows = [(p, sum(docblock_debt(p))) for p in files if p not in GENERATED]
    rows = [(p, n) for p, n in rows if n]
    with open(DOCBLOCK_FLOOR, 'w') as fh:
        fh.write("# Functions with no docblock, or a docblock with no `->`\n"
                 "# signature line, per file.  A ratchet: lint fails when a\n"
                 "# file goes above its number.  Lower one with\n"
                 "#   python3 src/compiler/lint.py --record-docblocks\n"
                 "# in the commit that earns it; a row at zero is dropped.\n")
        for path, n in rows:
            fh.write("%-38s %d\n" % (path, n))
    print("docblock floor: %d file(s), %d function(s)"
          % (len(rows), sum(n for _, n in rows)))


def check_separators(files):
    """The heavy separator is `;; ` plus 76 `=`, exactly 79 columns.

    A single-semicolon rule reads as an ordinary inline comment, which makes it
    harder to see where one function's documentation ends and the next begins.
    The lighter `;; ---` divider some files use to label a section is a
    different thing and is left alone.
    """
    RULE = ';; ' + '=' * 76
    bad = []
    for path in files:
        for n, line in enumerate(open(path), 1):
            ln = line.rstrip('\n')
            if re.match(r'^; [-=]{20,}\s*$', ln):
                bad.append((path, n, "separator rule with a single `;`",
                            "use `;;`, and `=` for the heavy form"))
            elif re.match(r'^;; ={20,}\s*$', ln) and ln != RULE:
                bad.append((path, n, "heavy separator is %d columns" % len(ln),
                            "`;; ` and 76 `=`, 79 columns"))
    return bad

def check_markers(files):
    """Every function opens with DEF_FUNC* and closes with a matching END_FUNC.

    A `global f` plus a bare `f:` assembles and runs, but emits no ELF size, so
    GDB cannot find the function's boundaries -- and it blinds four of the
    checks here to the rest of the file, because they scan between the two
    markers.  Comparing the two symbol sets catches both halves of that.
    """
    bad = []
    for path in files:
        src = open(path).read()
        opened = re.findall(r'^DEF_FUNC(?:_BARE|_LOCAL)?\s+(\w+)', src, re.M)
        closed = re.findall(r'^END_FUNC\s+(\w+)', src, re.M)
        for name in sorted(set(closed) - set(opened)):
            bad.append((path, 0, "END_FUNC %s with no DEF_FUNC" % name,
                        "open it with DEF_FUNC_BARE, not `global` + a bare label"))
        for name in sorted(set(opened) - set(closed)):
            bad.append((path, 0, "DEF_FUNC %s with no END_FUNC" % name,
                        "add END_FUNC %s" % name))
    return bad

def check_text(files):
    """Two formatting rules that are free to keep and awkward to restore.

    Tabs make the column-32 comment convention unreproducible, and an indented
    DEF_FUNC/END_FUNC hides the function from every regex here.
    """
    bad = []
    for path in files:
        for n, line in enumerate(open(path), 1):
            if '\t' in line:
                bad.append((path, n, "tab character", "use spaces"))
            if re.match(r'\s+(DEF_FUNC|END_FUNC)\b', line):
                bad.append((path, n, "indented function marker",
                            "DEF_FUNC and END_FUNC sit at column 0"))
    return bad

def check_guards(paths):
    """Each .inc has an include guard named for the file, echoed on the %endif.

    Without one a second include redefines every struc, and NASM's error names
    the field rather than the file.
    """
    bad = []
    for path in paths:
        src = open(path).read()
        want = os.path.basename(path).replace('.', '_').upper()
        if not re.search(r'^%ifndef\s+' + want + r'\s*$', src, re.M):
            bad.append((path, 0, "no include guard named " + want,
                        "%ifndef {0} / %define {0}".format(want)))
        elif not re.search(r'^%endif\s*;\s*' + want + r'\s*$', src, re.M):
            bad.append((path, 0, "%endif does not echo " + want,
                        "write `%endif ; " + want + "`"))
    return bad

def type_field_count(headers):
    """How many qwords a PyTypeObject is, from the struct declaration."""
    for path in headers:
        m = re.search(r'struc\s+PyTypeObject(.*?)endstruc', open(path).read(), re.S)
        if m:
            return len(re.findall(r'^\s*\.\w+:\s*resq\s+1', m.group(1), re.M))
    return None

def check_type_tables(files, nfields):
    """A static type table must be the whole PyTypeObject.

    NASM has nothing to say about a table that stops early: the fields past
    the end read whatever the next object in the section happens to be.  Both
    tables that were short here used `times N dq 0` for their tail and were
    left behind when a field was added -- super_type read its tp_flags,
    tp_traverse and tp_dictoffset out of the type object that followed it.

    A structseq type carries one extra qword, its descriptor, one past the
    end; those are the only tables allowed to be longer.
    """
    if not nfields:
        return []
    dq = re.compile(r'^\s*dq\s')
    times = re.compile(r'^\s*times\s+(\d+)\s+dq\s')
    skip = re.compile(r'^\s*(?:extern|global)\s')
    label = re.compile(r'^([A-Za-z_][A-Za-z0-9_]*(?:_type|_metatype)):\s*(?:;.*)?$')
    bad = []
    for path in files:
        lines = open(path).read().split('\n')
        i = 0
        while i < len(lines):
            m = label.match(lines[i])
            if m:
                j, n, last = i + 1, 0, None
                while j < len(lines):
                    L = lines[j]
                    t = times.match(L)
                    if t:
                        n += int(t.group(1)); last = j; j += 1
                    elif dq.match(L):
                        n += 1; last = j; j += 1
                    elif skip.match(L):
                        j += 1
                    elif n and (L.strip().startswith(';') or not L.strip()):
                        k = j
                        while k < len(lines) and (lines[k].strip().startswith(';')
                                                  or not lines[k].strip()):
                            k += 1
                        if k < len(lines) and (dq.match(lines[k]) or times.match(lines[k])
                                               or skip.match(lines[k])):
                            j = k
                        else:
                            break
                    else:
                        break
                if n >= 20:
                    extra = 1 if 'STRUCTSEQ_DESC' in lines[last] else 0
                    if n != nfields + extra:
                        bad.append((path, i + 1,
                                    "type table %s has %d qwords, PyTypeObject is %d"
                                    % (m.group(1), n, nfields + extra),
                                    "a short table reads the next object's fields"))
            i += 1
    return bad

def all_asm():
    """Every hand-written .asm in the tree."""
    return sorted(glob.glob('src/*.asm') + glob.glob('src/*/*.asm'))


def _file_consts(src):
    """Every plain `NAME equ <arithmetic>` in a file, evaluated in order.

    A prologue that carves `sub rsp, CFX_FRAME2 - 8` is as much a stack
    adjustment as `sub rsp, 128`, and reading only the literal form was one of
    the two holes that let a misaligned handler through.
    """
    consts = {}
    for cm in re.finditer(r'^(\w+)\s+equ\s+(.+?)\s*(?:;.*)?$', src, re.M):
        try:
            consts[cm.group(1)] = eval(cm.group(2), {"__builtins__": {}},
                                       dict(consts))
        except Exception:
            pass
    return consts


def _rsp_delta(line, consts):
    """(+bytes, ok) for one instruction's effect on rsp."""
    if line.startswith('push '):
        return 8, True
    if line.startswith('pop '):
        return -8, True
    mv = re.match(r'^(sub|add)\s+rsp\s*,\s*(.+)$', line)
    if not mv:
        return 0, True
    try:
        v = eval(mv.group(2), {"__builtins__": {}}, dict(consts))
    except Exception:
        return 0, False             # symbolic: the depth is no longer known
    return (v if mv.group(1) == 'sub' else -v), True


def _handler_walk(path, name, body, base, consts):
    """Depth-track a handler's body, resolving labels by their arrivals.

    Giving up permanently at the first label -- which is what this used to do
    -- left every call after it unchecked, and that is where the misaligned
    ones were: op_import_name's `call import_module` sits under `.have_name`,
    six instructions past the push that unbalanced it.

    So a label's depth is taken from the depths control can reach it AT: the
    fall-through, plus every jump that names it.  When they all agree the walk
    resumes there; when they disagree, or when any of them is unknown, that
    label stays unknown and so does what follows it.  Two passes, because a
    forward jump is only measured on the pass that reaches it.
    """
    lines = [l.split(';')[0].strip() for l in body.splitlines()]
    lines = [l for l in lines if l and not l.startswith('%')]

    label_depths = {}
    for _ in range(3):
        arrivals = {}
        depth, known = base, True
        for line in lines:
            lm = re.match(r'^(\.?\w+):$', line)
            if lm:
                lab = lm.group(1)
                if known:
                    arrivals.setdefault(lab, set()).add(depth)
                if lab in label_depths:
                    depth, known = label_depths[lab], True
                else:
                    known = False
                continue
            jm = re.match(r'^j\w+\s+(\.?\w+)\s*$', line)
            if jm and known:
                arrivals.setdefault(jm.group(1), set()).add(depth)
            if re.match(r'^(jmp|ret)\b', line):
                known = False       # the fall-through is unreachable
                continue
            d, ok = _rsp_delta(line, consts)
            if not ok:
                known = False
            depth += d
        settled = {k: next(iter(v)) for k, v in arrivals.items() if len(v) == 1}
        if settled == label_depths:
            break
        label_depths = settled

    bad = []
    depth, known = base, True
    for line in lines:
        lm = re.match(r'^(\.?\w+):$', line)
        if lm:
            lab = lm.group(1)
            if lab in label_depths:
                depth, known = label_depths[lab], True
            else:
                known = False
            continue
        if re.match(r'^call\s', line) and known and depth % 16:
            bad.append(("%s %s %s" % (path, name, line),
                        "rsp misaligned at `%s` in handler %s (%d bytes below entry)"
                        % (line, name, depth)))
        if re.match(r'^(jmp|ret)\b', line):
            known = False
            continue
        d, ok = _rsp_delta(line, consts)
        if not ok:
            known = False
        depth += d
    return bad


def check_handler_alignment(files):
    """rsp alignment at `call` inside an opcode handler.

    A handler is reached by `jmp` from the dispatcher, not by `call`, so there
    is no return address and rsp is 16-byte ALIGNED on entry -- the opposite of
    an ordinary function.  A call inside one therefore needs an EVEN number of
    8-byte slots pushed, where an ordinary DEF_FUNC_BARE function needs an odd
    one.  check_alignment above cannot see this: it counts only prologue
    pushes, and a handler's pushes are usually mid-body around the call.

    Fourteen calls across twelve handlers were misaligned when this was
    written, some of them under comments asserting the opposite.  glibc's
    allocator and strtod do use aligned SSE, and DECREF reaches free().

    Both forms are checked, and they want OPPOSITE parities.  A DEF_FUNC
    handler's own `push rbp` is an odd slot, so it needs FRAME + 8*pushes to be
    8 mod 16 where a DEF_FUNC_BARE one needs 0.  Checking only the BARE form
    was a blind spot that left 25 handlers -- op_call, op_load_attr,
    op_store_attr, op_build_string and the rest -- calling glibc misaligned
    under comments computing the ordinary-function rule.

    Which functions are handlers is read from the dispatch table in eval.asm
    rather than guessed from the name, because plenty of DEF_FUNC_BARE
    functions beginning with op_ are called normally.

    The walk is linear and stops trusting its count at the first label, since
    a label may be reached at more than one depth.  That leaves some calls
    unchecked rather than reporting them wrongly.
    """
    ev = open('src/eval.asm').read()
    handlers = set(re.findall(r'^\s*dq\s+(op_\w+)', ev, re.M))
    bad = []
    for path in files:
        src = open(path).read()
        consts = _file_consts(src)
        for m in re.finditer(
                r'^(DEF_FUNC_BARE|DEF_FUNC)[ \t]+(\w+)[ \t]*(?:,[ \t]*(\w+))?'
                r'[^\n]*$(.*?)^END_FUNC', src, re.M | re.S):
            kind, name, frame, body = m.groups()
            if name not in handlers:
                continue
            # DEF_FUNC's own prologue is the odd slot: `push rbp` puts rsp 8
            # past aligned before the body starts, and `sub rsp, FRAME` moves
            # it again.  So a DEF_FUNC handler needs FRAME + 8*pushes to be 8
            # mod 16, where a DEF_FUNC_BARE one needs 0 -- the opposite parity,
            # and the reason this loop cannot just look at the pushes.
            base = 0
            if kind == 'DEF_FUNC':
                base = 8
                if frame:
                    if frame.isdigit():
                        base += int(frame)
                    elif frame in consts:
                        base += consts[frame]
                    else:
                        continue       # a frame size we cannot resolve
            bad += _handler_walk(path, name, body, base, consts)

    # A RATCHET, not a rule.  Forty-four calls were already misaligned when
    # this check learned to see past a label, in twenty-two handlers, and they
    # cannot all be repaired in one commit -- each is a hand edit that has to
    # find the push it belongs to.  What matters is that the number can only
    # fall: a new one fails the build, and the ones already here are listed by
    # site so paying one down is a line deleted from the floor.
    #
    # They are not cosmetic.  A misaligned call propagates: every frame the
    # callee runs inherits it, so one bad handler misaligns the whole nested
    # interpreter stack under it.  The fault surfaces far away and only when
    # something reaches an aligned SSE store -- libz's inflate does, which is
    # how this was found at all.
    floor = set()
    try:
        for line in open(ALIGN_FLOOR):
            line = line.split('#')[0].strip()
            if line:
                floor.add(line)
    except FileNotFoundError:
        pass
    if '--record-alignment' in sys.argv:
        with open(ALIGN_FLOOR, 'w') as fh:
            fh.write("# Calls made with rsp misaligned inside an opcode handler,\n"
                     "# one per line as `file handler instruction`.  A ratchet:\n"
                     "# lint fails on any site not listed here, so the set can\n"
                     "# only shrink.  Re-record with\n"
                     "#   python3 src/compiler/lint.py --record-alignment\n"
                     "# in the commit that pays some of it down.\n")
            for key, _msg in sorted(bad):
                fh.write(key + "\n")
        return []
    out = []
    for key, msg in bad:
        if key not in floor:
            out.append((key.split()[0], 0, msg,
                        "a handler is entered ALIGNED, so DEF_FUNC wants "
                        "FRAME + 8*pushes == 8 (mod 16) and DEF_FUNC_BARE "
                        "wants 0; pad the odd push"))
    return out


def main():
    os.chdir(ROOT)
    if '--record-docblocks' in sys.argv:
        record_docblocks(all_asm())
        return 0
    # Some checks are scoped to src/compiler plus src/main.asm: main holds argc
    # and argv across compile_source, and DEF_FUNC main + 5 pushes enters
    # glibc's strtod misaligned on any source file with a float literal.  The
    # rest of src/ predates the alignment rule and would drown the signal.
    # See STYLE.md.
    scoped = sorted(glob.glob('src/compiler/*.asm')) + ['src/main.asm']

    # The other two are clean across the whole tree, so they run over the whole
    # tree: there is no debt to pay down first, and the only cost of scoping
    # them narrowly was that they missed things.
    everything = all_asm()

    # Every header that declares a struct, not just the two the compiler uses.
    # sre.inc and eventloop.inc were missing, which is why the 8-byte read of
    # SRE_PatternObject.flags in sre_pattern.asm went unseen.
    fields = dword_fields(sorted(glob.glob('src/include/*.inc'))
                          + ['src/compiler/compiler.inc'])

    headers = sorted(glob.glob('src/include/*.inc')) + ['src/compiler/compiler.inc']
    nfields = type_field_count(headers)

    problems = (check_field_widths(everything, fields) + check_section(everything)
                + check_rel(everything) + check_markers(everything)
                + check_exports(everything)
                + check_frame_offsets(everything)
                + check_separators(everything) + check_file_size(everything) + check_docblocks(everything)
                + check_text(everything) + check_guards(headers)
                + check_type_tables(everything, nfields)
                + check_alignment(everything)
                + check_handler_alignment(everything)
                + check_tailjumps(scoped)
                + check_callee_saved(scoped) + check_saved_writes(scoped))
    for path, n, what, detail in problems:
        where = "%s:%d" % (path, n) if n else path
        print("%s: %s\n    %s" % (where, what, detail))
    if problems:
        print("\n%d problem(s)" % len(problems))
        return 1
    print("lint: ok (%d files tree-wide, %d scoped, %d headers, %d dword fields,"
          " %d-qword type tables)"
          % (len(everything), len(scoped), len(headers), len(fields), nfields))
    return 0

if __name__ == '__main__':
    sys.exit(main())
