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

def main():
    os.chdir(ROOT)
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
                + check_separators(everything) + check_file_size(everything)
                + check_text(everything) + check_guards(headers)
                + check_type_tables(everything, nfields)
                + check_alignment(everything) + check_tailjumps(scoped)
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
