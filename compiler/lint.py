#!/usr/bin/env python3
"""Static checks over compiler/*.asm for two bug classes that are invisible
at assembly time and expensive to find at runtime.

Both of these actually bit during development, which is why they are checked:

  1. Reading a 4-byte struct field with a 64-bit `mov`.  NASM assembles it
     happily; it silently picks up the next field as the high half.  A
     `mov rdx, [rsi + Token.len]` read the length OR'd with the column, and
     turned into a multi-gigabyte memcpy.

  2. A call made with rsp not 16-byte aligned.  The SysV ABI requires it and
     glibc's floating-point paths (strtod, which the number scanner uses) do
     use aligned SSE stores.  After DEF_FUNC's `push rbp`, `sub rsp, N` and P
     register pushes, alignment holds when (N + 8*P) is a multiple of 16.

Run standalone, or as part of `make check`.
"""
import re, sys, glob, os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
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
    """
    bad = []
    for path in files:
        src = open(path).read()
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL)?)\s+(\w+)(?:\s*,[^\n]*)?$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, body = m.group(2), m.group(3)
            for jm in re.finditer(r'^\s*jmp\s+([a-z_][a-z0-9_]*)\s*(?:;.*)?$', body, re.M):
                target = jm.group(1)
                # A jump to a global function, not a local label or a register.
                if re.search(r'^(DEF_FUNC(_BARE|_LOCAL)?)\s+%s\b' % re.escape(target),
                             "\n".join(open(p).read() for p in files), re.M) \
                   or target in ('buf_grow','buf_push_u32','buf_push_ptr','str_new_heap',
                                 'cg_has_star','asm_loc_varint','asm_effect_var'):
                    bad.append((path, 0,
                                "%s tail-jumps to %s but pushed rbp" % (name, target),
                                "use DEF_FUNC_BARE for a tail-jump"))
    return bad

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
        # `.` matches a newline under re.S, so a trailing `;.*` comment used to
        # swallow the rest of the file: a DEF_FUNC whose declaration carried a
        # comment took everything to the last END_FUNC as its body, and 87 of
        # 290 functions were never examined at all.  The other checks in this
        # file already use [^\n]* for the same reason.
        for m in re.finditer(r'^(DEF_FUNC(?:_LOCAL)?)\s+(\w+)(?:\s*,\s*([^\s;]+))?[^\n]*$(.*?)^END_FUNC',
                             src, re.M | re.S):
            name, frame, body = m.group(2), m.group(3), m.group(4)
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
                p = 0
                for line in body.strip().splitlines():
                    s = line.split(';')[0].strip()
                    if not s or s.startswith('%'):
                        continue        # a preprocessor directive, not code
                    if s.startswith('push '):
                        p += 1
                    else:
                        break
            if (n + 8 * p) % 16:
                bad.append((path, 0,
                            "rsp misaligned at calls in %s (frame %d + %d pushes)" % (name, n, p),
                            "make the frame %d bytes" % (n + (16 - (n + 8 * p) % 16))))
    return bad

def main():
    os.chdir(ROOT)
    # compiler/ plus the one file in src/ that reaches the compiler: main holds
    # argc and argv across compile_source, and DEF_FUNC main + 5 pushes enters
    # glibc's strtod misaligned on any source file with a float literal.  The
    # rest of src/ predates the alignment rule and would drown the signal.
    files = sorted(glob.glob('compiler/*.asm')) + ['src/main.asm']
    fields = dword_fields(['compiler/compiler.inc', 'include/object.inc',
                           'include/frame.inc', 'include/types.inc'])
    problems = (check_field_widths(files, fields) + check_alignment(files)
                + check_tailjumps(files) + check_section(files)
                + check_callee_saved(files) + check_saved_writes(files))
    for path, n, what, detail in problems:
        where = "%s:%d" % (path, n) if n else path
        print("%s: %s\n    %s" % (where, what, detail))
    if problems:
        print("\n%d problem(s)" % len(problems))
        return 1
    print("compiler lint: ok (%d files, %d dword fields checked)" % (len(files), len(fields)))
    return 0

if __name__ == '__main__':
    sys.exit(main())
