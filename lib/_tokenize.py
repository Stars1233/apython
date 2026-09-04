"""_tokenize - the tokenizer behind tokenize.py.

CPython 3.12 moved tokenize.py onto the C tokenizer, so tokenize is now a
thin wrapper over `_tokenize.TokenizerIter` and cannot load without it.  That
one import is what kept traceback, linecache, logging, hashlib, doctest and
half a dozen others out, none of which ever tokenizes anything -- they import
tokenize for `tokenize.open`, or for something that does.

This is that tokenizer, in Python.  It yields the same 5-tuples the C one
does: (type, string, (srow, scol), (erow, ecol), line).

One deliberate difference: an f-string comes out as a single STRING token,
which is what CPython did through 3.11 and what every consumer here expects.
CPython 3.12 splits it into FSTRING_START / FSTRING_MIDDLE / FSTRING_END so
that the parser can see the expressions inside; nothing in this tree reads
those, and producing them would mean tokenizing the replacement fields too.
"""

ENDMARKER = 0
NAME = 1
NUMBER = 2
STRING = 3
NEWLINE = 4
INDENT = 5
DEDENT = 6
OP = 55
COMMENT = 64
NL = 65
ERRORTOKEN = 66

_OPERATORS = (
    # Longest first: the scanner takes the first that matches.
    '**=', '//=', '>>=', '<<=', '...',
    '**', '//', '>>', '<<', '<=', '>=', '==', '!=', '->',
    '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '@=', ':=',
    '+', '-', '*', '/', '%', '&', '|', '^', '~', '<', '>',
    '(', ')', '[', ']', '{', '}', '@', ',', ':', '.', ';', '=', '!',
)

_OPENERS = '([{'
_CLOSERS = ')]}'

# Every prefix a 3.12 string literal may carry, in either case.
_STRING_PREFIXES = frozenset((
    '', 'b', 'r', 'u', 'f', 'br', 'rb', 'fr', 'rf',
))


def _is_id_start(c):
    return c.isidentifier() or (c == '_')


def _is_id_continue(c):
    return c == '_' or c.isalnum() or ord(c) > 127


class TokenError(Exception):
    pass


class TokenizerIter:
    """The iterator tokenize.py drives.

    `source` is either a readline callable or, when `encoding` is given, a
    bytes-producing one; both are read to exhaustion.  `extra_tokens` asks for
    COMMENT and NL as well, which tokenize.generate_tokens wants and the
    parser does not.
    """

    def __init__(self, source, encoding=None, extra_tokens=False):
        self._encoding = encoding
        self._extra = bool(extra_tokens)
        if callable(source):
            self._readline = source
        else:
            raise TypeError("TokenizerIter() argument must be callable")
        self._tokens = None
        self._index = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self._tokens is None:
            self._tokens = list(self._run())
        if self._index >= len(self._tokens):
            raise StopIteration
        tok = self._tokens[self._index]
        self._index += 1
        return tok

    # -- the scanner -----------------------------------------------------
    def _lines(self):
        while True:
            try:
                line = self._readline()
            except StopIteration:
                return
            if not line:
                return
            if isinstance(line, bytes):
                line = line.decode(self._encoding or 'utf-8')
            yield line

    def _run(self):
        indents = [0]
        parens = 0
        contstr = None          # (text, quote, startpos, startline)
        lnum = 0
        line = ''
        at_line_start = True    # no NEWLINE has been seen for this logical line
        blank_logical = True    # nothing but whitespace/comments so far

        for line in self._lines():
            lnum += 1
            pos = 0
            maxpos = len(line)

            if contstr is not None:
                text, quote, start, startline = contstr
                end = line.find(quote)
                while end >= 0 and _escaped(line, end):
                    end = line.find(quote, end + 1)
                if end < 0:
                    contstr = (text + line, quote, start, startline + line)
                    continue
                end += len(quote)
                text += line[:end]
                yield (STRING, text, start, (lnum, end), startline + line)
                contstr = None
                blank_logical = False
                pos = end
            elif parens == 0 and at_line_start:
                # Indentation, but only for a line that has something on it.
                col = 0
                while pos < maxpos:
                    c = line[pos]
                    if c == ' ':
                        col += 1
                    elif c == '\t':
                        col = (col // 8 + 1) * 8
                    elif c == '\f':
                        col = 0
                    else:
                        break
                    pos += 1
                if pos >= maxpos or line[pos] in '#\r\n':
                    # A blank or comment-only line changes no indentation.
                    if self._extra:
                        if pos < maxpos and line[pos] == '#':
                            comment = line[pos:].rstrip('\r\n')
                            yield (COMMENT, comment, (lnum, pos),
                                   (lnum, pos + len(comment)), line)
                            pos += len(comment)
                        nl = line[pos:]
                        yield (NL, nl, (lnum, pos), (lnum, pos + len(nl)), line)
                    continue
                if col > indents[-1]:
                    indents.append(col)
                    yield (INDENT, line[:pos], (lnum, 0), (lnum, pos), line)
                while col < indents[-1]:
                    if col not in indents:
                        raise IndentationError(
                            "unindent does not match any outer indentation "
                            "level", ('<tokenize>', lnum, pos, line))
                    indents.pop()
                    yield (DEDENT, '', (lnum, pos), (lnum, pos), line)
                at_line_start = False
                blank_logical = False

            while pos < maxpos:
                c = line[pos]

                if c in ' \t\f':
                    pos += 1
                    continue

                if c == '#':
                    comment = line[pos:].rstrip('\r\n')
                    if self._extra:
                        yield (COMMENT, comment, (lnum, pos),
                               (lnum, pos + len(comment)), line)
                    pos += len(comment)
                    continue

                if c == '\\' and pos + 1 < maxpos and line[pos + 1] in '\r\n':
                    # An explicit line join: the next line continues this one.
                    pos = maxpos
                    at_line_start = False
                    break

                if c in '\r\n':
                    nl = line[pos:]
                    if parens > 0 or blank_logical:
                        if self._extra:
                            yield (NL, nl, (lnum, pos),
                                   (lnum, pos + len(nl)), line)
                    else:
                        yield (NEWLINE, nl, (lnum, pos),
                               (lnum, pos + len(nl)), line)
                        at_line_start = True
                        blank_logical = True
                    pos = maxpos
                    break

                # A string, possibly with a prefix, possibly triple-quoted.
                quote_start = _string_prefix_end(line, pos)
                if quote_start is not None:
                    prefix = line[pos:quote_start]
                    quote = _quote_at(line, quote_start)
                    end = _scan_string(line, quote_start + len(quote), quote)
                    if end is None:
                        if len(quote) == 3:
                            contstr = (line[pos:], quote, (lnum, pos), line)
                            pos = maxpos
                            blank_logical = False
                            break
                        raise SyntaxError(
                            "unterminated string literal (detected at line %d)"
                            % (lnum,), ('<tokenize>', lnum, pos + 1, line))
                    yield (STRING, line[pos:end], (lnum, pos), (lnum, end),
                           line)
                    pos = end
                    blank_logical = False
                    continue

                if c.isdigit() or (c == '.' and pos + 1 < maxpos
                                   and line[pos + 1].isdigit()):
                    end = _scan_number(line, pos)
                    yield (NUMBER, line[pos:end], (lnum, pos), (lnum, end),
                           line)
                    pos = end
                    blank_logical = False
                    continue

                if _is_id_start(c) or ord(c) > 127:
                    end = pos + 1
                    while end < maxpos and _is_id_continue(line[end]):
                        end += 1
                    yield (NAME, line[pos:end], (lnum, pos), (lnum, end), line)
                    pos = end
                    blank_logical = False
                    continue

                for op in _OPERATORS:
                    if line.startswith(op, pos):
                        if op in _OPENERS:
                            parens += 1
                        elif op in _CLOSERS:
                            parens -= 1
                            if parens < 0:
                                parens = 0
                        yield (OP, op, (lnum, pos), (lnum, pos + len(op)),
                               line)
                        pos += len(op)
                        break
                else:
                    yield (ERRORTOKEN, c, (lnum, pos), (lnum, pos + 1), line)
                    pos += 1
                blank_logical = False

            if pos >= maxpos and not line.endswith(('\n', '\r')):
                # A last line with no newline still ends the logical one.
                if not blank_logical and parens == 0 and contstr is None:
                    yield (NEWLINE, '', (lnum, maxpos), (lnum, maxpos + 1),
                           line)
                    at_line_start = True
                    blank_logical = True

        if contstr is not None:
            raise SyntaxError(
                "unterminated triple-quoted string literal (detected at line "
                "%d)" % (lnum,), ('<tokenize>', contstr[2][0], 1, ''))

        lnum += 1
        for _ in indents[1:]:
            yield (DEDENT, '', (lnum, 0), (lnum, 0), '')
        yield (ENDMARKER, '', (lnum, 0), (lnum, 0), '')


def _escaped(line, i):
    """Is the character at i preceded by an odd number of backslashes?"""
    n = 0
    j = i - 1
    while j >= 0 and line[j] == '\\':
        n += 1
        j -= 1
    return n % 2 == 1


def _quote_at(line, i):
    if line.startswith('"""', i) or line.startswith("'''", i):
        return line[i:i + 3]
    return line[i]


def _string_prefix_end(line, pos):
    """If a string literal starts at pos, where its quote begins."""
    i = pos
    n = len(line)
    while i < n and i - pos < 2 and line[i].isalpha():
        i += 1
    if i < n and line[i] in '"\'':
        if line[pos:i].lower() in _STRING_PREFIXES:
            return i
    return None


def _scan_string(line, i, quote):
    """The index just past the closing quote, or None if it is not on this
    line."""
    n = len(line)
    while i < n:
        c = line[i]
        if c == '\\':
            i += 2
            continue
        if line.startswith(quote, i):
            return i + len(quote)
        if len(quote) == 1 and c in '\r\n':
            return None
        i += 1
    return None


def _scan_number(line, pos):
    n = len(line)
    i = pos
    if line[i] == '0' and i + 1 < n and line[i + 1] in 'xXoObB':
        i += 2
        while i < n and (line[i].isalnum() or line[i] == '_'):
            i += 1
        return i
    seen_dot = False
    seen_exp = False
    while i < n:
        c = line[i]
        if c.isdigit() or c == '_':
            i += 1
        elif c == '.' and not seen_dot and not seen_exp:
            seen_dot = True
            i += 1
        elif c in 'eE' and not seen_exp and i + 1 < n and (
                line[i + 1].isdigit() or
                (line[i + 1] in '+-' and i + 2 < n and line[i + 2].isdigit())):
            seen_exp = True
            i += 2 if line[i + 1].isdigit() else 2
        elif c in 'jJ':
            i += 1
            break
        else:
            break
    return i
