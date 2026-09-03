"""_string - the two parsers behind str.format and string.Formatter.

CPython puts them in C because str.format uses them on every call; here
str.format is assembly and does not, so these exist for `string`, which
imports them at module level and is what four stdlib modules come in behind.

Both are pure parsers over the format-spec mini-language: no evaluation, no
lookups, just the split.
"""


def formatter_parser(format_string):
    """Split a format string into (literal, field_name, format_spec, conversion).

    A tuple per replacement field, plus a trailing one for text after the last
    field.  A field that is absent -- the trailing literal -- gets None for
    the other three; `{}` gets ''.  Doubled braces are literal and are
    unescaped into the literal part.
    """
    if not isinstance(format_string, str):
        raise TypeError("expected str, got %s" % (type(format_string).__name__,))
    out = []
    literal = []
    i = 0
    n = len(format_string)
    while i < n:
        c = format_string[i]
        if c == '{' or c == '}':
            if i + 1 < n and format_string[i + 1] == c:
                # A doubled brace ENDS the current literal: CPython emits one
                # tuple per escape, so 'a{{b}}c' is three of them and not one
                # with the braces unescaped in place.
                literal.append(c)
                out.append((''.join(literal), None, None, None))
                literal = []
                i += 2
                continue
            if c == '}':
                raise ValueError("Single '}' encountered in format string")
            # A real field.  Scan to its closing brace, counting nested ones,
            # which appear inside a format spec: "{a:{w}}".
            i += 1
            depth = 1
            start = i
            while i < n:
                ch = format_string[i]
                if ch == '{':
                    depth += 1
                elif ch == '}':
                    depth -= 1
                    if depth == 0:
                        break
                i += 1
            if depth != 0:
                raise ValueError("Single '{' encountered in format string")
            body = format_string[start:i]
            i += 1

            # The spec is after the first ':' that is not inside [] -- an
            # index can contain one -- and the conversion after the first '!'
            # before that, unless it is '!=' inside an index.
            spec = None
            conv = None
            depth = 0
            for j, ch in enumerate(body):
                if ch == '[':
                    depth += 1
                elif ch == ']':
                    depth -= 1
                elif depth == 0 and ch == ':':
                    spec = body[j + 1:]
                    body = body[:j]
                    break
            if len(body) >= 2 and body[-2] == '!':
                conv = body[-1]
                body = body[:-2]
            elif body.endswith('!'):
                raise ValueError("unmatched '{' in format spec")
            if spec is None:
                spec = ''
            out.append((''.join(literal), body, spec, conv))
            literal = []
            continue
        literal.append(c)
        i += 1
    if literal:
        out.append((''.join(literal), None, None, None))
    return iter(out)


def formatter_field_name_split(field_name):
    """Split "a.b[0].c" into its first part and an iterator over the rest.

    The first part is the name or the argument index; each element after it is
    (is_attribute, value), with an index left as an int when it is all digits.
    """
    if not isinstance(field_name, str):
        raise TypeError("expected str, got %s" % (type(field_name).__name__,))
    i = 0
    n = len(field_name)
    while i < n and field_name[i] not in '.[':
        i += 1
    first = field_name[:i]
    if first.isdigit():
        first = int(first)

    rest = []
    while i < n:
        c = field_name[i]
        if c == '.':
            i += 1
            start = i
            while i < n and field_name[i] not in '.[':
                i += 1
            name = field_name[start:i]
            if not name:
                raise ValueError("Empty attribute in format string")
            rest.append((True, name))
        elif c == '[':
            i += 1
            start = i
            while i < n and field_name[i] != ']':
                i += 1
            if i == n:
                raise ValueError("Missing ']' in format string")
            key = field_name[start:i]
            i += 1
            if key.isdigit():
                key = int(key)
            rest.append((False, key))
        else:
            raise ValueError("Only '.' or '[' may follow ']' in format string")
    return first, iter(rest)
