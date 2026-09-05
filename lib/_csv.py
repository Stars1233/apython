"""_csv - the CSV reader and writer, in Python.

CPython's is C, and csv.py is written against it: Dialect does the
validation, reader is a state machine over the characters of each line,
and writer joins the fields back with the quoting the dialect asks for.
Nothing here needs to be assembly -- the work is entirely string
handling, which is why this is a lib/ module rather than a src/ one.

The state machine is CPython's own, name for name: a field ends at a
delimiter or at the end of the line, a quoted field ends at a quote that
is not doubled, and an escapechar takes the next character literally
wherever it appears.
"""

__version__ = "1.0"

QUOTE_MINIMAL = 0
QUOTE_ALL = 1
QUOTE_NONNUMERIC = 2
QUOTE_NONE = 3
QUOTE_STRINGS = 4
QUOTE_NOTNULL = 5

_QUOTE_STYLES = (QUOTE_MINIMAL, QUOTE_ALL, QUOTE_NONNUMERIC, QUOTE_NONE,
                 QUOTE_STRINGS, QUOTE_NOTNULL)

_field_limit = 131072


class Error(Exception):
    pass


def field_size_limit(new_limit=None):
    """The longest field a reader will accept, and the previous value."""
    global _field_limit
    old = _field_limit
    if new_limit is not None:
        if not isinstance(new_limit, int):
            raise TypeError("limit must be an integer")
        _field_limit = new_limit
    return old


def _char_arg(value, name, allow_none):
    if value is None:
        if allow_none:
            return None
        raise TypeError('"%s" must be string, not None' % name)
    if not isinstance(value, str):
        raise TypeError('"%s" must be string, not %s'
                        % (name, type(value).__name__))
    if len(value) != 1:
        raise TypeError('"%s" must be a 1-character string' % name)
    return value


class Dialect:
    """The parameters a reader or writer works from, validated once.

    An instance is built from a dialect argument and any keyword
    overrides; every attribute is checked here so that neither the
    reader nor the writer has to check anything.
    """

    _name = ""
    _valid = False
    delimiter = ","
    quotechar = '"'
    escapechar = None
    doublequote = True
    skipinitialspace = False
    lineterminator = "\r\n"
    quoting = QUOTE_MINIMAL

    def __init__(self):
        if self.__class__ is Dialect:
            raise TypeError("can't instantiate Dialect directly")
        self._valid = True
        self._validate()

    def _validate(self):
        try:
            _Dialect(self)
        except TypeError as exc:
            raise Error(str(exc)) from None


class _Dialect:
    """The validated, flattened form the reader and writer actually use."""

    def __init__(self, dialect):
        self.delimiter = _char_arg(getattr(dialect, "delimiter", ","),
                                   "delimiter", False)
        quoting = getattr(dialect, "quoting", QUOTE_MINIMAL)
        # quotechar=None with nothing said about quoting means QUOTE_NONE,
        # which is what CPython's dialect_init does with the pair.
        if getattr(dialect, "quotechar", '"') is None and \
                not _said_quoting(dialect):
            quoting = QUOTE_NONE
        if quoting not in _QUOTE_STYLES:
            raise TypeError("bad 'quoting' value")
        self.quoting = quoting
        quotechar = getattr(dialect, "quotechar", '"')
        self.quotechar = _char_arg(quotechar, "quotechar",
                                   quoting == QUOTE_NONE)
        if self.quotechar is None and quoting != QUOTE_NONE:
            raise TypeError("quotechar must be set if quoting enabled")
        self.escapechar = _char_arg(getattr(dialect, "escapechar", None),
                                    "escapechar", True)
        self.doublequote = bool(getattr(dialect, "doublequote", True))
        self.skipinitialspace = bool(getattr(dialect, "skipinitialspace",
                                             False))
        lineterminator = getattr(dialect, "lineterminator", "\r\n")
        if lineterminator is None:
            raise TypeError('"lineterminator" must be a string')
        if not isinstance(lineterminator, str):
            raise TypeError('"lineterminator" must be a string')
        self.lineterminator = lineterminator
        self.strict = bool(getattr(dialect, "strict", False))


_dialects = {}


def register_dialect(name, dialect=None, **kwargs):
    if not isinstance(name, str):
        raise TypeError("dialect name must be a string")
    _dialects[name] = _build_dialect(dialect, kwargs)


def unregister_dialect(name):
    if name not in _dialects:
        raise Error("unknown dialect")
    del _dialects[name]


def get_dialect(name):
    try:
        return _dialects[name]
    except KeyError:
        raise Error("unknown dialect") from None


def list_dialects():
    return list(_dialects)


class _Default:
    """What "no dialect argument at all" is.

    It has to be distinguishable from an explicit "excel": CPython passes
    the dialect through only when one was given, and quotechar=None means
    QUOTE_NONE exactly when nothing else named the quoting.  Passing
    "excel" by hand therefore RAISES where passing nothing does not.
    """


def _said_quoting(dialect):
    """Whether the dialect names `quoting` at all, override or attribute.

    quotechar=None on its own means QUOTE_NONE -- CPython's dialect_init
    reads the pair together -- so the question is whether anything said
    otherwise.  _Default says nothing; it is what "no dialect at all" is.
    """
    if isinstance(dialect, _Overrides):
        return "quoting" in dialect._kwargs or _said_quoting(dialect._base)
    if dialect is _Default:
        return False
    return getattr(dialect, "quoting", None) is not None


class _ExcelLike:
    """The "excel" dialect before csv.py registers one of its own."""
    delimiter = ","
    quotechar = '"'
    doublequote = True
    skipinitialspace = False
    lineterminator = "\r\n"
    quoting = QUOTE_MINIMAL


class _Overrides:
    """A dialect and its keyword overrides, seen as one object."""

    def __init__(self, base, kwargs):
        self._base = base
        self._kwargs = kwargs

    def __getattr__(self, name):
        # A None override is passed through: _Dialect decides which of them
        # may be None, and quotechar=None is meaningful rather than wrong.
        if name in self._kwargs:
            return self._kwargs[name]
        return getattr(self._base, name)


def _build_dialect(dialect, kwargs):
    if dialect is None or dialect is _Default:
        dialect = _Default
    elif isinstance(dialect, str):
        # "excel" is the default name and needs no registration until csv.py
        # puts it there: CPython ships it as a dialect object.
        if dialect == "excel" and "excel" not in _dialects:
            dialect = _ExcelLike
        else:
            dialect = get_dialect(dialect)
    if kwargs:
        dialect = _Overrides(dialect, kwargs)
    return _Dialect(dialect)


class _Reader:
    def __init__(self, source, dialect):
        self._source = iter(source)
        self.dialect = dialect
        self.line_num = 0

    def __iter__(self):
        return self

    def __next__(self):
        d = self.dialect
        fields = []
        field = []
        quoted = False
        in_quotes = False
        after_quote = False
        started = False
        while True:
            line = next(self._source)
            self.line_num += 1
            if not isinstance(line, str):
                raise Error("iterator should return strings, not %s "
                            "(the file should be opened in text mode)"
                            % type(line).__name__)
            if "\0" in line:
                raise Error("line contains NUL")
            i = 0
            n = len(line)
            while i < n:
                c = line[i]
                i += 1
                if in_quotes:
                    if after_quote:
                        after_quote = False
                        if c == d.quotechar and d.doublequote:
                            field.append(c)
                            continue
                        in_quotes = False
                        if c == d.delimiter:
                            fields.append(_convert(d, "".join(field), quoted))
                            field = []
                            quoted = False
                            started = False
                            continue
                        if c in "\r\n":
                            i = n
                            break
                        if d.strict:
                            raise Error("',' expected after '\"'")
                        field.append(c)
                        continue
                    if c == d.escapechar:
                        if i < n:
                            field.append(line[i])
                            i += 1
                        continue
                    if c == d.quotechar:
                        after_quote = True
                        continue
                    field.append(c)
                    continue
                if c in "\r\n":
                    i = n
                    break
                if c == d.escapechar:
                    if i < n:
                        field.append(line[i])
                        i += 1
                    started = True
                    continue
                if c == d.delimiter:
                    fields.append(_convert(d, "".join(field), quoted))
                    field = []
                    quoted = False
                    started = False
                    continue
                if (c == d.quotechar and d.quoting != QUOTE_NONE
                        and not started):
                    in_quotes = True
                    quoted = True
                    started = True
                    continue
                if c == " " and d.skipinitialspace and not started:
                    continue
                started = True
                field.append(c)
            if in_quotes and not after_quote:
                # An embedded newline: the field continues on the next line,
                # and the line's own terminator is already in it.
                if len(field) > _field_limit:
                    raise Error("field larger than field limit (%d)"
                                % _field_limit)
                continue
            if after_quote:
                in_quotes = False
                after_quote = False
            break
        if field or fields or started or quoted:
            fields.append(_convert(d, "".join(field), quoted))
        for f in fields:
            if isinstance(f, str) and len(f) > _field_limit:
                raise Error("field larger than field limit (%d)"
                            % _field_limit)
        return fields


def _convert(dialect, text, quoted):
    if dialect.quoting == QUOTE_NONNUMERIC and not quoted:
        try:
            return float(text)
        except ValueError:
            raise Error("could not convert string to float: %r"
                        % (text,)) from None
    return text


def reader(csvfile, dialect=_Default, **kwargs):
    return _Reader(csvfile, _build_dialect(dialect, kwargs))


class _Writer:
    def __init__(self, target, dialect):
        self._write = target.write
        self.dialect = dialect

    def writerow(self, row):
        d = self.dialect
        try:
            iter(row)
        except TypeError:
            raise Error("iterable expected, not %s" % type(row).__name__)
        out = []
        row = list(row)
        for field in row:
            out.append(self._field(field, len(row)))
        return self._write(d.delimiter.join(out) + d.lineterminator)

    def writerows(self, rows):
        for row in rows:
            self.writerow(row)

    def _field(self, field, nfields):
        d = self.dialect
        quoted = d.quoting == QUOTE_ALL
        if field is None:
            text = ""
            if d.quoting == QUOTE_NOTNULL:
                quoted = True
        elif isinstance(field, str):
            text = field
            if d.quoting in (QUOTE_NONNUMERIC, QUOTE_STRINGS, QUOTE_NOTNULL):
                quoted = True
        else:
            text = str(field)
            if d.quoting == QUOTE_NOTNULL:
                quoted = True

        # CPython decides per character, and the two outcomes are not the
        # same: a delimiter or a newline QUOTES the field, while a quotechar
        # with doublequote off, or the escapechar itself, is ESCAPED and the
        # field stays bare.
        escape = [False] * len(text)
        for i, ch in enumerate(text):
            if (ch == d.delimiter or ch == d.escapechar
                    or (d.quotechar is not None and ch == d.quotechar)
                    or ch in d.lineterminator):
                if d.quoting == QUOTE_NONE:
                    escape[i] = True
                elif d.quotechar is not None and ch == d.quotechar:
                    if d.doublequote:
                        quoted = True
                    else:
                        escape[i] = True
                elif ch == d.escapechar:
                    escape[i] = True
                else:
                    quoted = True
                if escape[i] and d.escapechar is None:
                    raise Error("need to escape, but no escapechar set")

        # An empty field alone on a line would read back as no fields at
        # all, so it is quoted.
        if not text and nfields == 1 and d.quoting != QUOTE_NONE:
            quoted = True

        out = []
        for i, ch in enumerate(text):
            if escape[i]:
                out.append(d.escapechar)
            elif quoted and d.quotechar is not None and ch == d.quotechar:
                out.append(d.quotechar)
            out.append(ch)
        body = "".join(out)
        if quoted and d.quotechar is not None:
            return d.quotechar + body + d.quotechar
        return body


def writer(csvfile, dialect=_Default, **kwargs):
    if not hasattr(csvfile, "write"):
        raise TypeError("argument 1 must have a \"write\" method")
    return _Writer(csvfile, _build_dialect(dialect, kwargs))
