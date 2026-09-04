"""The bridge from apython's parse tree to the _ast node classes.

`compile(src, name, mode, ast.PyCF_ONLY_AST)` walks the parser's arena in
assembly -- src/compiler/astraw.asm -- and hands back a tree of plain tuples.
This turns those into node objects.

A raw node is a ten-tuple:

    (kind, subkind, lineno, col, end_lineno, end_col, a, b, c, children)

where a, b and c are a nested raw node, a Python object, an int or None
according to the kind, and `children` is a list, a single nested node, or
None.  A position of -1 means the parser did not record it, which is None
here; ast.py documents end_lineno and end_col_offset as optional.

The two trees disagree in ways that are all list surgery, and this is where
that happens:

  * an else, elif or finally tail is a BLOCK node here and a bare statement
    list in CPython;
  * decorators hang off a DECORATED wrapper rather than on the def;
  * every parameter is in one flat list with the counts in an EXTRA node,
    where CPython has seven parallel lists;
  * a class's bases and keywords are one CALL node;
  * `**x` is its own kind rather than a keyword with no name;
  * async def, async for, async with and except* are a subkind bit rather
    than four more node types;
  * and the operators and contexts are subkind bytes rather than twenty-nine
    singleton classes.
"""

import _ast

# --- the kinds, mirroring src/compiler/compiler.inc ------------------------
(CONST, NAME, BINOP, UNARYOP, BOOLOP, COMPARE, IFEXP, LAMBDA, TUPLE, LIST,
 SET, DICT, CALL, ATTRIBUTE, SUBSCRIPT, SLICE, STARRED, DOUBLESTARRED,
 KEYWORD, NAMEDEXPR, YIELD, YIELDFROM, AWAIT, JOINEDSTR, FORMATTEDVALUE,
 LISTCOMP, SETCOMP, DICTCOMP, GENEXP, COMPREHENSION) = range(1, 31)

MODULE, EXPRESSION, EXPR_STMT, ASSIGN, AUGASSIGN, ANNASSIGN = range(40, 46)
IF, WHILE, FOR, BLOCK, PASS, BREAK, CONTINUE, RETURN, DELETE = range(46, 55)
RAISE, ASSERT, GLOBAL, NONLOCAL, IMPORT, IMPORTFROM, ALIAS = range(55, 62)
FUNCTIONDEF, CLASSDEF, TRY, HANDLER, WITH, WITHITEM = range(62, 68)
ARGUMENTS, ARG, MATCH, EXTRA, DECORATED = range(68, 73)
(CASE, PAT_VALUE, PAT_CAPTURE, PAT_SEQUENCE, PAT_MAPPING, PAT_CLASS,
 PAT_KEYWORD, PAT_OR, PAT_AS) = range(73, 82)

# Raw tuple slots.
K, SUB, LINE, COL, ELINE, ECOL, A, B, C, CH = range(10)

# --- subkind tables --------------------------------------------------------
# One instance each, not one per node: CPython's parser shares them, so every
# Load in a tree is the same object and `node.ctx is ast.Load()` is False for
# a fresh one but True between two nodes.
CTX = (_ast.Load(), _ast.Store(), _ast.Del())
UNARY = (_ast.UAdd(), _ast.USub(), _ast.Invert(), _ast.Not())
BOOL = (_ast.And(), _ast.Or())
CMP = (_ast.Lt(), _ast.LtE(), _ast.Eq(), _ast.NotEq(), _ast.Gt(), _ast.GtE(),
       _ast.In(), _ast.NotIn(), _ast.Is(), _ast.IsNot())

# BINARY_OP's own argument numbering, from src/include/opcodes.inc.  The
# in-place codes are the same operators thirteen higher, which is what
# augmented assignment stores.
BINOPS = (_ast.Add(), _ast.BitAnd(), _ast.FloorDiv(), _ast.LShift(),
          _ast.MatMult(), _ast.Mult(), _ast.Mod(), _ast.BitOr(), _ast.Pow(),
          _ast.RShift(), _ast.Sub(), _ast.Div(), _ast.BitXor())


# A node whose source starts with one of its children rather than with the
# token that named it.  The parser stamps each node with the token it was
# looking at when the node was made, which for an infix or postfix form is
# the operator: `a + 1` gave the BinOp the column of the `+`, where CPython
# gives it the column of `a`.  The value is which raw slot the leftmost
# component is in.
# The expression parser stamps every infix node with the token its production
# began at, so BinOp, Call, Attribute and the rest arrive correct -- including
# the parentheses around a left operand, which is where CPython's position for
# them is.  What is left here are the STATEMENT forms, built by a parser that
# has only the statement's own keyword position to hand.
LEFT_START = {
    AUGASSIGN: A, ANNASSIGN: A, WITHITEM: A,
    ASSIGN: CH,                 # starts with its first target
}


def _start(raw):
    """Where a node's source really starts."""
    slot = LEFT_START.get(raw[K])
    if slot is not None:
        child = raw[slot]
        if slot is CH:
            child = child[0] if child else None
        if child is not None and isinstance(child, tuple):
            return _start(child)
    return raw[LINE], raw[COL]


def _pos(raw):
    """The four position attributes, as CPython's constructors take them."""
    line, col = _start(raw)
    end_line = raw[ELINE]
    end_col = raw[ECOL]
    return {
        "lineno": line,
        "col_offset": col if col >= 0 else 0,
        "end_lineno": None if end_line < 0 else end_line,
        "end_col_offset": None if end_col < 0 else end_col,
    }


def _stmts(raw):
    """A statement list from a BLOCK node, a list, or nothing."""
    if raw is None:
        return []
    if isinstance(raw, list):
        return [_node(x) for x in raw]
    if raw[K] == BLOCK:
        return [_node(x) for x in raw[CH]]
    return [_node(raw)]


def _each(items):
    return [_node(x) for x in items] if items else []


def _opt(raw):
    return _node(raw) if raw is not None else None


# --- one builder per kind --------------------------------------------------
# Each takes the raw tuple and the position kwargs and returns a node.

def _b_const(r, p):
    return _ast.Constant(r[A], None, **p)


def _b_name(r, p):
    return _ast.Name(r[A], CTX[r[SUB]], **p)


def _b_binop(r, p):
    return _ast.BinOp(_node(r[A]), BINOPS[r[SUB]], _node(r[B]), **p)


def _b_unaryop(r, p):
    return _ast.UnaryOp(UNARY[r[SUB]], _node(r[A]), **p)


def _b_boolop(r, p):
    return _ast.BoolOp(BOOL[r[SUB]], _each(r[CH]), **p)


def _b_compare(r, p):
    ops, operands = [], []
    for i in range(0, len(r[CH]), 2):
        ops.append(CMP[r[CH][i]])
        operands.append(_node(r[CH][i + 1]))
    return _ast.Compare(_node(r[A]), ops, operands, **p)


def _b_ifexp(r, p):
    return _ast.IfExp(_node(r[A]), _node(r[B]), _node(r[C]), **p)


def _b_lambda(r, p):
    return _ast.Lambda(_arguments(r[B]), _node(r[C]), **p)


def _b_tuple(r, p):
    return _ast.Tuple(_each(r[CH]), CTX[r[SUB]], **p)


def _b_list(r, p):
    return _ast.List(_each(r[CH]), CTX[r[SUB]], **p)


def _b_set(r, p):
    return _ast.Set(_each(r[CH]), **p)


def _b_dict(r, p):
    keys, values = [], []
    for i in range(0, len(r[CH]), 2):
        key, value = r[CH][i], r[CH][i + 1]
        if key is not None and key[K] == DOUBLESTARRED:
            # `{**x}`: CPython spells the key None and puts x in the value.
            keys.append(None)
            values.append(_node(key[A]))
        else:
            keys.append(_opt(key))
            values.append(_node(value))
    return _ast.Dict(keys, values, **p)


def _b_call(r, p):
    # Positional arguments and keywords share one child list here; CPython
    # keeps them apart, and `**x` is a keyword with no name rather than a
    # node kind of its own.
    args, keywords = [], []
    for child in r[CH] or []:
        if child[K] == DOUBLESTARRED:
            keywords.append(_ast.keyword(None, _node(child[A]), **_pos(child)))
        elif child[K] == KEYWORD:
            keywords.append(_b_keyword(child, _pos(child)))
        else:
            args.append(_node(child))
    keywords.extend(_kwlist(r[B]))
    return _ast.Call(_node(r[A]), args, keywords, **p)


def _kwlist(raw):
    """A call's keyword arguments, which hang off .b as their own list."""
    if raw is None:
        return []
    items = raw[CH] if raw[K] == BLOCK else [raw]
    out = []
    for kw in items:
        if kw[K] == DOUBLESTARRED:
            out.append(_ast.keyword(None, _node(kw[A]), **_pos(kw)))
        else:
            out.append(_ast.keyword(kw[A], _node(kw[B]), **_pos(kw)))
    return out


def _b_attribute(r, p):
    return _ast.Attribute(_node(r[A]), r[B], CTX[r[SUB]], **p)


def _b_subscript(r, p):
    return _ast.Subscript(_node(r[A]), _node(r[B]), CTX[r[SUB]], **p)


def _b_slice(r, p):
    return _ast.Slice(_opt(r[A]), _opt(r[B]), _opt(r[C]), **p)


def _b_starred(r, p):
    return _ast.Starred(_node(r[A]), CTX[r[SUB]], **p)


def _b_keyword(r, p):
    return _ast.keyword(r[A] or None, _node(r[B]), **p)


def _b_namedexpr(r, p):
    return _ast.NamedExpr(_node(r[A]), _node(r[B]), **p)


def _b_yield(r, p):
    return _ast.Yield(_opt(r[A]), **p)


def _b_yieldfrom(r, p):
    return _ast.YieldFrom(_node(r[A]), **p)


def _b_await(r, p):
    return _ast.Await(_node(r[A]), **p)


def _b_joinedstr(r, p):
    """Adjacent literal pieces are ONE Constant, as CPython's parser gives it.

    A run of implicitly concatenated f-strings is lexed here one token at a
    time, so `f'a' f'b'` arrives as two literal pieces; CPython's parser
    accumulates the text and emits a single Constant spanning both.  Merging
    them here rather than in the tokenizer keeps the run loop simple, and the
    bytecode is the same either way -- only the tree differs.
    """
    out = []
    for piece in _each(r[CH]):
        if (out and isinstance(piece, _ast.Constant)
                and isinstance(out[-1], _ast.Constant)
                and isinstance(piece.value, str)
                and isinstance(out[-1].value, str)):
            last = out[-1]
            merged = _ast.Constant(
                last.value + piece.value, None,
                lineno=last.lineno, col_offset=last.col_offset,
                end_lineno=piece.end_lineno,
                end_col_offset=piece.end_col_offset)
            out[-1] = merged
        else:
            out.append(piece)
    return _ast.JoinedStr(out, **p)


# The conversion, which apython numbers 0..3 and CPython spells as -1 or the
# character's own ordinal.
CONVERSIONS = (-1, ord("s"), ord("r"), ord("a"))


def _b_formattedvalue(r, p):
    return _ast.FormattedValue(_node(r[A]), CONVERSIONS[r[SUB]], _opt(r[B]),
                               **p)


def _b_listcomp(r, p):
    return _ast.ListComp(_node(r[A]), _each(r[CH]), **p)


def _b_setcomp(r, p):
    return _ast.SetComp(_node(r[A]), _each(r[CH]), **p)


def _b_dictcomp(r, p):
    return _ast.DictComp(_node(r[A]), _node(r[B]), _each(r[CH]), **p)


def _b_genexp(r, p):
    return _ast.GeneratorExp(_node(r[A]), _each(r[CH]), **p)


def _b_comprehension(r, p):
    return _ast.comprehension(_node(r[A]), _node(r[B]), _each(r[CH]),
                              r[SUB])


def _b_module(r, p):
    return _ast.Module(_each(r[CH]), [])


def _b_expression(r, p):
    return _ast.Expression(_node(r[A]))


def _b_expr_stmt(r, p):
    return _ast.Expr(_node(r[A]), **p)


def _b_assign(r, p):
    return _ast.Assign(_each(r[CH]), _node(r[B]), None, **p)


def _b_augassign(r, p):
    op = r[SUB]
    if op >= 13:
        op -= 13                # the in-place codes are the same operators
    return _ast.AugAssign(_node(r[A]), BINOPS[op], _node(r[B]), **p)


def _b_annassign(r, p):
    target = _node(r[A])
    simple = 1 if isinstance(target, _ast.Name) else 0
    return _ast.AnnAssign(target, _node(r[B]), _opt(r[C]), simple, **p)


def _b_if(r, p):
    return _ast.If(_node(r[A]), _stmts(r[C]), _stmts(r[B]), **p)


def _b_while(r, p):
    return _ast.While(_node(r[A]), _stmts(r[C]), _stmts(r[B]), **p)


def _b_for(r, p):
    cls = _ast.AsyncFor if r[SUB] else _ast.For
    return cls(_node(r[A]), _node(r[B]), _stmts(r[C]), _stmts(r[CH]),
               None, **p)


def _b_pass(r, p):
    return _ast.Pass(**p)


def _b_break(r, p):
    return _ast.Break(**p)


def _b_continue(r, p):
    return _ast.Continue(**p)


def _b_return(r, p):
    return _ast.Return(_opt(r[A]), **p)


def _b_delete(r, p):
    return _ast.Delete(_each(r[CH]), **p)


def _b_raise(r, p):
    return _ast.Raise(_opt(r[A]), _opt(r[B]), **p)


def _b_assert(r, p):
    return _ast.Assert(_node(r[A]), _opt(r[B]), **p)


def _b_global(r, p):
    return _ast.Global(list(r[CH] or []), **p)


def _b_nonlocal(r, p):
    return _ast.Nonlocal(list(r[CH] or []), **p)


def _b_import(r, p):
    return _ast.Import(_each(r[CH]), **p)


def _b_importfrom(r, p):
    return _ast.ImportFrom(r[A] or None, _each(r[CH]), r[SUB], **p)


def _b_alias(r, p):
    # `from a import *` is a subkind rather than a name: the parser has
    # nothing to intern for it.
    name = "*" if r[SUB] else r[A]
    return _ast.alias(name, r[B] or None, **p)


def _b_try(r, p):
    cls = _ast.TryStar if r[SUB] else _ast.Try
    return cls(_each(r[CH]), _stmts(r[A]), _stmts(r[B]), _stmts(r[C]), **p)


def _b_handler(r, p):
    return _ast.ExceptHandler(_opt(r[A]), r[B] or None, _each(r[CH]), **p)


def _b_with(r, p):
    cls = _ast.AsyncWith if r[SUB] else _ast.With
    return cls(_each(r[CH]), _stmts(r[A]), None, **p)


def _b_withitem(r, p):
    return _ast.withitem(_node(r[A]), _opt(r[B]))


def _b_arg(r, p):
    return _ast.arg(r[A], _opt(r[B]), None, **p)


def _b_match(r, p):
    return _ast.Match(_node(r[A]), _each(r[CH]), **p)


def _b_case(r, p):
    return _ast.match_case(_node(r[A]), _opt(r[B]), _each(r[CH]))


def _b_pat_value(r, p):
    if r[SUB]:
        return _ast.MatchSingleton(_node(r[A]).value, **p)
    return _ast.MatchValue(_node(r[A]), **p)


def _b_pat_capture(r, p):
    return _ast.MatchAs(None, r[A] or None, **p)


def _b_pat_sequence(r, p):
    items = list(r[CH] or [])
    star = r[B]                 # 1 + the index of the starred element, or 0
    out = []
    for i, child in enumerate(items):
        node = _node(child)
        if star and i == star - 1:
            name = node.name if isinstance(node, _ast.MatchAs) else None
            node = _ast.MatchStar(name, **_pos(child))
        out.append(node)
    return _ast.MatchSequence(out, **p)


def _b_pat_mapping(r, p):
    keys, patterns = [], []
    for i in range(0, len(r[CH] or []), 2):
        keys.append(_node(r[CH][i]))
        patterns.append(_node(r[CH][i + 1]))
    return _ast.MatchMapping(keys, patterns, r[B] or None, **p)


def _b_pat_class(r, p):
    npos = r[B]
    items = list(r[CH] or [])
    patterns = [_node(x) for x in items[:npos]]
    names, kwd = [], []
    for child in items[npos:]:
        names.append(child[A])
        kwd.append(_node(child[B]))
    return _ast.MatchClass(_node(r[A]), patterns, names, kwd, **p)


def _b_pat_or(r, p):
    return _ast.MatchOr(_each(r[CH]), **p)


def _b_pat_as(r, p):
    return _ast.MatchAs(_opt(r[A]), r[B] or None, **p)


def _b_decorated(r, p):
    """The wrapper is not a node of its own: its decorators go on the def."""
    inner = _node(r[A])
    inner.decorator_list = _each(r[CH])
    return inner


def _b_functiondef(r, p):
    cls = _ast.AsyncFunctionDef if r[SUB] else _ast.FunctionDef
    return cls(r[A], _arguments(r[B]), _each(r[CH]), [], _node(r[C]), None, [], **p)


def _b_classdef(r, p):
    bases, keywords = [], []
    if r[B] is not None:
        call = _b_call(r[B], _pos(r[B]))
        bases, keywords = call.args, call.keywords
    return _ast.ClassDef(r[A], bases, keywords, _each(r[CH]), [], [], **p)


def _arguments(raw):
    """CPython's seven parallel lists, out of one flat one and the counts.

    apython keeps every parameter in .clist and hangs an EXTRA node off .a
    carrying how many are positional and how many of those are
    positional-only; *args and **kwargs are .b and .c.  Each parameter keeps
    its own default on .c of its ARG node, where CPython has one right-aligned
    `defaults` list and a None-padded `kw_defaults`.
    """
    if raw is None:
        return _ast.arguments([], [], None, [], [], None, [])
    items = list(raw[CH] or [])
    extra = raw[A]
    npos = extra[A] if extra is not None else len(items)
    posonly = extra[B] if extra is not None else 0

    positional = items[:npos]
    kwonly = items[npos:]

    posonlyargs = [_node(x) for x in positional[:posonly]]
    args = [_node(x) for x in positional[posonly:]]
    kwonlyargs = [_node(x) for x in kwonly]

    # `defaults` covers the tail of posonlyargs + args, so it stops at the
    # first parameter that has one.
    defaults = [_node(x[C]) for x in positional if x[C] is not None]
    kw_defaults = [_opt(x[C]) for x in kwonly]

    vararg = _node(raw[B]) if raw[B] is not None else None
    kwarg = _node(raw[C]) if raw[C] is not None else None
    return _ast.arguments(posonlyargs, args, vararg, kwonlyargs, kw_defaults,
                          kwarg, defaults)


BUILDERS = {
    CONST: _b_const, NAME: _b_name, BINOP: _b_binop, UNARYOP: _b_unaryop,
    BOOLOP: _b_boolop, COMPARE: _b_compare, IFEXP: _b_ifexp,
    LAMBDA: _b_lambda, TUPLE: _b_tuple, LIST: _b_list, SET: _b_set,
    DICT: _b_dict, CALL: _b_call, ATTRIBUTE: _b_attribute,
    SUBSCRIPT: _b_subscript, SLICE: _b_slice, STARRED: _b_starred,
    KEYWORD: _b_keyword, NAMEDEXPR: _b_namedexpr, YIELD: _b_yield,
    YIELDFROM: _b_yieldfrom, AWAIT: _b_await, JOINEDSTR: _b_joinedstr,
    FORMATTEDVALUE: _b_formattedvalue, LISTCOMP: _b_listcomp,
    SETCOMP: _b_setcomp, DICTCOMP: _b_dictcomp, GENEXP: _b_genexp,
    COMPREHENSION: _b_comprehension,
    MODULE: _b_module, EXPRESSION: _b_expression, EXPR_STMT: _b_expr_stmt,
    ASSIGN: _b_assign, AUGASSIGN: _b_augassign, ANNASSIGN: _b_annassign,
    IF: _b_if, WHILE: _b_while, FOR: _b_for, PASS: _b_pass, BREAK: _b_break,
    CONTINUE: _b_continue, RETURN: _b_return, DELETE: _b_delete,
    RAISE: _b_raise, ASSERT: _b_assert, GLOBAL: _b_global,
    NONLOCAL: _b_nonlocal, IMPORT: _b_import, IMPORTFROM: _b_importfrom,
    ALIAS: _b_alias, FUNCTIONDEF: _b_functiondef, CLASSDEF: _b_classdef,
    TRY: _b_try, HANDLER: _b_handler, WITH: _b_with, WITHITEM: _b_withitem,
    ARG: _b_arg, MATCH: _b_match, DECORATED: _b_decorated, CASE: _b_case,
    PAT_VALUE: _b_pat_value, PAT_CAPTURE: _b_pat_capture,
    PAT_SEQUENCE: _b_pat_sequence, PAT_MAPPING: _b_pat_mapping,
    PAT_CLASS: _b_pat_class, PAT_OR: _b_pat_or, PAT_AS: _b_pat_as,
}
# PAT_KEYWORD has no builder of its own: _b_pat_class reads both its fields
# straight, because CPython puts the names and the patterns in two lists on
# MatchClass rather than in a node each.


def _node(raw):
    if raw is None:
        return None
    build = BUILDERS.get(raw[K])
    if build is None:
        raise ValueError("no _ast mapping for parser node kind %d" % raw[K])
    return build(raw, _pos(raw))


def _from_raw(raw):
    """The entry point src/compiler/compile.asm calls."""
    return _node(raw)
