"""The implementation of the js-regex library."""
from __future__ import unicode_literals

import re
import sre_compile
import sre_constants
import sre_parse
from copy import deepcopy
from functools import lru_cache
from sys import version_info as python_version
from typing import Any, Iterable, List, Optional, Pattern, Tuple, Union, cast


class NotJavascriptRegex(ValueError):
    """The pattern uses Python regex features that do not exist in Javascript."""


SubNodeT = Tuple[sre_constants._NamedIntConstant, int]

ParsedCharClassT = List[SubNodeT]

ParsedNodeT = Union[
    ParsedCharClassT,
    Tuple[None, List[sre_parse.SubPattern]],
    Iterable[sre_parse.SubPattern],
    Tuple[int, sre_parse.SubPattern, sre_parse.SubPattern],
    Tuple[Optional[int], int, int, sre_parse.SubPattern],
]

SubPatternT = Union[sre_parse.SubPattern, List[sre_parse.SubPattern], SubNodeT]

ElementT = Tuple[sre_constants._NamedIntConstant, ParsedNodeT]

SubElementT = Union[sre_parse.SubPattern, ElementT]


def ast_sub_in(
    subpattern: SubPatternT, target: SubNodeT, replacements: List[SubNodeT]
) -> None:
    """
    in-place substitution for an IN clause member (i.e. character class)
    """
    for i, el in enumerate(cast(Iterable, subpattern)):
        if isinstance(el, tuple) and el[0] is sre_constants.IN:
            assert isinstance(subpattern, sre_parse.SubPattern)
            in_list = el[1]
            for i, el in enumerate(in_list):
                if el == target:
                    in_list.pop(i)
                    for repl in reversed(replacements):
                        in_list.insert(i, repl)
        elif isinstance(el, (list, tuple, sre_parse.SubPattern)):
            ast_sub_in(cast(SubPatternT, el), target, replacements)


def ast_sub_el(
    subpattern: SubPatternT, target: SubElementT, replacement: SubElementT
) -> None:
    """
    in-place substitution for a single SubPattern member
    """
    for i, el in enumerate(cast(Iterable, subpattern)):
        if el == target:
            cast(sre_parse.SubPattern, subpattern)[i] = cast(ElementT, replacement)
        elif isinstance(el, (list, tuple, sre_parse.SubPattern)):
            ast_sub_el(cast(SubPatternT, el), target, replacement)


def _prepare_and_parse(pattern: str, flags: int = 0) -> sre_parse.SubPattern:
    if not isinstance(pattern, (str, type(""))):
        raise TypeError("pattern={!r} must be a unicode string".format(pattern))
    if not isinstance(flags, int):
        raise TypeError("flags={!r} must be an integer".format(flags))
    # Check that the supplied flags are legal in both Python and JS.  See
    # https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp#Parameters
    # and the list of flags at https://docs.python.org/3/library/re.html#re.compile
    if flags & re.LOCALE:
        raise NotJavascriptRegex("The re.LOCALE flag has no equivalent in Javascript")
    if flags & re.TEMPLATE:
        raise NotJavascriptRegex("The re.TEMPLATE flag has no equivalent in Javascript")
    if flags & re.VERBOSE:
        raise NotJavascriptRegex("The re.VERBOSE flag has no equivalent in Javascript")

    # Replace JS-only control-character escapes \cA - \cZ and \ca - \cz
    # with their corresponding control characters.
    # e.g. /\ca/.test('\u0001') -> true
    pattern = re.sub(
        r"(?<!\\)(\\c[A-Za-z])",
        repl=lambda m: chr(ord(m.group(0)[-1].upper()) - 64),
        string=pattern,
    )

    # check for Python-only constructs *before* we transform further
    try:
        parsed = sre_parse.parse(pattern, flags=flags)
    except re.error as e:
        raise re.error("{} in pattern={!r}".format(e, pattern))
    check_features(parsed, flags=flags, pattern=pattern)

    # Check for comments - with `in` because don't appear in the parse tree.
    if re.search(r"\(\?\#[^)]*\)", pattern):
        raise NotJavascriptRegex(
            "'(?#comment)' groups are ignored by Python, but have no meaning in "
            "Javascript regular expressions (pattern={!r})".format(pattern)
        )

    def ast_charclass_from_str(pattern: str) -> ParsedCharClassT:
        subpattern = sre_parse.parse(pattern, flags=flags)
        assert subpattern[0][0] is sre_constants.IN
        char_class = subpattern[0][1]
        return cast(ParsedCharClassT, char_class)

    # we have to deepcopy(parsed) to avoid a weird interaction with python `re`
    # constants must pass an `is` check in sre_compile (can't replace with int)
    # but are not deepcopiable as defined, so we monkeypatch the cls temporarily
    def _monkeypatch(self, memo):
        # type: (sre_constants._NamedIntConstant, Any) -> sre_constants._NamedIntConstant
        return self

    sre_constants._NamedIntConstant.__deepcopy__ = _monkeypatch  # type: ignore
    parsed = deepcopy(parsed)
    del sre_constants._NamedIntConstant.__deepcopy__  # type: ignore

    # replace character class shortcuts (Unicode in Python) with the
    # corresponding ASCII set like in JS.
    for target, replacements in [
        # \d: [(RANGE, (48, 57))]
        (ast_charclass_from_str(r"\d")[0], ast_charclass_from_str("[0-9]")),
        (
            ast_charclass_from_str(r"\D")[0],
            ast_charclass_from_str("[\x00-\x2f\x3a-\U0010ffff]"),
        ),
        # \w: [(RANGE, (65, 90)), (RANGE, (97, 122)), (RANGE, (48, 57)), (LITERAL, 95)]
        (ast_charclass_from_str(r"\w")[0], ast_charclass_from_str("[A-Za-z0-9_]")),
        (
            ast_charclass_from_str(r"\W")[0],
            ast_charclass_from_str("[\x00-\x2f\x3a-\x40\x5b-\x5e\x60\x7b-\U0010ffff]"),
        ),
        # https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Character_Classes
        # https://www.ecma-international.org/ecma-262/5.1/#sec-7.2
        # https://www.ecma-international.org/ecma-262/5.1/#sec-7.3
        # \s: [(RANGE, (9, 13)), (LITERAL, 32), < ...unicodes >]
        (
            ast_charclass_from_str(r"\s")[0],
            ast_charclass_from_str(
                r"[ \f\n\r\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]"
            ),
        ),
        (
            ast_charclass_from_str(r"\S")[0],
            ast_charclass_from_str(
                "[\x00-\x08\x0e-\x1f\x21-\x9f\xa1-\u167f\u1681-\u1fff\u200b-\u2027\u202a-\u202e\u2030-\u205e\u2060-\u2fff\u3001-\ufefe\uff00-\U0010ffff]"
            ),
        ),
    ]:
        ast_sub_in(parsed, target, replacements)

    # Replace any unescaped $ - which is allowed in both but behaves
    # differently - with the Python-only \Z which behaves like JS' $.
    ast_sub_el(
        parsed,
        sre_parse.parse(r"$", flags=flags)[0],
        sre_parse.parse(r"\Z", flags=flags)[0],
    )
    return parsed


@lru_cache(maxsize=512)  # Matches the internal cache size for re.compile
def compile(pattern: str, flags: int = 0) -> Pattern[str]:
    """Compile the given string, treated as a Javascript regex.

    This aims to match all strings that would be matched in JS, and as few
    additional strings as possible.  Where possible it will also warn if the
    pattern would not be valid in JS.

    This is not a full implementation of EMCA-standard regex, but somewhat
    better than simply ignoring the differences between dialects.
    """
    parsed = _prepare_and_parse(pattern, flags=flags)

    # Finally, we compile our fixed pattern to a Python regex pattern and return it.
    return sre_compile.compile(parsed, flags=flags)


def check_features(parsed: Any, flags: int, pattern: str) -> None:
    """Recursively walk through a SRE regex parse tree to check that every
    node is for a feature that also exists in Javascript regular expressions.

    `parsed` is either a list of SRE regex elements representations or a
    particular element representation. Each element is a tuple of element code
    (as string) and parameters. E.g. regex 'ab[0-9]+' compiles to following
    elements:

        [
            (LITERAL, 97),
            (LITERAL, 98),
            (MAX_REPEAT, (1, 4294967295, [
                (IN, [
                    (RANGE, (48, 57))
                ])
            ]))
        ]

    This function is inspired by https://github.com/HypothesisWorks/hypothesis
    /blob/master/hypothesis-python/src/hypothesis/searchstrategy/regex.py
    """
    if not isinstance(parsed, tuple):
        for elem in parsed:
            assert isinstance(elem, tuple)
            check_features(elem, flags=flags, pattern=pattern)
    else:
        code, value = parsed
        if code == sre_constants.ASSERT or code == sre_constants.ASSERT_NOT:
            # Regexes '(?=...)', '(?<=...)', '(?!...)' or '(?<!...)'
            # (positive/negative lookahead/lookbehind)
            check_features(value[1], flags=flags, pattern=pattern)
        elif code == sre_constants.MIN_REPEAT or code == sre_constants.MAX_REPEAT:
            # Regexes 'a?', 'a*', 'a+', and their non-greedy variants (repeaters)
            check_features(value[2], flags=flags, pattern=pattern)
        elif code == sre_constants.BRANCH:
            # Regex 'a|b|c' (branch)
            for branch in value[1]:
                check_features(branch, flags=flags, pattern=pattern)
        elif code == sre_constants.SUBPATTERN:
            # Various groups: '(...)', '(:...)' or '(?P<name>...)'
            # The parser converts group names to numbers, so the `_` doesn't help here
            check_features(value[-1], flags=flags, pattern=pattern)
            if python_version >= (3, 6) and (value[1] | value[2]):  # pragma: no cover
                raise NotJavascriptRegex(
                    "Javascript regular expressions do not support "
                    "subpattern flags (pattern={pattern!r})"
                )
        elif code == sre_constants.AT:
            # Regexes like '^...', '...$', '\bfoo', '\Bfoo', '\A', '\Z'
            if value == sre_constants.AT_BEGINNING_STRING:
                raise NotJavascriptRegex(
                    r"\A is not valid in Javascript regular expressions - "
                    "use ^ instead (pattern={!r})".format(pattern)
                )
            if value == sre_constants.AT_END_STRING:
                raise NotJavascriptRegex(
                    r"\Z is not valid in Javascript regular expressions - "
                    "use $ instead (pattern={!r})".format(pattern)
                )
        elif code == sre_constants.GROUPREF_EXISTS:
            # Regex '(?(id/name)yes-pattern|no-pattern)' (if group exists choice)
            raise NotJavascriptRegex(
                "Javascript regular expressions do not support if-group-exists choice, "
                "like `'(?(id/name)yes-pattern|no-pattern)'` (pattern={!r})".format(
                    pattern
                )
            )
        else:
            assert code in [
                sre_constants.IN,  # Regex '[abc0-9]' (set of characters)
                sre_constants.ANY,  # Regex '.' (any char)
                sre_constants.LITERAL,  # Regex 'a' (single char)
                sre_constants.NOT_LITERAL,  # Regex '[^a]' (negation of a single char)
                sre_constants.GROUPREF,  # Regex '\\1' or '(?P=name)' (group reference)
            ]
