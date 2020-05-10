"""Tests for the js-regex library."""
# -*- coding: UTF-8 -*-

from __future__ import unicode_literals

import re
import string
from sys import version_info as python_version

import pytest
from hypothesis import event, example, given, note, settings, strategies as st
from py_mini_racer import py_mini_racer

import js_regex
from js_regex._impl import _prepare_and_parse

PY2 = python_version.major == 2
SKIP_ON_PY2 = pytest.mark.skipif(PY2, reason="Not supported on Python 2")
SKIP_BEFORE_36 = pytest.mark.skipif(python_version < (3, 6), reason="also old")


@pytest.fixture()
def v8context():
    return py_mini_racer.MiniRacer()


@pytest.fixture()
def randexp_ctx(v8context):
    with open("src/randexp.min.js") as f:
        randexp_src = f.read()
    # randexp.min.js needs this to be able to export itself
    v8context.eval("const window = new Object()")
    v8context.eval(randexp_src)
    v8context.eval("const RandExp = window.RandExp")
    return v8context


@pytest.mark.parametrize(
    "pattern,good_match,bad_match",
    [
        ("^abc", "abc", ""),
        ("^abc", "abcd", "not abc"),
        ("^abc$", "abc", "abc\n"),
        ("^abc$|^def$", "abc", "abc\n"),
        ("^abc$|^def$", "def", "def\n"),
        (r"^abc\$", "abc$", "abc"),
        (r"\a", "\a", r"\a"),
        (r"\cA", "\x01", r"\cA"),
        (r"\ca", "\x01", r"\ca"),
    ],
)
def test_expected_transforms(pattern, good_match, bad_match):
    regex = js_regex.compile(pattern)
    assert regex.search(good_match)
    assert not regex.search(bad_match)


@pytest.mark.parametrize(
    "pattern,good_match,bad_match",
    [
        pytest.param(r"\d", "1", "߀", marks=SKIP_ON_PY2),  # NKO DIGIT ZERO
        (r"\D", "߀", "1"),
        pytest.param(r"\w", "a", "é", marks=SKIP_ON_PY2),  # Latin-1 e-acute
        (r"\W", "é", "a"),
        pytest.param(r"\s", "\t", "\x1f", marks=SKIP_ON_PY2),  # non-breaking space
        (r"\S", "\x1f", "\t"),
    ],
)
def test_charclass_transforms(pattern, good_match, bad_match):
    regex = js_regex.compile(pattern)
    assert regex.search(good_match)
    assert not regex.search(bad_match)
    if ord(bad_match) >= 128:
        # Non-ascii string is matched by Python 3, but not in JS mode
        assert re.compile(pattern).search(bad_match)


@pytest.mark.parametrize(
    "pattern,string",
    [
        ("a(?=b)", "ab"),
        ("a(?=b)", "ac"),
        ("(?<=a)b", "ab"),
        ("(?<=a)b", "ac"),
        ("a(?!b)", "ab"),
        ("a(?!b)", "ac"),
        ("(?<!a)b", "ab"),
        ("(?<!a)b", "ac"),
        ("a?", "abc"),
        ("a*", "abc"),
        ("a+", "abc"),
        ("a+?", "abc"),
        ("a{1,2}", "abc"),
        ("a?", "def"),
        ("a*", "def"),
        ("a+", "def"),
        ("a+?", "def"),
        ("a{1,2}", "def"),
    ],
)
def test_consistent_behaviour_is_consistent(pattern, string):
    # The main point of this test is to excercise the recursion in check_features
    convert = bool if PY2 else repr
    assert convert(re.search(pattern, string)) == convert(
        js_regex.compile(pattern).search(string)
    )


@pytest.mark.parametrize(
    "pattern,error",
    [
        (1, TypeError),
        (r"(abc(", re.error),
        (r"\A", js_regex.NotJavascriptRegex),
        (r"\Z", js_regex.NotJavascriptRegex),
        (r"(?#comment)", js_regex.NotJavascriptRegex),
        (r"(?#a different comment)", js_regex.NotJavascriptRegex),
        pytest.param(r"(?i:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?-i:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?m:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?-m:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?s:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?-s:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?x:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?-x:regex)", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        (r"(abc)(?(1)then|else)", js_regex.NotJavascriptRegex),
        # Defining a named capture group is checked separately to these named
        # references; it's therefore a Python-level error or a redundant test.
        (r"(?(name)then|else)", re.error),
        (r"(?P<name>regex)(?(name)then|else)", js_regex.NotJavascriptRegex),
        # Test that check_features recurses through all the things it should
        (r"a(?=\Z)", js_regex.NotJavascriptRegex),
        (r"(?<=\A)b", js_regex.NotJavascriptRegex),
        (r"a(?!\Z)", js_regex.NotJavascriptRegex),
        (r"(?<!\A)b", js_regex.NotJavascriptRegex),
        pytest.param(r"a|(?i:b)|c", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?i:regex)?", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(r"(?i:regex)+", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36),
        pytest.param(
            r"(?i:regex)+?", js_regex.NotJavascriptRegex, marks=SKIP_BEFORE_36
        ),
    ],
)
def test_pattern_validation(pattern, error):
    with pytest.raises(error):
        js_regex.compile(pattern)


@pytest.mark.parametrize(
    "flags,error",
    [
        ("flags", TypeError),
        (re.LOCALE, js_regex.NotJavascriptRegex),
        (re.TEMPLATE, js_regex.NotJavascriptRegex),
        (re.VERBOSE, js_regex.NotJavascriptRegex),
    ],
)
def test_flags_validation(flags, error):
    with pytest.raises(error):
        js_regex.compile("", flags=flags)


def regex_patterns():
    """Generates JS-valid regex patterns - a subset of all valid JS regex syntax,
    but a superset of that recommended for JSON-schema:
    https://json-schema.org/understanding-json-schema/reference/regular_expressions.html

    For example, it doesn't generate inverted metachars in charsets because
    converting [^\\W] ('not not word') is not currently supported.
    """
    char = st.sampled_from(
        ["."]
        + [re.escape(c) for c in string.printable]
        + [r"\w", r"\s", r"\d", r"\W", r"\D", r"\S"]
    )
    setchars = list(string.printable) + [r"\w", r"\s", r"\d", r"\W", r"\D", r"\S"]
    for c in r"\-[]":
        setchars.remove(c)
        setchars.append("\\" + c)
    sets = st.builds(
        str.format,
        st.sampled_from(["[{}]", "[^{}]"]),
        st.lists(st.sampled_from(setchars), min_size=1, unique=True).map("".join),
    ).filter(lambda x: x != "[^]")
    special = st.sampled_from([r"\c" + l for l in string.ascii_letters])
    groups = [
        "(%s)",
        r"(?=%s)",
        r"(?!%s)",
    ]  # [r"(?<=%s)", r"(?<!%s)"]  not supported by py_mini_racer V8
    repeaters = ["%s?", "%s*", "%s+", "%s??", "%s*?", "%s+?"]
    small = st.integers(0, 9).map(str)
    num_repeat = st.one_of(
        small,
        small.map(lambda s: s + ","),
        st.tuples(small, small).map(sorted).map(",".join),
    ).map(lambda s: r"%s{" + s + r"}")
    repeat_chars = tuple("?*+}")

    def repeater(rgx, rpt):
        if rgx.endswith(repeat_chars) and rpt.endswith(repeat_chars):
            rgx = "(" + rgx + ")"
        return rpt % (rgx,)

    regex = st.deferred(
        lambda: char
        | sets
        | special
        | st.builds(repeater, char | sets | special, st.sampled_from(groups))
        | st.builds(repeater, regex, st.sampled_from(["(%s)"] + repeaters) | num_repeat)
        | st.lists(regex, min_size=2, unique=True).map("|".join)
    )
    return st.builds(str.format, st.sampled_from(["{}", "^{}", "{}$", "^{}$"]), regex)


@settings(deadline=None, max_examples=1000)
@given(regex_patterns())
@example(pattern="\\\t")  # means: [\ or \t]
@example(pattern="[\\n]")  # means: [\ or n]
@example(pattern="/")
@example(pattern="[\n]")
def test_translates_any_pattern(randexp_ctx, pattern):
    jr = js_regex.compile(pattern)
    parsed = _prepare_and_parse(pattern, flags=0)
    note(
        "pattern: {pattern!r} {pattern_len}".format(
            pattern=pattern, pattern_len=len(pattern)
        )
    )
    note("parsed: {parsed}".format(parsed=parsed))
    note("js-regex: {jr_pattern!r}".format(jr_pattern=jr.pattern))

    for target, replacement in (
        ("\\", "\\\\"),
        ("\f", "\\f"),
        ("\n", "\\n"),
        ("\r", "\\r"),
        ("\t", "\\t"),
        ("\b", "\\b"),
        ('"', '\\"'),
        ("'", "\\'"),
    ):
        pattern = pattern.replace(target, replacement)

    randexp_ctx.eval('var pattern = "%s"' % pattern)
    pattern_len = randexp_ctx.eval("pattern.length")
    note("js-pattern-len: {pattern_len}".format(pattern_len=pattern_len))

    randexp_ctx.eval("var regexp = new RegExp(pattern)")
    randexp_ctx.eval("var randexp = new RandExp(regexp)")

    for _ in range(5):
        randexp_ctx.eval("var val = randexp.gen()")
        match = randexp_ctx.eval("regexp.test(val)")
        value = randexp_ctx.eval("val")
        value_len = randexp_ctx.eval("val.length")
        note(
            "randexp: {value!r} {value_len} -> {match!r}".format(
                value=value, value_len=value_len, match=match
            )
        )
        if match:
            assert jr.search(value)
        else:
            # randexp.js failed to generate a valid example
            # e.g. https://github.com/fent/randexp.js/issues/104
            event(
                "randexp.js failed to generate a valid example for: {pattern!r}".format(
                    pattern=pattern
                )
            )
