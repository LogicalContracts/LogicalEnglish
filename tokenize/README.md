# `pack(tokenize) :-`

A modest tokenization library for SWI-Prolog, seeking a balance between
simplicity and flexibility.

[![CircleCI](https://circleci.com/gh/shonfeder/tokenize.svg?style=svg)](https://circleci.com/gh/shonfeder/tokenize)

## Synopsis

```prolog
?- tokenize(`\tExample  Text.`, Tokens).
Tokens = [cntrl('\t'), word(example), space(' '), space(' '), word(text), punct('.')]

?- tokenize(`\tExample  Text.`, Tokens, [cntrl(false), pack(true), cased(true)]).
Tokens = [word('Example', 1), space(' ', 2), word('Text', 1), punct('.', 1)]

?- tokenize(`\tExample  Text.`, Tokens), untokenize(Tokens, Text), format('~s~n', [Text]).
	example  text.
Tokens = [cntrl('\t'), word(example), space(' '), space(' '), word(text), punct('.')],
Text = [9, 101, 120, 97, 109, 112, 108, 101, 32|...]
```

## Description

Module `tokenize` provides a straightforward tool for
[lexing](https://en.wikipedia.org/wiki/Lexical_analysis) text into a list of
tokens. It supports several options to customize the kinds of tokens generated,
but it errs on the side of simplicity over flexibility.

`tokenize` is not a viable alternative to industrial strength lexer generators,
but it is a useful tool if you need to lex some text into common tokens to ease
your parsing needs.

## Installation

`tokenize` is packaged as an [SWI-Prolog pack](http://www.swi-prolog.org/pack/list?p=tokenize).
Install it into your SWI-Prolog system by running the following query in the
`swipl` top level:

```prolog
?- pack_install(tokenize).
```

Then answer `y` at the prompts.

## Usage

Import the library into your source files with the directive

```prolog
:- use_module(library(tokenize)).
```

Please see [the documentation](https://www.swi-prolog.org/pack/file_details/tokenize/prolog/tokenize.pl)
and consult [the wiki](https://github.com/shonfeder/tokenize/wiki/tokenize.pl-options-and-examples)
for more detailed instructions and examples, including a full list of supported options.

## Contributing

See the [CONTRIBUTING.md](https://github.com/shonfeder/tokenize/blob/develop/CONTRIBUTING.md)
document.
