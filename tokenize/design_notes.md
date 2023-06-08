# Design Notes

Initially extracted from conversation with
[@Annieppo](https://github.com/Anniepoo) and [@nicoabie](https://github.com/nicoabie) in
##prolog on [freenode](https://freenode.net/).

The library started as a very simple and lightweight set of predicates for a
common, but very limited, form of lexing. As we extend it, we aim to maintain a
modest scope in order to achieve a sweet spot between ease of use and powerful
flexibility.

## Scope and Aims

`tokenize` does not aspire to become an industrial strength lexer generator. We
aim to serve most users needs between raw input and a structured form ready for
parsing by a DCG.

If a user is parsing a language with keywords such as `class`, `module`, etc.,
and wants to distinguish these from variable names, `tokenize` isn't going to
give you this out of the box. But, it should provide an easy means of achieving
this result through a subsequent lexing pass.

## Some Model Users

* somebody making a computer language
  * needs to be able to distinguish keywords, variables and literals
  * needs to be able to identify comments
* somebody making a parser for an interactive fiction game
  * needs to handle stuff like "William O. N'mutu-O'Connell went to the market"
* somebody wanting to analyze human texts
  * wanting to do some analysis on New York Times articles, they want to first
    process the articles into meaningful tokens

## Design Rules

* We don't parse.
* Every token generated is callable (i.e., an atom or compound).
  * Example of an possible compound token: `space(' ')`.
  * Example of a possible atom token: `escape`.
  tokenization need to return tokens represented with the same arity)
* Users should be able to determine the kind of token by unification.
* Users should be able to clearly see and specify the precedence for tokenizaton
  * E.g., given `"-12.3"`, `numbers, punctuation` should yield `[pnct('-'),
    number(12), pnct('.'), number(3)]` while `punctuation, numbers` should yield
    `[number(-12.3)]`.
