# Contributing

Contributions of all kinds are welcome: feedback, PRs, reviews, ideas, bug
reports, etc.

## Code of Conduct

Please review and accept our [code of conduct](CODE_OF_CONDUCT.md) prior to
engaging in the project.

## Overall direction and aims

Consult the [`design_notes.md`](design_notes.md) to see the latest codified
design philosophy and principles.

## Setting up Development

1. Install swi-prolog's [swipl](http://www.swi-prolog.org/download/stable).
    - Optionally, you may wish to use [swivm](https://github.com/fnogatz/swivm) to
      manage multiple installed versions of swi-prolog.
2. Hack on the source code in `[./prolog](./prolog)`.
3. Run and explore your changes by loading the file in `swipl` (or using your
   editors IDE capabilities):
    - Example in swipl

    ```prolog
    # in ~/oss/tokenize on git:develop x [22:45:02]
    $ cd ./prolog

    # in ~/oss/tokenize/prolog on git:develop x [22:45:04]
    $ swipl
    Welcome to SWI-Prolog (threaded, 64 bits, version 8.0.2)
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
    Please run ?- license. for legal details.

    For online help and background, visit http://www.swi-prolog.org
    For built-in help, use ?- help(Topic). or ?- apropos(Word).

    % lod the tokenize module
    ?- [tokenize].
    true.

    % experiment
    ?- tokenize("Foo bar baz", Tokens).
    Tokens = [word(foo), space(' '), word(bar), space(' '), word(baz)].

    % reload the module when you make changes to the source code
    ?- make.
    % Updating index for library /usr/local/Cellar/swi-prolog/8.0.2/libexec/lib/swipl/library/
    true.

    % finished
    ?- halt.
    ```

Please ask here or in `##prolog` on [freenode](https://freenode.net/) if you
need any help! :)

## Running tests

Tests are located in the [`./test`](./test) directory. To run the test suite,
simply execute make test:

```sh
$ make test
% PL-Unit: tokenize .. done
% All 2 tests passed
```

If inside the swipl repl, make sure to load the test file and query run_tests.

```prolog
?- [test/test].
?- run_tests.
% PL-Unit: tokenize .. done
% All 2 tests passed
true.
```
