:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(package, PackageDir)).

:- use_module(package(comment)).
:- begin_tests(tokenize_comment).

id(X)    --> {atom_codes(X,XX)},XX.
id(X,XX) --> {atom_codes(X,XX)},XX.

mytest(Tok,S,U) :-
    atom_codes(S,SS),
    call_dcg(Tok,SS,U).

test_comment(S) :-
    mytest(comment(id('<'),id('>')),S,[]).

test_comment_rec(S) :-
    mytest(comment_rec(id('<'),id('>')),S,[]).

test_comment_token(S,T) :-
    mytest(comment_token(id('<'),id('>'),TT),S,[]),
    atom_codes(T,TT).

test_comment_token_rec(S,T) :-
    mytest(comment_token_rec(id('<'),id('>'),TT),S,[]),
    atom_codes(T,TT).

start(AA) :-
    (
        catch(b_getval(a,[N,A]),_,N=0) ->
          true;
        N=0
    ),
    NN is N + 1,
    (
        N == 0 ->
          AA = _;
        AA = A
    ),
    b_setval(a,[NN,AA]).

end(A) :-
    b_getval(a,[N,A]),
    NN is N - 1,
    b_setval(a,[NN,A]).

left(A) -->
    {atom_codes(A,AA)},
    AA,
    {start(B)},
    [B].

left(A,C) -->
    {atom_codes(A,AA)},
    AA,
    {start(B)},
    [B],
    {append(AA,[B],C)}.

right(A) -->
    {end(B)},
    [B],
    {atom_codes(A,AA)},
    AA.

right(A,C) -->
    {end(B)},
    [B],
    {atom_codes(A,AA)},
    AA,
    {append([B],AA,C)}.

test_adapt(S,T) :-
    mytest(comment_token_rec(left('<'),right('>'),TT),S,[]),
    atom_codes(T,TT).


:- multifile test/2.

test('Test comment',[true(test_comment('<alla>'))]) :- true.
test('Test comment_rec',[true(test_comment_rec('<alla<balla>>'))]) :- true.
test('Test comment_token',[true(A == B)]) :-
    A='<alla>',
    test_comment_token(A,B).

test('Test comment_token_rec',[true(A == B)]) :-
    A='<alla<balla>>',
    test_comment_token(A,B).

test('Test comment_token_rec advanced 1',[true(A == B)]) :-
    A='<1 alla2> <1 balla2> 1>1>',
    test_adapt(A,B).

test('Test comment_token_rec advanced 2',[true(A == B)]) :-
    A='<2 alla1> <2 balla1> 2>2>',
    test_adapt(A,B).


:- end_tests(tokenize_comment).
