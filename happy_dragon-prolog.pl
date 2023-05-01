:-module('happy_dragon-prolog', []).
source_lang(en).
local_dict([is_a_parent_of, A, B], [dragon-dragon, dragon-dragon], [A, is, a, parent, of, B]).
local_dict([is_a_dragon, A], [creature-creature], [A, is, a, dragon]).
local_dict([is_healthy, A], [dragon-dragon], [A, is, healthy]).
local_dict([is_happy, A], [dragon-dragon], [A, is, happy]).
local_dict([smokes, A], [dragon-dragon], [A, smokes]).
local_meta_dict([],[],[]).
prolog_le(verified).
smokes(A) :-
    is_a_parent_of(B, A),
    is_a_dragon(B),
    smokes(B).
is_healthy(A) :-
    is_a_dragon(A),
    not smokes(A).
is_happy(A) :-
    is_a_dragon(A),
    forall(is_a_parent_of(A, B), is_healthy(B)).
is_(A, B) :-
    nonvar(B),
    A is B.
example(null, []).
example(smoky, [scenario([(is_a_dragon(bob):-true), (is_a_dragon(alice):-true), (is_a_parent_of(alice, bob):-true)], true)]).
query(null, true).
query(happy, is_happy(_)).
query(healthy, is_healthy(_)).
