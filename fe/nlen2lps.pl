% Written by Jacinto Dávila
% Copyright Logical Contracts 2017, 2018. All rights reserved.
% "Logical Contracts" is a trademark for an upcoming software company, owned by:
%  Renting Point - Serviços de Informática, Lda., Portugal
%  Imperial Innovations Ltd, UK
%  Contratos Lógicos, CA, Merida, Venezuela
%  Robert Kowalski, UK
%  Fariba Sadri, UK
%
% nlen2lps.pl
% compiling formal natural language (English) into LPS (Wei Syntax)
%
% Jacinto Dávila
%
% Spec (from Bob Kowalski):
%
%    Use memnonic names for typed variables, e.g. person, customer, candidate, voter.
%    Use the determiner "a" for the first occurrence of a typed variable in a sentence.
%    Use "the" for subsequent occurrences of the same typed variable in the same sentence.
%    Use ordinals "first" "second" etc, for different variables of the same type in the same sentence.
%    Write predicates p(a, b) in infix form, eg p1 a p2 b p3.
%    Notice that this begins to resemble the non-positional arguments of F-logic, because
%    sometimes we may be able to write the same predicate in different orders eg p2 b p1 p3.
%    Keep the number of arguments of a predicate to a minimum
%
% Usage (standalone): en2lps(+Filename, -OutputFilename)

:- module(nlen2lps, [en2lps/2, en_string2lps_terms/2, contract/3, settings/4, content/3, read_to_tokens/2]).
:- thread_local fluent/3, action/3, error_at/2.
:- discontiguous statement/3, declaration/4, fluent/3, action/3.

:- op(900, fx, initiate).

en2lps(English_file, LPS_file) :-
   upload(English_file, Tokens), writeq(Tokens), nl, nl,
   contract(LPS, Tokens, []),
   filenaming(English_file, LPS_file),
   print_lps(LPS, LPS_file).

en_string2lps_terms(String,LPS) :-
	read_to_tokens(String,Tokens),
	contract(LPS, Tokens, []).

upload(File, Tokens) :-
   read_file_to_string(File,Text,[]),
   remove_comments(Text, CleanText),
   tokenize_atom(CleanText, Tokens), !.

% read_to_tokens(+String, -Tokens)
read_to_tokens(String, Tokens) :-
   string_codes(String, Codes),
   remove_comments(Codes, CleanCodes),
   tokenize_atom(CleanCodes, Tokens).

remove_comments(Text, NewText) :-
   string_codes(Text, Codes),
   delete_comments(Codes, NewCodes),
   string_codes(NewCodes, NewText), !.

delete_comments([],[]).
delete_comments(Input, NewText) :-
   comment_boundaries(Start, End),
   append(Start, Rest, Input),
   skip_comment(End, Rest, NewRest),
   delete_comments(NewRest, NewText).
delete_comments([H|R], [H|RR]) :-
   delete_comments(R, RR).

skip_comment(End, Input, Rest) :-
   append(End, Rest, Input), !.
skip_comment(End, [_H|R], RR) :- % actually skipping
   skip_comment(End, R, RR).

% Prolog comments
% "/*", "*/"
comment_boundaries([47, 42], [42, 47]).
% "%", "\n"
comment_boundaries([37], [10]).
% html comments
% "<!--", "-->"
comment_boundaries([60, 33, 45, 45], [45, 45, 62]).
% html tags
% "<html", ">"
comment_boundaries([60, 104, 116, 109, 108], [62]).
% "<body", ">"
comment_boundaries([60, 98, 111, 100, 121], [62]).
% "</html", ">"
comment_boundaries([60, 47, 104, 116, 109, 108], [62]).
% "</body", ">"
comment_boundaries([60, 47, 98, 111, 100, 121], [62]).

% expecting something.en to produce something.lpsw
filenaming(English_filename, Filename) :-
   sub_atom(English_filename, Prefix, _, _, 'en'),
   sub_atom(English_filename, 0, Prefix, _, Name),
   atom_concat(Name, 'lpsw', Filename), !.

% first approx
print_lps(LPS, File) :- tell(File), nl, nl, nl,
   print_all_sentences(LPS), nl, told.

print_all_sentences([]).
print_all_sentences([S|R]) :-
   writeq(S), write('.'), nl, nl, print_all_sentences(R).

% target (Wei Syntax) approximately
% spec ::= statement
%  statement ::= settings rules
%  settings ::= max_time  actions  fluents  initial_state  observations  events
%  rules ::= if_rules reactive_rules initiate_rules terminate_rules constraints
%  if_rules ::= if_rule | if_rule if_rules
%  if_rule ::= timeless_rule | event_rule | prolog_clause
%  timeless_rule ::= "l_timeless(" literal "," conjunction ")."
%  event_rule ::= "l_events(" happens_literal "," hold_conjunction_list ")."
%  reactive_rules ::= if_then_rule | if_then_rule if_then_rules
%  if_then_rule ::=  "reactive_rule(" conjunction "," conjunction ")."
%  constraints ::= constraint | constraint constraints
%  constraint ::= "d_pre(" conjunction ")."
%  conjunction ::= "true" | literal | literal "," conjunction

/* ---------------------------------------------------------- Grammar rules */
contract(FullText, In, Out) :-
  settings(Rules, Settings, In, Next),
  RulesforErrors =
    [(fluent(_, Pos, _R1)
       :- asserterror('Found no fluent at', Pos), fail),
     (action(_, Pos, _R2)
       :- asserterror('Found no action at', Pos), fail)],
  append(Rules, RulesforErrors, MRules),
  assertall(MRules), % asserting parsing rules for fluents and actions
  content(Content, Next, Out), append(Settings, Content, FullText).
contract(_,_,_) :- showerror, fail.

settings(AllR, AllS) --> declaration(Rules,Setting), settings(RRules, RS),
	{append(Setting, RS, AllS),
   append(Rules, RRules, AllR)}.
settings([],[]) --> [].

content(C) --> statement(S), content(R), {append(S,R,C)}.
content([]) --> [].

when_ --> ['When'].
when_ --> [when].

then_ --> [then].

and_ --> [and].
and_ --> [','].

%
conjunction(Map1, Map2, [C|CC]) --> literal(Map1, Map3, C),
   ( and_; comma ), conjunction(Map3, Map2, CC).
conjunction(Map1, Map2, [C]) --> literal(Map1, Map2, C).

to_ --> [to].

that_ --> [that].

become --> [becomes].

math_expression(V1 + V2, Map) --> type(V1, Map, Map0), plus, type(V2, Map0, _Map1).
math_expression(V1 - V2, Map) --> type(V1, Map, Map0), minus, type(V2, Map0, _Map1).
math_expression(N, _Map) --> word(N), {number(N)}.

plus --> [plus].

minus --> [minus].

declaration([],[maxTime(M)]) --> maximum_previous, word(M), period, {integer(M)}.

maximum_previous --> [the, maximum, time, 'is'].

/* ---------------------------------------------------- fluent declarations */
declaration(Rules, [fluents(Fluents)]) -->
    fluent_previous, list_of_fluents_decl(Rules, Fluents).

initially_ --> [initially, ':'].

fluent_previous --> [the, fluents, are, ':'].

list_of_fluents_decl([Ru|R1], [F|R2]) --> fluent_decl(Ru,F), list_of_fluents_decl(R1,R2).
list_of_fluents_decl([R],[F]) --> fluent_decl(R, F).

% the game is over, known as gameOver
fluent_decl(fluent(lambda([], Fluent), [is, O|R], R), Fluent) -->
   type(_X, [], _), is_, word(O), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N]}.

% the reward is an amount, known as reward,
fluent_decl(fluent(lambda([Y], Fluent), [is|R], R), Fluent) -->
   type(_X, [], _), is_, type(Y, [], _), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N,Y]}.

% a person has a balance, known as balanceOf,
fluent_decl(fluent(lambda([X,Y], Fluent), [V|R], R), Fluent) -->
   type(X, [], _), word(V), type(Y, [], _), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N,X,Y]}.

% a candidate has votes, known as voteCount.
fluent_decl(fluent(lambda([X,Y], Fluent), [V|R], R), Fluent) -->
   type(X, [], _), word(V), word(_), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N,X,Y]}.

% a candidate has a number of votes, known as voteCount
fluent_decl(fluent(lambda([X,Y], Fluent), [V|R], R), Fluent) -->
   type(X, [], _), word(V), type(Y, [], _),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y]}.

% a voter in a ballot has a number of votes, know as voter
fluent_decl(fluent(lambda([X,B,Y], Fluent), [V|R], R), Fluent) -->
   type(X, [], _), in, type(B, [], _), word(V), type(Y,[],_),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,B,Y]}.

% a voter is represented by a voter, know as representedBy
fluent_decl(fluent(lambda([X,Y], Fluent), [is, V, by|R], R), Fluent) -->
   type(X, [], _), is_, word(V), by, type(Y, [], _), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N,X,Y]}.

% a delegate representes a voter, alter know as representedBy
fluent_decl(fluent(lambda([X,Y], Fluent), [V|R], R), Fluent) -->
    type(X, [], _), word(V), type(Y, [], _), comma, alter, known_as, word(N),
    comma_or_period, {Fluent =.. [N,Y,X]}.

% a player has played a choice, known as played.
fluent_decl(fluent(lambda([X,Y], Fluent), [has, V|R], R), Fluent) -->
   type(X, [], _), word(has), word(V), type(Y, [], _),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y]}.

% a voter has voted for a candidate in a ballot, known as voted.
fluent_decl(fluent(lambda([X,Y,B], Fluent), [has, V|R], R), Fluent) -->
   type(X, [], _), word(has), word(V), for, type(Y, [], _), in, type(B, [],_),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y,B]}.

fluent_decl(_, _, Pos, _) :- asserterror('Found no fluent declaration at ', Pos), fail.

/* --------------------------------------------- undeclared external fluents */

% e_getBalance the address latest a value
external_fluent(lambda([A, L, V], Fluent)) --> word(E), {Fluent =.. [E, A, L, V]}.

% e_existsTransactionReceipt the payment
external_fluent(lambda([P], Fluent)) --> word(E), {Fluent =.. [E, P]}.

/* ---------------------------------------------------------------- lexicon */

is_ --> ['is'].
is_ --> ['are']. % correct?

alter --> [alter].

in --> [in].
on --> [on].

by --> [by].

comma --> [','].
period --> ['.'].

known_as --> [known, as].

comma_or_period --> comma.
comma_or_period --> period.

called_ --> [called].

/* ---------------------------------------------- prolog events declarations */
declaration(Rules, [prolog_events(Actions)]) -->
    prolog_events_previous, list_of_actions_decl(Rules, Actions).

prolog_events_previous --> [the, prolog, events, are, ':'].

/* ----------------------------------------------------- events declarations */
declaration(Rules, [events(Actions)]) -->
    events_previous, list_of_actions_decl(Rules, Actions).

events_previous --> [the, events, are, ':'].

/* ----------------------------------------------------- action declarations */
declaration(ActRules, [actions(Actions)]) -->
    action_previous, list_of_actions_decl(ActRules, Actions).

action_previous --> [the, actions, are, ':'].

list_of_actions_decl([Ru|R1], [F|R2]) -->
    action_decl(Ru, F), list_of_actions_decl(R1,R2).
list_of_actions_decl([R], [F]) --> action_decl(R, F).

% e_transaction a time a first address an input an amount a second address.
action_decl(action(lambda([T,A1,I, Wei, A2], Action), In, Out) :- word(W, In, Out), Action) -->
   word(W),
   t_or_w(T, [], _), type(A1, [], _), type(I, [], _), type(Wei, [], _), type(A2, [], _),
   comma_or_period, {Action =.. [W,T,A1,I,Wei,A2]}.

% no need for this. Assumed as timeless_command
% e_sendTransaction the address the player the prize a payment from the first time to a third time
% action_decl(action(lambda([A1,Player, Prize, Payment], Action), In, Out) :- word(W, In, Out), Action) -->
%   word(W),
%   t_or_w(A1, [], _), type(Player, [], _), type(Prize, [], _), type(Payment, [], _),
%   comma_or_period, {Action =.. [W, A1, Player, Prize, Payment]}.

% a prize is sent to a winner. send(_Winner,_Prize)
action_decl(action(lambda([Y,X], Action), [is, V|R], R), Action) -->
   type(X, [], _), is_, word(V), to_,  type(Y, [], _),
   comma_or_period, {Action =.. [V,Y,X]}.

% a player inputs a choice and a value.
action_decl(action(lambda([X,Y,Z], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), type(Y, [], _), and_, type(Z, [], _),
   comma_or_period, {Action =.. [V,X,Y,Z]}.

% a first person transfers an amount to a second person,
action_decl(action(lambda([X,Y,Z], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), type(Y, [], _), to_, type(Z, [], _),
   comma_or_period, {Action =.. [V,X,Y,Z]}.

% a first voter delegates to a second voter in a ballot.
action_decl(action(lambda([X,Y,B], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), to_, type(Y, [], _), in, type(B, [],_),
   comma_or_period, {Action =.. [V,X,Y,B]}.

% a first person votes for a second person in a ballot,
% a voter votes for a candidate in a ballot.
action_decl(action(lambda([X,Y,B], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), for, type(Y, [], _), in, type(B, [],_),
   comma_or_period, {Action =.. [V,X,Y,B]}.

% a chairman authorises a voter on a ballot,
action_decl(action(lambda([X,Y,B], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), type(Y, [], _), on, type(B, [],_),
   comma_or_period, {Action =.. [V,X,Y,B]}.

% voter1 delegates to voter2,
action_decl(action(lambda([X,Y], Action), [V|R], R), Action) -->
   word(_), word(V), to_, word(_), comma_or_period, {Action =.. [V,X,Y]}.

% voter votes for candidate in ballot,
action_decl(action(lambda([X,Y], Action), [V|R], R), Action) -->
   word(_), word(V), for, word(_), in, word(_),
   comma_or_period, {Action =.. [V,X,Y]}.

% a chairman creates a ballot,
action_decl(action(lambda([X,Y], Action), [V|R], R), Action) -->
   type(X, [], _), word(V), type(Y, [], _),
   comma_or_period, {Action =.. [V,X,Y]}.

action_decl(_, _, Pos, _)
    :- asserterror('Found no action declaration at ', Pos), fail.

for --> [for].

obs_previous --> [the, observations, are, ':'].

list_of_obs_stat([observe(F,T)|R]) --> list_of_obs(F,T), comma, list_of_obs_stat(R).
list_of_obs_stat([observe(F,T)]) --> list_of_obs(F,T), period.

list_of_obs([E|RE], T2) -->
    obs_decl(E), and_, list_of_obs(RE, T2).
list_of_obs([E], T2) --> obs_decl(E), time_expression([_T1, T2]).


/* ---------------------------------------------------------------- obs_decl */
% adjusting to Wei syntax in Miguel s psyntax.P
% notice that observations are statements, not declarations.
% this version, no variables allowed.

% prize is sent to winner.
obs_decl(Action) -->
    word(X), action(lambda([Y,X], Action)), to_,  word(Y).

% player inputs choice and value.
obs_decl(Action) -->
    word(X), action(lambda([X,Y,Z], Action)), word(Y), and_, word(Z).

% p1 transfers a1 to p2,
obs_decl(Action) -->
    word(X), action(lambda([X,Y,Z], Action)), word(Y), to_,  word(Z).

% v1 delegates to v2 in ballot.
obs_decl(Action) -->
    word(X), action(lambda([X,Y,B], Action)), to_,  word(Y), in, word(B).

% v1 votes for c1 in b1.
obs_decl(Action) -->
    word(X), action(lambda([X,Y,B], Action)), for, word(Y), in, word(B).

% c1 authorises v1 on b1,
obs_decl(Action) -->
    word(X), action(lambda([X,Y,B], Action)), word(Y), on, word(B).

% voter1 delegates to voter2,
obs_decl(Action) -->
    word(X), action(lambda([X,Y], Action)), to_,  word(Y).

% chairman creates ballot,
obs_decl(Action) -->
    word(X), action(lambda([X,Y], Action)), word(Y).

obs_decl(_, Pos, _) :- asserterror('Found no observation at ', Pos), fail.

time_expression([T1,T2]) --> from_, word(T1), to_, word(T2).
time_expression([_T,T1]) --> at_, word(T1). % by ending time

mapped_time_expression([T1,T2], Mi, Mo) -->
   from_, t_or_w(T1, Mi, M2), to_, t_or_w(T2, M2, Mo).
mapped_time_expression([_T0,T], Mi, Mo) -->
   at_, t_or_w(T, Mi, Mo).

at_ --> [at].
from_ --> [from].

different_from --> [different, from].

arithmetic_expression(lambda([C1, C2], C1 \= C2)) -->
    different_from.
arithmetic_expression(lambda([C1, C2], C1 =< C2)) -->
    [equal, or, less, than].
arithmetic_expression(lambda([C1, C2], C1 >= C2)) -->
    [greater, or, equal, than].
arithmetic_expression(lambda([C1, C2], C1 < C2)) -->
    [less, than].
arithmetic_expression(lambda([C1, C2], C1 >= C2)) -->
    [greater, or, equal, than].
arithmetic_expression(lambda([C1, C2], C1 > C2)) -->
    [greater, than].
arithmetic_expression(lambda([C1, C2], C1 = C2)) -->
    [the, same, as].
% a second number is half the first number
arithmetic_expression(lambda([C1, C2], C1 is C2/2)) -->
    [half].
% the sum of all
arithmetic_expression(lambda([C1,P,C,T],
  sum_all(holds(Fluent,T), C1))) -->
    the_sum_of_all,
    t_or_w(P, [], M2), fluent(lambda([P,C], Fluent)),
    t_or_w(C, M2, M3), at_, t_or_w(T, M3, _).
% approximately 0.9 times the value
arithmetic_expression(lambda([C1, N, C2], C1 is round(N*C2))) -->
    approximatelly_(N).

approximatelly_(N) --> [approximately, N, times], {number(N)}.

the_sum_of_all --> [the, sum, of, all].

not_ --> ['not'].

opposite(A,B) :- ropposite(A,B); ropposite(B,A).

ropposite(C1 \= C2, C1 = C2).
ropposite(C1 =< C2, C1 > C2).
ropposite(C1 >= C2, C1 < C2).

command(M, M, lps_terminate) --> [lps, '_', terminate].
command(M, M, initiate gameOver) --> [initiate, the, game, is, over].
% e_sendTransaction the address the player the prize a payment
command(M_In,  M_Out, e_sendTransaction(A, Player, Prize, Payment)) -->
  word(e_sendTransaction), t_or_w(A, M_In, M1),
  type(Player, M1, M2), type(Prize, M2, M3), type(Payment, M3, M_Out).

timeless_command(M_In, M_Out, lps_my_account(A)) -->
  word(lps_my_account), t_or_w(A, M_In, M_Out).

/* ---------------------------------------------------------------- literal */
% it is not the case that the player has played a second choice from ..
literal(M_In,M_Out, holds(not(Fluent), T)) -->
   it_is_not_the_case, that_, literal(M_In, M_Out, holds(Fluent, T)).

% lps_terminate from a first time to a second time
% e_sendTransaction the address the player the prize a payment from the first time to a third time
literal(M_In,M_Out, happens(Command, T1, T2)) -->
    command(M_In, M_Next, Command),
    mapped_time_expression([T1,T2], M_Next, M_Out).

% any of the holds and happens below plus time expression
% a player pays a prize, called pay,
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(X, M_In, M2), word(_), % one-word defined literal
    t_or_w(Y, M2, M_Next), comma, called_, word(P), comma,
    mapped_time_expression([T1,T2], M_Next, M_Out), {Action =..[P, X, Y]}.

% e_getBalance the address latest a value at the time
literal(M_In,M_Out, holds(Fluent, T)) -->
    external_fluent(lambda([A, L, V], Fluent)), t_or_w(A, M_In, M1),
    t_or_w(L, M1, M2), t_or_w(V, M2, M3), at_, t_or_w(T, M3, M_Out).

% e_existsTransactionReceipt the payment at the second time.
literal(M_In,M_Out, holds(Fluent, T)) -->
    external_fluent(lambda([P], Fluent)), t_or_w(P, M_In, M1),
    at_, t_or_w(T, M1, M_Out).

% a player has played a choice at ..
literal(M_In,M_Out, holds(Fluent, T)) -->
    t_or_w(P, M_In, M2), fluent(lambda([P,C], Fluent)),
    t_or_w(C, M2, M3), at_, t_or_w(T, M3, M_Out).

% implicit fluent declaration
% the players are a number, known as num_players, at a time
% the reward is a prize, known as reward,
literal(M_In,M_Out, holds(Fluent, T)) -->
    t_or_w(P, M_In, M2), is_,
    t_or_w(N, M2, M3),
    comma, known_as, word(Name), comma,
    at_, t_or_w(T, M3, M_Out), {Fluent =.. [Name, P], P=N}.

% the game is over
literal(M_In,M_Out, holds(Fluent, T)) -->
    t_or_w(_P, M_In, M2), fluent(lambda([], Fluent)),
    mapped_time_expression([_,T], M2, M_Out).

% the reward is an amount
literal(M_In,M_Out, holds(Fluent, T)) -->
    t_or_w(_P, M_In, M2), fluent(lambda([N], Fluent)),
    t_or_w(N, M2, M3), mapped_time_expression([_,T], M3, M_Out).

% a player inputs a choice and a value from ..
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(P, M_In, M2), action(lambda([P,C,V], Action)),
    t_or_w(C, M2, M3), and_, t_or_w(V, M3, M4),
    mapped_time_expression([T1,T2], M4, M_Out).

% a player pays a prize
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(Pl, M_In, M2), action(lambda([Pl, Pr], Action)),
    t_or_w(Pr, M2, M3), mapped_time_expression([T1,T2], M3, M_Out).

% a prize is sent to somebody at..
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(P, M_In, M2), action(lambda([W,P], Action)),
    to_, t_or_w(W, M2, M3), mapped_time_expression([T1,T2], M3, M_Out).

% a voter votes for a first candidate in a ballot
% the voter votes for a second candidate in the ballot at..
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(X, M_In, M2), action(lambda([X,Y,B], Action)), for,
    t_or_w(Y, M2, M3), in, t_or_w(B, M3, M4),
    mapped_time_expression([T1,T2], M4, M_Out).

% a first voter delegates to a second voter in a ballot from ..
% the first voter delegates to a third voter in the ballot
% a first voter delegates to a second voter in a ballot
% and a third voter delegates to the second voter in the ballot
literal(M_In,M_Out, happens(Action, T1, T2)) -->
    t_or_w(X, M_In, M2), action(lambda([X,Y,B], Action)), to_,
    t_or_w(Y, M2, M3), in, t_or_w(B, M3, M4),
    mapped_time_expression([T1,T2], M4, M_Out).

% anyone of the holds and happens above without time expression
% implicit predicate declaration
% a player pays a prize, known as pay,
literal(M_In,M_Out, happens(Action, _T1, _T2)) -->
    t_or_w(X, M_In, M2), word(_), % one-word defined literal
    t_or_w(Y, M2, M_Out), comma, known_as, word(P), comma, {Action =..[P, X, Y]}.

% the reward is an amount
literal(M_In,M_Out, holds(Fluent, _T)) -->
    t_or_w(_P, M_In, M2), fluent(lambda([N], Fluent)),
    t_or_w(N, M2, M_Out).

% a player has played a choice.
literal(M_In, M_Out, holds(Fluent, _T)) -->
    t_or_w(P, M_In, M2), fluent(lambda([P,C], Fluent)),
    t_or_w(C, M2, M_Out).

% a player inputs a choice and a value
literal(M_In,M_Out, happens(Action, _T1, _T2)) -->
    t_or_w(P, M_In, M2), action(lambda([P,C,V], Action)),
    t_or_w(C, M2, M3), and_, t_or_w(V, M3, M_Out).

% a prize is sent to somebody
literal(M_In,M_Out, happens(Action, _T1, _T2)) -->
    t_or_w(P, M_In, M2), action(lambda([W,P], Action)),
    to_, t_or_w(W, M2, M_Out).

% a voter votes for a first candidate in a ballot
% the voter votes for a second candidate in the ballot
literal(M_In,M_Out, happens(Action, _T1, _T2)) -->
    t_or_w(X, M_In, M2), action(lambda([X,Y,B], Action)), for,
    t_or_w(Y, M2, M3), in, t_or_w(B, M3, M_Out).

% a first voter delegates to a second voter in a ballot
% the first voter delegates to a third voter in the ballot
% a first voter delegates to a second voter in a ballot
% and a third voter delegates to the second voter in the ballot
literal(M_In,M_Out, happens(Action, _T1, _T2)) -->
    t_or_w(X, M_In, M2), action(lambda([X,Y,B], Action)), to_,
    t_or_w(Y, M2, M3), in, t_or_w(B, M3, M_Out).

/* ----------------------------------------------------- timeless literals */
% it is not the case that the first choice beats the second choice
literal(M_In,M_Out, not(Timeless)) -->
   it_is_not_the_case, that_, literal(M_In, M_Out, Timeless).

% lps_my_account the second address
literal(M_In,M_Out, Command) -->
    timeless_command(M_In, M_Out, Command).

% the first player is not the same as the second player
literal(M_In,M_Out, Opposite) -->
    t_or_w(C1, M_In, M2), is_, not_, arithmetic_expression(lambda([C1, C2], Expression)),
    t_or_w(C2, M2, M_Out), {opposite(Expression, Opposite)}.

% the amount at the time is the sum of all a player has played a value at the time
literal(M_In,M_Out, Expression) -->
    t_or_w(A, M_In, M2), at_, t_or_w(T, M2, M_Out), is_,
    arithmetic_expression(lambda([A, _C1, _C2, T], Expression)).

% the number is approximately 0.9 times the value.
literal(M_In,M_Out, Expression) -->
    t_or_w(C1, M_In, M2), is_, arithmetic_expression(lambda([C1, _N, C2], Expression)),
    t_or_w(C2, M2, M_Out).

% the first candidate is different from the second candidate.
% the second voter is different from the third voter.
literal(M_In,M_Out, Expression) -->
    t_or_w(C1, M_In, M2), is_, arithmetic_expression(lambda([C1, C2], Expression)),
    t_or_w(C2, M2, M_Out).

% a candidate belongs to the ballot
literal(M_In,M_Out, Cond) -->
    t_or_w(X, M_In, M2), word(Predicate), to_, t_or_w(Y, M2, M_Out),
    {Cond =.. [Predicate, X, Y]}.

% scissors beats paper.
literal(M_In,M_Out, Cond) -->
    t_or_w(X, M_In, M2), word(Predicate), t_or_w(Y, M2, M_Out),
    {Cond =.. [Predicate, X, Y]}.

literal(M, M, _, Pos, _) :- asserterror('Found no literal at ', Pos), fail.

/* ------------------------------------------------------ observe statements */

statement(Obs) --> obs_previous, list_of_obs_stat(Obs).

/* ---------------------------------------------------- initially statements */
statement([initial_state(States)]) -->
    initially_, list_of_init(States), period.

list_of_init([E|RE]) -->
    state(E), and_, list_of_init(RE).
list_of_init([E]) --> state(E).

% the reward is amount
state(Fluent) -->
   type(_X, [], _), fluent(lambda([Y], Fluent)), word(Y).

/* --------------------------------------------------------- prolog staments */

statement([Prolog]) --> prolog_fact(Prolog).

prolog_fact(Prolog) -->  % binary predicate
    t_or_w(X, [], M2), word(Predicate), t_or_w(Y, M2, _M_Out), period,
    {Prolog =.. [Predicate, X, Y]}.

/* --------------------------------------------------------- When statements */
/* When e_transaction latest a first address an input an amount a second address
and lps_my_account the second address
and the amount is greater than 0
and it is not the case that the first address has played a second choice
then the first address has played the input.
*/
% initiated(happens(e_transaction(latest,_1032,_1034,_1036,_1038),_1024,_1026),played(_1032,_1034),[lps_my_account(_1038),_1036>0,holds(not played(_1032,_1088),_1024)]).
statement([initiated(happens(A,T1,_T2),F, Conds)]) -->
   when_, action(lambda([T, A1, I, W, A2],A)), t_or_w(T, [], Map1),
          t_or_w(A1, Map1, Map2), t_or_w(I, Map2, Map3), t_or_w(W, Map3, Map4), t_or_w(A2, Map4, Map5),
   and_, simul_conjunction(T1, Map5, Map6, Conds),
   then_, t_or_w(P, Map6, Map7), fluent(lambda([P,C],F)), t_or_w(C, Map7, _Map8), period.

/* When a player inputs a choice and a value
and the value is greater than 0
and it is not the case that the player has played a second choice
then the player has played the choice.
*/
% player_input(Sender,Choice,_Value) initiates played(Sender,Choice).
statement([initiated(happens(A,T1,_T2),F, Conds)]) -->
   when_, t_or_w(P,[], Map1), action(lambda([P,C,V],A)), t_or_w(C, Map1, Map2),
   and_, t_or_w(V, Map2, Map3),
   and_, simul_conjunction(T1, Map3, Map4, Conds),
   then_, t_or_w(P, Map4, Map5), fluent(lambda([P,C],F)), t_or_w(C, Map5, _Map6), period.

/* When a player inputs a choice and a value then the player has played the choice.
*/
% player_input(Sender,Choice,_Value) initiates played(Sender,Choice).
statement([initiated(happens(A,_T1,_T2),F,[])]) -->
   when_, t_or_w(P,[], Map1), action(lambda([P,C,V],A)), t_or_w(C, Map1, Map2),
   and_, t_or_w(V, Map2, Map3),
   then_, t_or_w(P, Map3, Map4), fluent(lambda([P,C],F)), t_or_w(C, Map4, _Map5), period.

/* When a player inputs a choice and a value
then the reward that is a number becomes the number plus the value.
*/
% player_input(_Sender,_Choice,Value) updates Old to New in reward(Old) if New is Old+Value.

statement([updated(happens(Action,_T1,_T2),Fluent,O-Y,[Y is Exp])]) -->
   when_, t_or_w(P,[], Map1), action(lambda([P,C,V],Action)), t_or_w(C, Map1, Map2),
        and_, t_or_w(V, Map2, Map3),
   then_, t_or_w(_R, _, _), that_, fluent(lambda([O], Fluent)), t_or_w(O, Map3, Map5),
   become, math_expression(Exp, Map5), period.

/* When a player pays a prize
then the reward that is a number becomes the number minus the prize.
   */
%  pay(_,Prize) updates Old to New in reward(Old) if New is Old-Prize.

statement([updated(happens(Action,_T1,_T2),Fluent,O-Y,[Y is Exp])]) -->
  when_, t_or_w(Player,[], Map1), action(lambda([Player, Prize], Action)),
          t_or_w(Prize, Map1, Map2),
  then_, t_or_w(_R, _, _), that_, fluent(lambda([O], Fluent)), t_or_w(O, Map2, Map5),
  become, math_expression(Exp, Map5), period.

/* When a prize is sent to a player
then the reward that is a number becomes the number minus the prize.
*/
%  send(_Player,Prize) updates Old to New in reward(Old) if New is Old-Prize.

statement([updated(happens(Action,_T1,_T2),Fluent,O-Y,[Y is Exp])]) -->
   when_, t_or_w(Prize,[], Map1), action(lambda([Player, Prize], Action)),
         to_, t_or_w(Player, Map1, Map2),
   then_, t_or_w(_R, _, _), that_, fluent(lambda([O], Fluent)), t_or_w(O, Map2, Map5),
   become, math_expression(Exp, Map5), period.


/* 	When a chairman creates a ballot
and a candidate belongs to the ballot
then the candidate has 0 votes.
*/
% do(create(Chairman, Ballot) ) initiates voteCount(Candidate, 0) if member(Candidate, Ballot).
% initiated(happens(do(create(_704,_706)),_924,_930),voteCount(_714,0),[member(_714,_706)]).

statement([initiated(happens(A,_T1,_T2),F,Conds)]) -->
   when_, t_or_w(S,[], Map1), action(lambda([S,O],A)), t_or_w(O, Map1, Map2),
   and_, conjunction(Map2, Map3, Conds),
   then_, t_or_w(O, Map3, Map4), fluent(lambda([O,V],F)), t_or_w(V, Map4, _Map5), period.

% action(lambda([S,O],create(S,O))) --> [creates].
% fluent(lambda([Arg1,Arg2],voteCount(Arg1,Arg2))) --> [has].

/* 	When a chairman authorises a voter on a ballot
then the voter in the ballot has 1 votes
and the voter represents the voter.
*/
% do(authorise(Chairman, Voter, Ballot) ) initiates voter(Voter, Ballot, 1).
% do(authorise(Chairman, Voter, Ballot) ) initiates representedBy(Voter, Voter).

statement([initiated(happens(A,_T1,_T2),F1,[]),initiated(happens(A,_T3,_T4),F2,[])]) -->
   when_, t_or_w(S,[], Map1), action(lambda([S,O2,B],A)), t_or_w(O2, Map1, Map2),
          on, t_or_w(B, Map2, Map3),
   then_, t_or_w(O2, Map3, Map3), in, t_or_w(B, Map3, Map3),
         fluent(lambda([O2,B,V1],F1)), t_or_w(V1, Map3, Map5), and_,
         t_or_w(O2, Map5, Map6), fluent(lambda([O2,V2],F2)), t_or_w(V2, Map6, _Map7), period.

% action(lambda([S,O1,O2],authorise(S,O1,O2))) --> [authorises].
% luent(lambda([Arg1,Arg2],voter(Arg1,Arg2))) --> [has].

/*	When a first voter delegates to a second voter in a ballot
then the number of votes that the first voter has becomes 0.
*/
% do(delegate(Voter1, Voter2, Ballot) ) initiates voter(Voter1, Ballot, 0).
% do(delegate(Voter1, Voter2, Ballot) ) terminates voter(Voter1, Ballot, _).

statement([updated(happens(Action,_T1,_T2),Fluent,_-V,[V is Exp])]) -->
   when_, t_or_w(F,[], Map1), action(lambda([F,S,B],Action)), to_, t_or_w(S, Map1, Map2),
        in, t_or_w(B, Map2, Map3),
   then_, t_or_w(V, Map3, Map4), that_, t_or_w(F, Map4, Map5),
   fluent(lambda([S,V],Fluent)), become, math_expression(Exp, Map5), period.

% This is perhaps the trickiest bit of the representation.
/* 	When a first voter delegates to a second voter in a ballot,
    	and the first voter represents a third voter
	and a fourth voter represents the second voter
	then the voter that represents the third voter becomes the fourth voter.
*/
% do(delegate(Voter1, Voter2, Ballot) ) initiates representedBy(Voter, Delegate)
% if		representedBy(Voter, Voter1), representedBy(Voter2, Delegate).
% do(delegate(Voter1, Voter2, Ballot) ) terminates representedBy(_, Voter1).

statement([updated(happens(Action,T1,_T2),Fluent3,_-Fo,
  [holds(Fluent1, T1), holds(Fluent2, T1), V = Fo])]) -->
   when_, t_or_w(F,[], Map1), action(lambda([F,S,B],Action)),
      to_,  t_or_w(S, Map1, Map2), in, t_or_w(B, Map2, Map3),
   and_, t_or_w(F, Map3, Map4), fluent(lambda([F,Th], Fluent1)), t_or_w(Th, Map4, Map5),
   and_, t_or_w(Fo, Map5, Map6), fluent(lambda([Fo, S], Fluent2)), t_or_w(S, Map6, Map7),
   then_, t_or_w(V, Map7, Map8), that_, fluent(lambda([V,Th],Fluent3)), t_or_w(Th, Map8, Map9),
      become, t_or_w(Fo, Map9, _Map10), period.

%This is the case where a delegate’s representative has not yet voted.
/* 	When a first voter delegates to a second voter in a ballot
    	and a third voter represents the second voter
	and it is not the case that the third voter has voted for a candidate in the ballot
	and the first voter in the ballot has a number of votes
  and the third voter in the ballot has a number of votes
	then the number of votes that the third voter in the ballot has becomes
	the number of votes that the third voter in the ballot has plus
	the number of votes that the first voter in the ballot has.
*/
% do(delegate(Voter1, Voter2, Ballot) ) initiates voter(Delegate, Ballot, New)
% if		representedBy(Voter2, Delegate),
% not voted(Delegate, _, Ballot),
%		voter(Voter1, Ballot, N),
% voter(Delegate, Ballot, Old),
% New is Old + N.
% do(delegate(Voter1, Voter2, Ballot) ) 	terminates voter(Delegate, Ballot, _)
% if		representedBy(Voter2, Delegate),
% not voted(Delegate, _, Ballot).

statement([updated(happens(Action,T1,_T2),Fluent5,_-N1,
   [holds(Fluent1, T1), holds(not(Fluent2), T1),
    holds(Fluent3, T1), holds(Fluent4, T1), N1 is Exp])]) -->
   when_, t_or_w(F,[], Map1), action(lambda([F,S,B],Action)), to_,
        t_or_w(S, Map1, Map2), in, t_or_w(B, Map2, Map3),
   and_, t_or_w(Th, Map3, Map4), fluent(lambda([Th,S], Fluent1)), t_or_w(S, Map4, Map5),
   and_, it_is_not_the_case, that_, t_or_w(Th, Map5, Map6), fluent(lambda([Th, C, B], Fluent2)),
      for, t_or_w(C, Map6, Map7), in, t_or_w(B, Map7, Map8),
   and_, t_or_w(F, Map8, Map9), in, t_or_w(B, Map9, Map10),
        fluent(lambda([F,B,N], Fluent3)), t_or_w(N, Map10, Map11),
   and_, t_or_w(Th, Map11, Map12), in, t_or_w(B, Map12, Map13),
        fluent(lambda([Th,B,M], Fluent4)), t_or_w(M, Map13, _Map14),
   then_, t_or_w(N1, [], _), that_, t_or_w(Th, [], _),
       in, t_or_w(B, [], _), fluent(lambda([Th, B, N1],Fluent5)), become,
      t_or_w(NV1, [], _), that_, t_or_w(Th, [], _),
      in, t_or_w(B, [], _), fluent(lambda([Th, B, NV1], Fluent4)), operator(Op),
      t_or_w(NV2, [], _), that_, t_or_w(F, [], _),
      in, t_or_w(B, [], _), fluent(lambda([F, B, NV2], Fluent3)), period,
   {Exp =.. [Op, NV1, NV2]}.

it_is_not_the_case --> [it, 'is', not, the, case].

operator('+') --> [plus].
operator('-') --> [minus].
operator('*') --> [times].
operator('/') --> [divided, by].

%This is the case where a delegate has already voted when a delegation is made.
/* 	When a first voter delegates to a second voter in a ballot
and a third voter represents the second voter
and the third voter has voted for a candidate in the ballot
and the first voter has a number of votes
and the candidate has a number of votes
then the number of votes that the candidate has becomes
the number of votes that the candidate has plus
the number of votes that the first voter has.
*/
% do(delegate(Voter1, Voter2, Ballot) ) initiates voteCount(Candidate, NewVotes)
% if		representedBy(Voter2, Delegate),
% voted(Delegate, Candidate, Ballot),
% voter(Voter1, Ballot, N),
% voteCount(Candidate, OldVotes),
% NewVotes is OldVotes + N.
% do(delegate(Voter1, Voter2, Ballot) ) terminates voteCount(Candidate, _)
% if		representedBy(Voter2, Delegate),
% voted(Delegate, Candidate, Ballot).

statement([updated(happens(Action,T1,T2),Fluent5,_-N1,
   [holds(Fluent1, T1), holds(Fluent2, T1),
    holds(Fluent3, T2), holds(Fluent4, T1), N1 is Exp])]) -->
   when_, t_or_w(F,[], Map1), action(lambda([F,S,B],Action)),
        to_, t_or_w(S, Map1, Map2), in, t_or_w(B, Map2, Map3),
   and_, t_or_w(Th, Map3, Map4), fluent(lambda([Th, S], Fluent1)),
        t_or_w(S, Map4, Map5),
   and_, t_or_w(Th, Map5, Map6),
        fluent(lambda([Th,C,B], Fluent2)), for, t_or_w(C, Map6, Map7),
        in, t_or_w(B, Map7, Map8),
   and_, t_or_w(F, Map8, Map9),
        fluent(lambda([F,N], Fluent3)), t_or_w(N, Map9, Map10),
   and_,  t_or_w(C, Map10, Map11),
         fluent(lambda([C,M], Fluent4)), t_or_w(M, Map11, _Map12),
   then_, t_or_w(N1, [], _), that_, t_or_w(C, [], _),
         fluent(lambda([C,N1],Fluent5)), become,
         t_or_w(NV1,[], _), that_, t_or_w(C, [], _),
         fluent(lambda([C, NV1], Fluent4)), operator(Op),
         t_or_w(NV2, [], _), that_, t_or_w(F, [], _),
         fluent(lambda([F, NV2], Fluent3)), period,
      {Exp =.. [Op, NV1, NV2]}.


/* 	When a voter votes for a candidate in a ballot
then the voter has voted for the candidate in the ballot
and the number of votes that the voter has becomes 0.
*/
% do(vote(Voter, Candidate, Ballot) ) initiates voted(Voter, Candidate, Ballot).
% do(vote(Voter, Candidate, Ballot) ) initiates voter(Voter, Ballot, 0).
% do(vote(Voter, Candidate, Ballot) ) terminates voter(Voter, Ballot, _).

statement([initiated(happens(Action),T1,T2,Fluent1,[]),
            updated(happens(Action,T1,T2),Fluent2,_-N, [N is Exp])]) -->
   when_, t_or_w(V,[], Map1), action(lambda([V,C,B],Action)),
        for, t_or_w(C, Map1, Map3), in, t_or_w(B, Map3, Map4),
   then_, t_or_w(V, [], _), fluent(lambda([V,C,B],Fluent1)),
        for, t_or_w(C, [], _), in, t_or_w(B, [],_),
   and_, t_or_w(N, [], _), that_, t_or_w(V, [], _),
        fluent(lambda([V,B,N],Fluent2)),
   become, math_expression(Exp, Map4), period.

/* 	When a voter votes for a candidate in a ballot
  and the voter in the ballot has a number of votes
  and the candidate has a number of votes
	then the number of votes that the candidate has becomes
	the number of votes that the candidate has plus
	the number of votes that the voter in the ballot has.
*/
% do(vote(Voter, Candidate, Ballot) ) initiates voteCount(Candidate, NewVotes)
% if		voter(Voter, Ballot, N),
% voteCount(Candidate, OldVotes),
% NewVotes is OldVotes + N.
% do(vote(Voter, Candidate, Ballot) ) terminates voteCount(Candidate, _).

statement([updated(happens(Action,T1,_T2),Fluent3,_-N1,
   [holds(Fluent1, T1), holds(Fluent2, T1), N1 is Exp])]) -->
   when_, t_or_w(V,[], Map1), action(lambda([V,C,B],Action)),
        for, t_or_w(C, Map1, Map2), in, t_or_w(B, Map2, Map3),
   and_, t_or_w(V, Map3, Map4), in, t_or_w(B, Map4, Map5),
        fluent(lambda([V,B,N], Fluent1)), t_or_w(N, Map5, Map6),
   and_, t_or_w(C, Map6, Map7), fluent(lambda([C,M], Fluent2)), t_or_w(M, Map7, _Map8),
   then_, t_or_w(N1, [], _), that_, t_or_w(C, [], _), fluent(lambda([C, N1],Fluent3)), become,
         t_or_w(NV1,[], _), that_, t_or_w(C, [], _), fluent(lambda([C, NV1], Fluent2)), operator(Op),
         t_or_w(NV2, [], _), that_, t_or_w(V, [], _),
         in, t_or_w(B, [], _), fluent(lambda([V, B, NV2], Fluent1)), period,
      {Exp =.. [Op, NV1, NV2]}.

% When a first person transfers an amount to a second person
% then the balance that the second person has becomes the balance plus the amount
% and the balance that the first person has becomes the balance minus the amount.

% Old in balance(T, Old) becomes  Old + A if  transfer(F, T, A).
% Old in balance(F, Old) becomes  Old - A if  transfer(F, T, A).
% 	Event updates Old to New in Fluent if Conditions.
%	Event updates Old to New in Fluent.

% updated(Ev, Fl, Change, Cond),
% updated(happens(transfer(_7054,_7056,_7058),_7362,_7368),balance(_7056,_7062),_7062-_7064,[_7064 is _7062+_7058]).
% updated(happens(transfer(_7054,_7056,_7058),_7346,_7352),balance(_7054,_7062),_7062-_7064,[_7064 is _7062-_7058]).

statement([updated(happens(Action,_T1,_T2),Fluent1,_-B1, [B1 is Exp1]),
          updated(happens(Action,_T3,_T4),Fluent2,_-B2, [B2 is Exp2])]) -->
   when_, t_or_w(F,[], Map1), action(lambda([F,S,A],Action)),
          t_or_w(A, Map1, Map2), to_, t_or_w(S, Map2, Map3),
   then_, t_or_w(B1, Map3, Map4), that_, t_or_w(S, Map4, Map5),
        fluent(lambda([S,B1],Fluent1)), become, math_expression(Exp1, Map5),
   and_, t_or_w(B2, Map5, Map6), that_, t_or_w(F, Map6, Map7),
        fluent(lambda([F,B2],Fluent2)), become, math_expression(Exp2, Map7), period.

      % action(lambda(F,S,A,transfer(F,S,A))) --> [transfers].
      % fluent(lambda(S,B1,balance(S,B1))) --> [has].

      % In general, in the simplest case, a binary predicate pred(arg1, arg2) becomes
      % arg1 pred arg2. In that case, postconditions for a fluent arg1 pred arg2 are written in one of the three forms. The LPS
      % equivalents will be seen below.

      % When event(s) and conditions then arg1 pred arg2.
      % When event(s) and conditions then arg1 that pred arg2 becomes newarg1.
      % When event(s) and conditions then arg2 that arg1 pred becomes newarg2.

/* --------------------------------------------------- constraint statements */

/* It should be possible to identify a simpler more natural way of saying that there
can only be one value for a particular argument of a particular fluent at any given time.
Some of the constraints below or conditions above may be redundant.
*/
/* 	It must not be true that
	a voter votes for a first candidate in a ballot
	and the voter votes for a second candidate in the ballot
	and the first candidate is different from the second candidate.
*/
% false 	do(vote(Voter, Candidate1, Ballot) ),
% do(vote(Voter, Candidate2, Ballot) ),
% Candidate1 \= Candidate2.

statement([d_pre(Conditions)]) -->
   false_if_simult, simul_conjunction(_T, [], _Map, Conditions), period.

statement([d_pre(Conditions)]) -->
   false_if, conjunction([], _Map, Conditions), period.

false_if_simult --> [it, must, not, be, true, that, at, the, same, time].
false_if_simult  --> ['It', must, not, be, true, that, at, the, same, time].
false_if --> [it, must, not, be, true, that].
false_if --> ['It', must, not, be, true, that].

simul_conjunction(T, Map1, Map2, [C|CC]) --> simul_literal(T, Map1, Map3, C),
    and_, simul_conjunction(T, Map3, Map2, CC).
simul_conjunction(T, Map1, Map2, [C|CC]) --> simul_literal(T, Map1, Map3, C),
    comma, simul_conjunction(T, Map3, Map2, CC).
simul_conjunction(T, Map1, Map2, [C]) --> simul_literal(T, Map1, Map2, C).

simul_literal(T, Map1, Map2, holds(not(F),T)) --> literal(Map1, Map2, holds(not(F),T)).
simul_literal(T, Map1, Map2, holds(F,T)) --> literal(Map1, Map2, holds(F,T)).
% preconditions time T coincide with the first time of events
simul_literal(T, Map1, Map2, happens(A,T, T2)) -->
    literal(Map1, Map2, happens(A,T, T2)).
simul_literal(_, Map1, Map2, C, In, Out) :-
    % C \= holds(_,_), C \= happens(_,_,_),
    literal(Map1, Map2, C, In, Out).

/* 	It must not be true that at the same time
	a first voter delegates to a second voter in a ballot
	and the first voter delegates to a third voter in the ballot
	and the second voter is different from the third voter.
*/


% false 	do(delegate(Voter, Voter1, Ballot) ),
% do(delegate(Voter, Voter2, Ballot) ),
% Voter1 \= Voter2.

/* 	It must not be true that at the same time
a first voter votes for a candidate in a ballot
and a second voter votes for the candidate in the ballot
and the first voter is different from the second voter.
*/
% false 	do(vote(Voter1, Candidate, Ballot) ),
% do(vote(Voter2, Candidate, Ballot) ),
% Voter1 \= Voter2.

/* 	It must not be true that at the same time
	a first voter delegates to a second voter in a ballot
	and a third voter delegates to the second voter in the ballot
	and the first voter is different from the third voter.
*/
% false 	do(delegate(Voter1, Voter, Ballot) ),
% do(delegate(Voter2, Voter, Ballot)),
% Voter1 \= Voter2.

% false 	do(delegate(Voter1, Voter, Ballot) ),
% do(vote(Voter, _, Ballot)).

% false 	do(delegate(Voter1, Voter2, Ballot) ),
% representedBy(Voter2, Voter1).

% false 	do(vote(Voter,_, Ballot) ),
% voted(Voter, _, Ballot).

% false 	do(delegate(Voter1, Voter2, Ballot) ),
% do(delegate(Voter2, Voter1, Ballot) ).

/* ------------------------------------- if then (reactive_rules) statements */

statement([reactive_rule(Conditions, Conclusions)]) --> spypoint,
   if_, conjunction([], MapC, Conditions),
   then_, conjunction(MapC, _, Conclusions), period.

if_ --> ['If'].
if_ --> [if].

/* ------------------------------------------------------------ A if B rules */
%  if_rules ::= if_rule | if_rule if_rules
%  if_rule ::= intensional_rules | timeless_rule | event_rule | prolog_clause
%  timeless_rule ::= "l_timeless(" literal "," conjunction ")."
%  intensional_rules ::= "l_int(" literal "," conjunction ")."
%  event_rule ::= "l_events(" happens_literal "," hold_conjunction_list ")."
%  current nlen2lps implementation mixes timed and timeless literals!

statement([l_int(Literal,Conditions)]) -->
   literal([], Map1, Literal), if_, conjunction(Map1, _, Conditions), period,
   { Literal \= happens(_,_,_) }.

statement([l_events(Happens, Holds)]) -->
    happens_literal(_T1, _T2, [], Map1, Happens), if_,
    conjunction(Map1, _Map2, Holds), period. % <-- Check this! holds_conjunction?

happens_literal(T1, T2, MapIn, MapOut, happens(Action, T1, T2)) -->
    literal(MapIn, MapNext, happens(Action, T1, T2)),
    mapped_time_expression([T1, T2], MapNext, MapOut).

% do we need a T parameter to set precondition time?
holds_conjunction(T, Map1, Map2, [C|CC]) -->
    holds_literal(T, Map1, Map3, C), comma, holds_conjunction(T, Map3, Map2, CC).
holds_conjunction(T, Map1, Map2, [C|CC]) -->
    holds_literal(T, Map1, Map3, C), and_, holds_conjunction(T, Map3, Map2, CC).
holds_conjunction(T, Map1, Map2, [HL]) --> holds_literal(T, Map1, Map2, HL).

% setting T on holds!
holds_literal(T, Map1, Map2, holds(L, T)) -->
   literal(Map1, Map2, holds(L,T)).

holds_literal(_T, Map1, Map2, L) -->
    literal(Map1, Map2, L).

statement(_, Pos, _) :- asserterror('Found no statement at ', Pos), fail.

/* --------------------------------------------------------- Utils in Prolog */

ordinal(1,  'first').
ordinal(2,  'second').
ordinal(3,  'third').
ordinal(4,  'fourth').
ordinal(5,  'fifth').
ordinal(6,  'sixth').
ordinal(7,  'seventh').
ordinal(8,  'eighth').
ordinal(9,  'ninth').
ordinal(10, 'tenth').

% if it is a type is not a word. Using cut
t_or_w(S, InMap, OutMap, In, Out) :- type(S, InMap, OutMap, In, Out), !.
t_or_w(W, Map, Map, In, Out) :- word(W, In, Out).

% treating a list as one word for simplicity
word(L, ['['|R], RR) :- rebuilt_list(R, [], L, RR).
       % append(L, [']'|RR], R). % append(['['|W], [']'], L), !.
word(W, [P1, '_', P2|R], R) :- atomic_list_concat([P1, '_', P2], '', W), !.
word(W, [P1, '_', P2, '_', P3|R], R) :-
  atomic_list_concat([P1, '_', P2, '_', P3], '', W), !.
word(W, [W|R], R)  :- not(reserved_word(W)).
word(_, Pos, _) :- asserterror('No word at ', Pos), fail.

rebuilt_list([']'|RR], In, In, RR) :- !. % only the first ocurrence
rebuilt_list([','|RR], In, Out, NRR) :- !,
   rebuilt_list(RR, In, Out, NRR).
rebuilt_list([A|RR], In, [A|Out], NRR) :-
   rebuilt_list(RR, In, Out, NRR).

% 0 votes
type(N, Map, Map, [N, Type|Out], Out) :-
   number(N), is_a_type(Type).
% a number of votes
type(V, InMap,OutMap, [D, number, of, Type|Out], Out) :-
   ind_det(D),
   atomic_concat(number, Type, VarName),
   OutMap = [map(V,VarName)|InMap], !.
% the number of votes is a special case of anaphora
type(V, InMap,OutMap, [the, number, of, Type|Out], Out) :-
   atomic_concat(number, Type, VarName),
   ((member(map(V,VarName),InMap), OutMap = InMap);
    OutMap = [map(V,VarName)|InMap]), !.
type(V, InMap,OutMap, [D, Ordinal, Type|Out], Out) :-
   ind_det(D),
   ordinal(_, Ordinal),
   atomic_concat(Ordinal, Type, VarName),
   OutMap = [map(V,VarName)|InMap], !.
type(V, InMap,InMap, [the, Ordinal, Type|Out], Out) :-
   ordinal(_, Ordinal),
   atomic_concat(Ordinal, Type, VarName),
   member(map(V,VarName),InMap), !.
type(V, InMap,OutMap, [the, Ordinal, Type|Out], Out) :-
   ordinal(_, Ordinal),
   atomic_concat(Ordinal, Type, VarName),
   not(member(map(V, VarName), InMap)),
   OutMap = [map(V,VarName)|InMap]. % creates the var if it does not exist
type(V, InMap,OutMap, [D,Type|Out], Out) :- ind_det(D), OutMap = [map(V,Type)|InMap].
type(V, InMap,InMap, [the,Type|Out], Out) :- member(map(V,Type),InMap).
type(V, InMap,OutMap, [the,Type|Out], Out) :-  OutMap = [map(V,Type)|InMap]. % creates the var if it does not exist
type(_, In, In, Pos, _) :- asserterror('No type at ', Pos), fail.

is_a_type(T) :- % pending integration with wei2nlen:is_a_type/1
   atom(T),
   not(number(T)), not(punctuation(T)),
   not(reserved_word(T)).

ind_det(a).
ind_det(an).
ind_det(some).

def_det(the).

reserved_word(W) :- % more reserved words pending
  punctuation(W);
  W = 'is'; W ='not'; W='When'; W='when'; W='if'; W='If'; W='then';
  W = 'at'; W= 'from'; W='to'; W='and'; W='half'.

punctuation('.').
punctuation(',').
punctuation(';').
punctuation(':').

assertall([]).
assertall([F|R]) :-
    not(asserted(F)),
    assertz(F), !,
    % write('Asserting .. '), write(F), nl, nl,
    assertall(R).
assertall([_F|R]) :-
    % write(' Already there .. '), write(F), nl,
    assertall(R).

asserted(F :- B) :- clause(F, B). % as a rule with a body
asserted(F) :- clause(F,true). % as a fact

asserterror(Me, Pos) :-
   % (clause(error_at(_,_), _) -> retractall(error_at(_,_));true),
   asserta(error_at(Me, Pos)).

showerror :-
   findall((Me, W), (error_at(Me, Pos), first_words(W, Pos,_)), L),
   Lc = [_,_,_,_,_,_],
   append(Lc, _, L),
   print_all_sentences(Lc).

first_words([W1,W2,W3], [W1,W2,W3|R],R).
%first_words([W1,W2], [W1,W2|R],R).
%first_words([W1], [W1|R],R).

%   (clause(error_at(Me,Pos), _) ->
%        ( nl, nl, write('Error: '), writeq(Me), writeq(Pos), nl, nl)
%    ; nl, nl, writeln('No error reported')).

spypoint(A,A). % for debugging more easily

/* ---------------------------------------------------------- dictionaries */
dica([num_players, N], ['num_players', 'number'], [the, number, of, players, is, N]).
dica([sum, P, N], [sum, 'predicate', 'number'], [N, is, the, sum, of, all, P]).
dica([voteCount, A, B], [voteCount, 'candidate', 'votes'], [A, has, B, votes]).
dica([transfers, A, B, C], [transfers, 'person', 'amount', 'person'], [A, transfers, B, to, C]).
dica(Plantiff, Model, Plantiff) :-  % No clue, no hassle. Use predicate estructure
   nonvar(Plantiff),
   make_model(Plantiff, Model).

make_model([], []).
make_model([H|R], [H|L]) :-
   nonvar(H), make_model(R,L).
make_model([H|R], ['object'|L]) :-
   var(H), make_model(R,L).


/* ---------------------------------------------------------------- parser */
parser(true).
parser(Builtin) :-
    not(predicate_property(Builtin, imported_from(nlen2lps))), call(Builtin).
parser(Sem_Goal) :-
    predicate_property(Sem_Goal, imported_from(nlen2lps)),
    findall(Body, clause(Sem_Goal, Body), Bodies),
    parser(Sem_Goal, Bodies).
parser((A,B)) :-
    parser(A), parser(B).
parser((A;B)) :- parser(A);parser(B).
parser(_,[H|_R]) :- parser(H).
parser(SG,[_H|R]) :- parser(SG, R).
parser(SG,[]) :- asserterror('Failed at', SG), false.


/* -------------------------------------------------------- APIs Extensions */

sum_all(P, N) :-
   findall(_, P, L), length(L, N).
