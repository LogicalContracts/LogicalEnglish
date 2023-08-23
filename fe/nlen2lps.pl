% Copyright (c) 2017, 2018. Logical Contracts. See terms below
% Written by Jacinto Dávila
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
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
% 1. Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
%
% 2. Redistributions in binary form must reproduce the above copyright
% notice, this list of conditions and the following disclaimer in the
% documentation and/or other materials provided with the distribution.
%
% 3. Neither the name of the copyright holder nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
% IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
:- discontiguous statement/3, declaration/4, fluent/3, action/3, literal/5.

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

settings(AllR, AllS) --> %spypoint, 
	declaration(Rules,Setting), settings(RRules, RS),
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
math_expression(V1 * V2, Map) --> type(V1, Map, Map0), times, type(V2, Map0, _Map1).
math_expression(N, _Map) --> word(N), {number(N)}.

plus --> [plus].

minus --> [minus].

times --> [times].

declaration([],[maxTime(M)]) --> maximum_previous, word(M), period, {integer(M)}.

maximum_previous --> [the, maximum, time, 'is'].

/* ---------------------------------------------------- fluent declarations */
declaration(Rules, [fluents(Fluents)]) --> % {writeln('stop and trace'), trace},
    fluent_previous, list_of_fluents_decl(Rules, Fluents).

initially_ --> [initially, ':'].

fluent_previous --> [the, fluents, are, ':'].

list_of_fluents_decl([Ru|R1], [F|R2]) --> fluent_decl(Ru,F), list_of_fluents_decl(R1,R2).
list_of_fluents_decl([R],[F]) --> fluent_decl(R, F).

% the game is over, known as gameOver % corrected on 2023-06-23
fluent_decl(fluent(lambda([], Fluent), [is, over|R], R), Fluent) -->
   type(_X, [], _), is_, word(over), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N]}.

% added on 2023-06-23
% a person is alive, known as alive.
fluent_decl(fluent(lambda([X], Fluent), [is, W|R], R), Fluent) -->
   type(X, [], _), is_, word(W), comma, known_as, word(N),
   comma_or_period, {Fluent =.. [N,X]}.
   
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
   
% added on 2023-06-22
% a vehicle is at a place heading a direction, known as location,
fluent_decl(fluent(lambda([X,Y,B], Fluent), [is, at|R], R), Fluent) --> 
   type(X, [], _), is_, at_, type(Y, [], _), word(heading), type(B, [],_),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y,B]}.
   
% added on 2023-06-22
% there is a collision warning between a first vehicle and a second vehicle at a place, known as collisionAlert,
fluent_decl(fluent(lambda([X,Y,B], Fluent), [is, a, collision|R], R), Fluent) --> 
   word(there), is_, word(a), word(collision), word(warning), word(between), 
   type(X, [], _), and_, type(Y, [], _), at_, type(B, [],_),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y,B]}.

% added on 2023-06-22
% a first vehicle is to the right of a second vehicle at a place, known as toTheRight,
fluent_decl(fluent(lambda([X,Y,B], Fluent), [is, to|R], R), Fluent) --> 
   type(X, [], _), is_, to_, word(the), word(right), word(of), type(Y, [], _), at_, type(B, [],_),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y,B]}.
   
% added on 2023-06-22
% there is a possible collision between a first vehicle and a second vehicle, known as possibleCollision.
fluent_decl(fluent(lambda([X,Y], Fluent), [is, a, possible|R], R), Fluent) -->  
   word(there), is_, word(a), word(possible), word(collision), word(between), 
   type(X, [], _), and_, type(Y, [], _),
   comma, known_as, word(N), comma_or_period, {Fluent =.. [N,X,Y]}.
   
fluent_decl(_, _, Pos, _) :- asserterror('Found no fluent declaration at ', Pos), fail.

/* --------------------------------------------- undeclared external fluents */

% e_getBalance the address latest a value
external_fluent(lambda([A, L, V], Fluent)) --> word(E), {E\=a, E\=an, Fluent =.. [E, A, L, V]}.

% e_existsTransactionReceipt the payment
external_fluent(lambda([P], Fluent)) --> word(E), {E\=a, E\=an, Fluent =.. [E, P]}.

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

%added on 2023-06-26
% an animal sings,
action_decl(action(lambda([X], Action), [V|R], R), Action) -->
   type(X, [], _), word(V),
   comma_or_period, {Action =.. [V,X]}.

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

%added on 2023-06-22
% a vehicle starts driving to a place,
action_decl(action(lambda([X,Y], Action), [starts, V|R], R), Action) -->
   type(X, [], _), word(starts), word(V), to_,  type(Y, [], _),
   comma_or_period, {Action =.. [V,X,Y]}.
   
%added on 2023-06-22
% a vehicle is removed.  
action_decl(action(lambda([X], Action), [is, V|R], R), Action) -->
   type(X, [], _), is_, word(V),
   comma_or_period, {Action =.. [V,X]}.
   
%added on 2023-06-22
% a vehicle steps from an old place to a new place.
action_decl(action(lambda([X,Y,Z], Action), [V|R], R), Action) --> 
   type(X, [], _), word(V), from_,  type(Y, [], _), to_,  type(Z, [], _),
   comma_or_period, {Action =.. [V,X,Y,Z]}.   
   
%added on 2023-06-24
%  a vehicle turns to a direction.
action_decl(action(lambda([X,Y], Action), [V|R], R), Action) --> 
   type(X, [], _), word(V), to_,  type(Y, [], _),
   comma_or_period, {Action =.. [V,X,Y]}.
      
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

% voter1 delegates to voter2 ;
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

list_of_obs([E|RE], T2) --> %spypoint, 
    obs_decl(E), and_, list_of_obs(RE, T2).
list_of_obs([E], T2) --> obs_decl(E), time_expression([_T1, T2]).


/* ---------------------------------------------------------------- obs_decl */
% adjusting to Wei syntax in Miguel s psyntax.P
% notice that observations are statements, not declarations.
% this version, no variables allowed.

%added on 2023-06-22
% vehicle is removed
obs_decl(Action) -->
    word(X), action(lambda([X], Action)).  

%added on 2023-06-22 - modified by Jacinto on 2023-07-30
% vehicle starts driving to place
obs_decl(Action) --> %spypoint,
    word(X), action(lambda([X,Y], Action)), to_,  t_or_w(Y, [], _).
    
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

% times 20 and plus 15
arithmetic_expression(lambda([C1, C2, N1, N2], C1 is C2 * N1 + N2)) -->
shift_(N1, N2).

approximatelly_(N) --> [approximately, N, times], {number(N)}.
shift_(N1, N2) --> [times, N1], {number(N1)}, [plus, N2], {number(N2)}.
% shift_(N1, N2) --> [times, N1, plus, N2], {number(N1)}, {number(N2)}.

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
% added on 2023-06-24
% a vehicle steps from a first place to a second place from a first time to a second time
literal(M_In,M_Out, happens(Action, T1, T2)) --> %spypoint,
    t_or_w(V, M_In, M2), action(lambda([V,P1,P2], Action)), from_, 
    t_or_w(P1, M2, M3), to_, t_or_w(P2, M3, M4),
    mapped_time_expression([T1,T2], M4, M_Out).
    
% added on 2023-06-24
% a vehicle turns to a direction from a first time to the second time
literal(M_In,M_Out, happens(Action, T1, T2)) --> %spypoint,
    t_or_w(V, M_In, M2), action(lambda([V,D], Action)), to_, 
    t_or_w(D, M2, M3), 
    mapped_time_expression([T1,T2], M3, M_Out).    

% added on 2023-06-22
% edited on 2023-08-04
% a vehicle drives to a place through a route from a first time to a second time
%literal(M_In,M_Out, happens(Action, T1, T2)) --> %spypoint,
%    t_or_w(V, M_In, M2), word(A), to_, :- discontiguous nlen2lps:literal/5.
%    t_or_w(P, M2, M3), word(through), t_or_w(R, M3, M4),
%    mapped_time_expression([T1,T2], M4, M_Out), {Action=..[A,V,P,R]}.
literal(M_In,M_Out, happens(Action, T1, T2)) --> %spypoint,
    t_or_w(V, M_In, M2), word(A), to_, 
    t_or_w(P, M2, M3), word(through), t_or_w(R, M3, M4),
    mapped_time_expression([T1,T2], M4, M_Out), {Action=..[A,V,R,P]}.

% added on 2023-06-22; deleted on 2023-06-24
% there is a route from a first place to a second place at a time
% literal(M_In,M_Out, holds(Fluent, T)) -->
%    word(there), is_, t_or_w(R, M_In, M1), from_, t_or_w(P1, M1, M2), to_, t_or_w(P2, M2, M3), 
%    at_, t_or_w(T, M3, M_Out), {Fluent=..[directions, P1, R, P2]}.

% added on 2023-06-22
% a vehicle is at a place heading a direction at a  time
literal(M_In,M_Out, holds(Fluent, T)) -->
    t_or_w(V, M_In, M2), fluent(lambda([V,P,D], Fluent)),
    t_or_w(P, M2, M3), word(heading), t_or_w(D, M3, M4), at_, t_or_w(T, M4, M_Out).

% it is not the case that the player has played a second choice from ..
literal(M_In,M_Out, holds(not(Fluent), T)) --> %  
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
% is equal to is: a meta predicate to transit from timed to timeless
% added on 2023-08-12 by Jacinto    
% the object is equal to mycar is at [a X, a Y] heading a direction at a time
literal(M_In,M_Out, Cond) --> %spypoint, 
    t_or_w(O, M_In, M2), is_, equal_, to_, literal(M2, M_Out, TimedLiteral),
    {TimedLiteral = holds(F,_), Cond = (O = F)}. 
    
% added on 2023-08-12 by Jacinto
% a form has type rectangle from [XX,YY] to [XX2, YY2] fillColor yellow
literal(M_In,M_Out, Timeless) --> %spypoint, 
    t_or_w(S, M_In, M1), word(has), word(type), t_or_w(Type, M1, M2), from_, t_or_w(From, M2, M3),
    to_, t_or_w(To, M3, M4), fillColor_, t_or_w(Color, M4, M_Out), 
    {Timeless=(S=[type:Type, from:From, to:To, fillColor:Color])}.   
    
fillColor_ --> [fillColor].   

% added on 2023-08-12 by Jacinto
% a setting has type ellipse point [a XX, a YY] size [a Xcar, a Ycar] fillColor blue
literal(M_In,M_Out, Timeless) -->
    t_or_w(S, M_In, M1), word(has), word(type), t_or_w(Type, M1, M2), word(point), t_or_w(Point, M2, M3),
    word(size), t_or_w(Size, M3, M4), fillColor_, t_or_w(Color, M4, M_Out), 
    {Timeless=(S=[type:Type, point:Point, size:Size, fillColor:Color])}.
    
% added on 2023-08-13 by Jacinto
% the setting has type circle center [a XX2, a YY2] radius 10 fillColor red 
literal(M_In,M_Out, Timeless) --> 
    t_or_w(S, M_In, M1), word(has), word(type), t_or_w(Type, M1, M2), word(center), t_or_w(Point, M2, M3),
    word(radius), t_or_w(Radius, M3, M4), fillColor_, t_or_w(Color, M4, M_Out), 
    {Timeless=(S=[type:Type, center:Point, radius:Radius, fillColor:Color])}.   

% added on 2023-06-25
% a route starts from a street with a direction
literal(M_In,M_Out, Timeless) -->
    t_or_w(R, M_In, M1), word(starts), from_, t_or_w(S, M1, M2), with_, t_or_w(D, M2, M_Out), 
    {Timeless=..[starts, R, S, D]}.

% added on 2023-06-25
% a first place is next to a second place along a direction
literal(M_In,M_Out, Timeless) -->
    t_or_w(F, M_In, M1), is_, word(next), to_, t_or_w(N, M1, M2), word(along), t_or_w(D, M2, M_Out), 
    {Timeless=..[next, F, D, N]}.
    
% added on 2023-06-25
% a place is on a street
literal(M_In,M_Out, Timeless) --> %spypoint,
    t_or_w(P, M_In, M1), is_, word(on), t_or_w(D, M1, M_Out), 
    {Timeless=..[on, P, D]}.
    
% added on 2023-06-25
% a route has as second a street with a direction
literal(M_In,M_Out, Timeless) -->
    t_or_w(R, M_In, M1), word(has), word(as), word(second), t_or_w(S, M1, M2), with_, t_or_w(D, M2, M_Out), 
    {Timeless=..[has_second, R, S, D]}.
    
% added on 2023-06-25
% a route has as tail a second route
literal(M_In,M_Out, Timeless) --> %spypoint
    t_or_w(R, M_In, M1), word(has), word(as), word(tail), t_or_w(S, M1, M_Out), 
    {Timeless=..[has_tail, R, S]}.    

% added on 2023-06-24
% there is a route from a first place to a second place at a time
literal(M_In,M_Out, Timeless) --> 
    word(there), is_, t_or_w(R, M_In, M1), from_, t_or_w(P1, M1, M2), to_, t_or_w(P2, M2, M_Out), 
    {Timeless=..[directions, P1, R, P2]}.

% there is a collision warning between a first vehicle and a second vehicle at a third place at a first time.
literal(M_In, M_Out, holds(Fluent, T)) --> %spypoint,
     word(there), is_, word(a), word(collision), word(warning), word(between), t_or_w(V1, M_In, M1), and_, t_or_w(V2, M1, M2),
     at_, t_or_w(P, M2, M3), at_, t_or_w(T, M3, M_Out), {Fluent=..[collisionWarning, V1, V2, P]}.

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
literal(M_In,M_Out, Expression) --> %spypoint,
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

equal_ --> [equal]. 

/* ------------------------------------------------------ observe statements */

statement(Obs) --> obs_previous, 
	list_of_obs_stat(Obs).

/* ---------------------------------------------------- initially statements */
statement([initial_state(States)]) --> 
    initially_, list_of_init(States), period.

list_of_init([E|RE]) -->
    state(E), and_, list_of_init(RE).
list_of_init([E]) --> state(E).

% the reward is 1
state(Fluent) -->
   type(_X, [], _), fluent(lambda([Y], Fluent)), word(Y).

% added 2023-06-22  - modified on 2023-07-30 
% mycar is at 2-1 heading northward
state(Fluent) --> 
   word(C), fluent(lambda([C,A,H], Fluent)), t_or_w(A, [],_), word(heading), word(H).
   
% added 2023-06-23
% socrates is alive.
state(Fluent) --> 
   word(C), fluent(lambda([C], Fluent)).

/* --------------------------------------------------------- prolog staments */
statement([Prolog]) --> prolog_fact(Prolog).

prolog_fact(Prolog) -->  % binary predicate
    t_or_w(X, [], M2), word(Predicate), t_or_w(Y, M2, _M_Out), period,
    {Prolog =.. [Predicate, X, Y]}.

/* --------------------------------------------------------- When statements */
/* 
When a vehicle steps from a first place to a second place
and the vehicle is at the first place heading an old direction
then the vehicle is at the second place heading the old direction
*/
statement([initiated(happens(A,T1,_T2),F, Conds)]) --> %spypoint, 
   when_, t_or_w(V, [], Map2), action(lambda([V,P1,P2],A)), from_, t_or_w(P1,  Map2, Map3), to_, t_or_w(P2, Map3, Map4), 
   and_, simul_conjunction(T1, Map4, Map5, Conds),
   then_, t_or_w(V, Map5, Map6), fluent(lambda([V,P2,D],F)), t_or_w(P2, Map6, Map7), word(heading), t_or_w(D, Map7, _), period.


% added on 2023-06-23
% When a person is born 
% then the person is alive.
statement([initiated(happens(A,_T1,_T2),F, [])]) -->
   when_, t_or_w(P, [], Map1), action(lambda([P],A)), 
   then_, t_or_w(P, Map1, _Map2), fluent(lambda([P],F)), period.

% added on 2023-06-22
/* When a vehicle is removed
   then it becomes no longer the case that the vehicle is at a place heading a direction 
*/
statement([terminated(happens(A,_T1,_T2),F, [])]) -->
   when_,  t_or_w(V, [], Map2), action(lambda([V],A)),
   then_, it_becomes_no_longer_the_case_that_, t_or_w(V, Map2, Map3), fluent(lambda([V,P,D],F)), t_or_w(P, Map3, Map4), word(heading), t_or_w(D, Map4, _Map5), period.

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

% added on 2023-06-25
/* When a vehicle steps from a first place to a second place
then the first place that the vehicle is at the first place heading a direction becomes the second place. 
*/
statement([updated(happens(Action,_T1,_T2),Fluent,P1-Y,[Y = P2])]) --> %spypoint, 
   when_, t_or_w(V,[], Map1), action(lambda([V,P1,P2],Action)), from_, t_or_w(P1, Map1, Map2),
        to_, t_or_w(P2, Map2, Map3),
   then_, t_or_w(P1, Map3, Map4), that_, t_or_w(V, Map4, Map5), fluent(lambda([V,P1,D], Fluent)), t_or_w(P1, Map5, Map6), 
   word(heading), t_or_w(D, Map6, Map7),
   become, t_or_w(P2, Map7, _Map8), period.
   
% added on 2023-06-25
/* When a vehicle turns to a second direction
then the first direction that the vehicle is at a place heading the first direction becomes the second direction. 
*/
statement([updated(happens(Action,_T1,_T2),Fluent,D1-Y,[Y = D2])]) --> %spypoint, 
   when_, t_or_w(V,[], Map1), action(lambda([V,D2],Action)), to_, t_or_w(D2, Map1, Map2),
   then_, t_or_w(D1, Map2, Map3), that_, t_or_w(V, Map3, Map4), fluent(lambda([V,P,D1], Fluent)), t_or_w(P, Map4, Map5), 
   word(heading), t_or_w(D1, Map5, Map6),
   become, t_or_w(D2, Map6, _Map7), period.

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

% This is the case where a delegate s representative has not yet voted.
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
it_becomes_no_longer_the_case_that_ --> [it, becomes, no, longer, the, case, that]. 

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

statement([reactive_rule(Conditions, Conclusions)]) --> %spypoint,
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

statement([l_int(Literal,[])]) --> %spypoint,
   literal([], _Map1, Literal), { Literal = holds(_,_) }, 
   period.

statement([l_int(Literal,Conditions)]) --> %spypoint,
   literal([], Map1, Literal), { Literal = holds(_,_) }, 
   if_, conjunction(Map1, _, Conditions), period.

statement([l_events(Happens, Holds)]) --> %spypoint, 
    happens_literal(_T1, _T2, [], Map1, Happens), if_,
    conjunction(Map1, _Map2, Holds), period. % <-- Check this! holds_conjunction?the object is equal to mycar is at [a X, a Y] heading a direction

statement([Literal]) --> %spypoint,
   literal([], _Map1, Literal), period,
   { Literal \= happens(_,_,_), Literal \= holds(_,_) }.
   
% added on 2023-08-12 by Jacinto for:
% display an object with a setting if
%		the object is equal to mycar is at [a X, a Y] heading a direction
%		and the setting has type ellipse point [a XX, a YY] size [a Xcar, a Ycar] fillColor blue
%		and point at the X and the Y heading the direction is mapped to the XX and the YY with horizontal size the Xcar and vertical size the Ycar.
statement( [(display(O,S) :- Body)]) --> %spypoint, 
    display_, t_or_w(O,[], Map1), with_, t_or_w(S,Map1, Map2), if_, conjunction(Map2, _, Conditions), period,
   { Conditions \= [], list2and(Conditions, Body) }. 
   
% added on 2023-08-12 by Jacinto for:
% display timeless with a background if
%		for every 
%			[a X, a Y] is defined as a place
%			and it is not the case that [X,Y] is on a street
%			and a XX is X times 20
%			and a YY is X times 20
%			and a XX2 is XX plus 20
%			and a YY2 is YY plus 20
%		with
%			a form has type rectangle from [XX,YY] to [XX2, YY2] fillColor yellow
%		add the form to the background.    
statement( [(display(O,S) :- findall(Structure, Body, S))] ) --> %spypoint, 
    display_, t_or_w(O, [], Map1), with_, t_or_w(B, Map1, Map2), if_, for_every_, conjunction(Map2, Map3, Conditions), 
    with_, literal(Map3, Map4, (F=Structure)), add_, t_or_w(F, Map4, Map5), to_, t_or_w(B, Map5, _Map6), period,  
   { Conditions \= [], list2and(Conditions, Body) }.    

for_every_ --> [for, every]. 
add_ --> [add]. 

statement([(Literal :- Body)]) --> %spypoint,
   literal([], Map1, Literal), if_, conjunction(Map1, _, Conditions), period,
   { Literal \= happens(_,_,_), Literal \= holds(_,_), Conditions \= [], list2and(Conditions, Body) }.
   
list2and([Cond], Cond) :- !. 
list2and([Cond|Rest], (Cond, ARest)) :- list2and(Rest, ARest).      

happens_literal(T1, T2, MapIn, MapOut, happens(Action, T1, T2)) -->
    literal(MapIn, MapOut, happens(Action, T1, T2)). 
    %mapped_time_expression([T1, T2], MapNext, MapOut). % excluded on 2023-06-25

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

display_ --> [display].
with_ --> [with]. 

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

ordinal(11,  'old').
ordinal(12,  'next').
ordinal(13, 'new').
ordinal(14, 'present').

% if it is a type is not a word. Using cut
t_or_w(S, InMap, OutMap, In, Out) :- type(S, InMap, OutMap, In, Out), !.
t_or_w(S, InMap, OutMap, In, Out) :- list(S, InMap, OutMap, In, Out), !. % Added by Jacinto on 2023-07-30
t_or_w(W, Map, Map, In, Out) :- word(W, In, Out).

% treating a list as one word for simplicity
%word(L, ['['|R], RR) :- rebuilt_list(R, [], L, RR).
       % append(L, [']'|RR], R). % append(['['|W], [']'], L), !.
word(W, [P1, '_', P2|R], R) :- atomic_list_concat([P1, '_', P2], '', W), !.
word(W, [P1, '_', P2, '_', P3|R], R) :-
  atomic_list_concat([P1, '_', P2, '_', P3], '', W), !.
word(W, [W|R], R)  :- not(reserved_word(W)).
word(_, Pos, _) :- asserterror('No word at ', Pos), fail.

rebuilt_list([']'|RR], Map, Map, In, In, RR) :- !. % only the first ocurrence
rebuilt_list([','|RR], InMap, OutMap, In, Out, NRR) :- !,
   rebuilt_list(RR, InMap, OutMap, In, Out, NRR).
rebuilt_list(InS, InMap, OutMap, In, [A|Out], NRR) :-
   t_or_w(A, InMap, NextMap, InS, RR), 
   rebuilt_list(RR, NextMap, OutMap, In, Out, NRR).

% lists
list(L, InMap, OutMap, ['['|R], RR) :- rebuilt_list(R, InMap, OutMap, [], L, RR).

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
% added by Jacinto on 2023-07-30 to partially handle symbolic variables
type(V, InMap,InMap, [Type|Out], Out) :- member(map(V,Type),InMap).
% modified by Jacinto on 2023-07-30 removing the following to prevent for using the on unknown variables
% uncommented again on 2023-08-23 to allow for the use of "the" to introduce a new variable, as required by earlier examples. 
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
  W = 'is'; W ='not'; W='When'; W='when'; W='if'; W='If'; W='then'; W='with'; W='fillColor'; 
  W = 'at'; W= 'from'; W='to'; W='and'; W='half'; W = 'plus'; W = 'minus'; W = 'times'; W='display'.

punctuation('.').
punctuation(',').
punctuation(';').
punctuation(':').
punctuation(']').  % Jacinto adding [ and ] as punctuation on 2023-07-29
punctuation('['). 

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

/* -------------------------------------------------------- New productions for driverlesscar */

% the head of the route is the street
% the second of the route is the direction. 
literal(M_In, M_Out, Timeless) --> %spypoint,
    word(the), word(P), word(of), t_or_w(R, M_In, M2), is_, t_or_w(D, M2, M_Out),
    {Timeless=..[part, P, R, D]}.

head([H|_], H).
second([_,S|_], S).
part(head, [H|_], H).

% timeless literals
% a direction is horizontal 
%literal(M_In, M_Out, Timeless) --> %spypoint,
%     t_or_w(D, M_In, M1), is_, t_or_w(H, M1, M_Out), 
%     {Timeless=..[is, D, H]}.

/*
literal(M_In, M_Out, Timeless) --> %spypoint,
     t_or_w(D, M_In, M_Out), is_, word(H), 
     {Timeless=..[is, D, H]}.
*/

% there is a clash between a first direction and a second direction
literal(M_In, M_Out, Timeless) --> %spypoint,
     word(there), is_, word(a), word(clash), word(between), t_or_w(D1, M_In, M1), and_, t_or_w(D2, M1, M_Out),
     {Timeless=..[clash, D1, D2]}.

% appending a first route followed by a second route gives a third route
literal(M_In, M_Out, Timeless) --> %spypoint,
   word(appending), t_or_w(R1, M_In, M1), word(followed), word(by), t_or_w(R2, M1, M2),
   word(gives), t_or_w(R3, M2, M_Out), {Timeless=..[append, R1, R2, R3]}.

% a first route has as second a second route
literal(M_In,M_Out, Timeless) --> %spypoint,
    t_or_w(R1, M_In, M1), word(has), word(as), word(second), t_or_w(R2, M1, M_Out), 
    {Timeless=..[second_route, R1, R2]}.

% added on 2023-06-25
% a route has as second a street with a direction
% literal(M_In,M_Out, Timeless) -->
%    t_or_w(R, M_In, M1), word(has), word(as), word(second), t_or_w(S, M1, M2), with_, t_or_w(D, M2, M_Out), 
%    {Timeless=..[has_second, R, S, D]}.

% edited on 2023-08-04
% a new route is from a first place to a second place
literal(M_In,M_Out, Timeless) --> %spypoint,
    t_or_w(R, M_In, M1), is_, from_, t_or_w(P1, M1, M2), to_, t_or_w(P2, M2, M_Out),
    {Timeless=..[directions, P1, R,  P2]}.

% appending the second route followed by a second street with a second direction gives the new route
literal(M_In,M_Out, Timeless) --> %spypoint,
   word(appending), t_or_w(R1, M_In, M1), word(followed), word(by), t_or_w(S, M1, M2), with_, t_or_w(D, M2, M3),
   word(gives), t_or_w(R2, M3, M_Out), {Timeless=..[append, R1, S, D, R2]}.

% the orientation from [X, Y1] to [X, Y2] is northward.
literal(M_In, M_Out, Timeless) --> %spypoint,
    word(the), word(orientation), from_, t_or_w(P1, M_In, M1), to_, t_or_w(P2, M1, M2), is_, t_or_w(H, M2, M_Out),
    {Timeless=..[orientation, P1, P2, H]}.

% modified on 2023-07-27 by Jacinto ----
% the following rules can all be subsumed by the first
% a first number < a second number
literal(M_In, M_Out, Expression) --> %spypoint,
   t_or_w(C1, M_In, M1), arithmetic_expression(lambda([C1, C2], Expression)), t_or_w(C2, M1, M_Out).

% a first number is less than a second number
% literal(M_In, M_Out, C1 < C2) --> %spypoint,
%   t_or_w(C1, M_In, M1), is_, word(less), word(than), t_or_w(C2, M1, M_Out).

% a first number > a second member
%literal(M_In, M_Out, Expression) --> %spypoint,
%   t_or_w(C1, M_In, M1), arithmetic_expression(lambda([C1, C2], C1 > C2)), t_or_w(C2, M1, M_Out).

% a value =< a number
%literal(M_In, M_Out, Expression) --> %spypoint,
%   t_or_w(C1, M_In, M1), arithmetic_expression(lambda([C1, C2], C1 =< C2)), t_or_w(C2, M1, M_Out).

% a value >= a number
%literal(M_In, M_Out, Expression) --> %spypoint,
%   t_or_w(C1, M_In, M1), arithmetic_expression(lambda([C1, C2], C1 >= C2)), t_or_w(C2, M1, M_Out).
% end of modifications on 2023-07-27 by Jacinto

% a first number is greater or equal than a second number
% literal(M_In, M_Out, C1 >= C2) --> %spypoint,
%   t_or_w(C1, M_In, M1), is_, word(greater), word(or), word(equal), word(than), t_or_w(C2, M1, M_Out).

% a first number is equal or less than a second number
% literal(M_In, M_Out, C1 =< C2) --> %spypoint,
%   t_or_w(C1, M_In, M1), is_, word(equal), word(or), word(less), word(than), t_or_w(C2, M1, M_Out).

% modified on 2023-07-27 by Jacinto (subsumed as above)
% a first number is greater than a second number
%literal(M_In, M_Out, C1 > C2) --> %spypoint,
%   t_or_w(C1, M_In, M1), is_, word(greater), word(than), t_or_w(C2, M1, M_Out).

% a first number is less than a second number
% literal(M_In, M_Out, C1 < C2) --> %spypoint,
%   t_or_w(C1, M_In, M1), is_, word(less), word(than), t_or_w(C2, M1, M_Out).

% section moved here on 2023-08-13 by Jacinto
% if two productions share the starting elements, put the longest first. 
% added by YB on 2023-08-08
% a first number is a second number times a third number plus a fourth number.
literal(M_In, M_Out, Y1 is Y2 * Y3 + Y4) --> %spypoint,
    t_or_w(Y1, M_In, M1), is_, t_or_w(Y2, M1, M2), times, t_or_w(Y3, M2, M3), plus, t_or_w(Y4, M3, M_Out).

% added by YB on 2023-08-08
% a XX is a X times 20.
literal(M_In, M_Out, C1 is C2 * C3) --> %spypoint,
   t_or_w(C1, M_In, M1), is_, t_or_w(C2, M1, M2), times, t_or_w(C3, M2, M_Out).

/*
% a XX is a X times 20 plus 15.
literal(M_In, M_Out, Expression) --> %spypoint,
   t_or_w(C1, M_In, M1), is_, t_or_w(C2, M1, M2), arithmetic_expression(lambda([C1, C2, N1, N2], Expression)),
    t_or_w(N1, M2, M3), t_or_w(N2, M3, M_Out).
*/


% a first number is a second number plus a third number.
literal(M_In, M_Out, Y1 is Y2 + Y3) --> %spypoint,
    t_or_w(Y1, M_In, M1), is_, t_or_w(Y2, M1, M2), plus, t_or_w(Y3, M2, M_Out).

% a first number is a second number minus a third number.
literal(M_In, M_Out, Y1 is Y2 - Y3) --> %spypoint,
    t_or_w(Y1, M_In, M1), is_, t_or_w(Y2, M1, M2), minus, t_or_w(Y3, M2, M_Out).

% a first number is a second member
literal(M_In, M_Out, C1 = C2) --> %spypoint,
   t_or_w(C1, M_In, M1), is_, t_or_w(C2, M1, M_Out).

% arithmetic_expression(lambda([C1, N, C2], C1 is (N + C2))) --> addition(N).

% addtion(X) --> [X, '+'].

% addition(X)-->t_or_w(X,In,Out),plus.

% deleted on 2023-08-04
% a street with a direction is a member of a route
% literal(M_In, M_Out, Timeless) --> %spypoint,
%   t_or_w(S, M_In, M1), with_, t_or_w(D, M1, M2), is_, word(a), word(member), word(of), t_or_w(R, M2, M_Out),
%   {Timeless=..[member, S, D, R]}.

% added on 2023-08-04
% the street2 is a member of a route
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(S, M_In, M1), is_, word(a), word(member), word(of), t_or_w(R, M1, M_Out),
   {Timeless=..[member, S, R]}.

% the horizontal coordinate of the first place is a first number
literal(M_In, M_Out, Timeless) --> %spypoint,
   word(the), word(horizontal), word(coordinate), word(of), t_or_w(P, M_In, M1), is_, t_or_w(C, M1, M_Out), 
   {Timeless=..[x_coordinate, P, C]}.

% the vertical coordinate of the first place is a first number
literal(M_In, M_Out, Timeless) --> %spypoint,
   word(the), word(vertical), word(coordinate), word(of), t_or_w(P, M_In, M1), is_, t_or_w(C, M1, M_Out), 
   {Timeless=..[y_coordinate, P, C]}.


% there is a possible collision between a first vehicle and a second vehicle at a time
literal(M_In,M_Out, holds(Fluent, T)) -->
    word(there), is_, word(a), word(possible), word(collision), word(between), t_or_w(V1, M_In, M1), and_, t_or_w(V2, M1, M2), 
    at_, t_or_w(T, M2, M_Out), {Fluent=..[collisionPossible, V1, V2]}.


% a first direction is opposite to a second direction.
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(D1, M_In, M1), is_, word(opposite), to_, t_or_w(D2, M1, M_Out), {Timeless=..[opposite, D1, D2]}.


% a first vehicle is to the right of a second vehicle at a place at a time
literal(M_In,M_Out, holds(Fluent, T)) --> %spypoint,
   t_or_w(V1, M_In, M1), is_, to_, word(the), word(right), word(of), t_or_w(V2, M1, M2), at_, t_or_w(P, M2, M3),
   at_, t_or_w(T, M3, M_Out),
   {Fluent=..[rightOfWay, V1, V2, P]}.

% modified by Jacinto on 2023-07-27
% a place is the priority Tjunction of a street
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(P, M_In, M1), is_, word(the), word(priority), word('Tjunction'), word(of), t_or_w(S, M1, M_Out),
   {Timeless=..[priorityTjunction, P, S]}.

% a place is a cross of roads
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(P, M_In, M_Out), is_, word(a), word(cross), word(of), word(roads),
   {Timeless=..[crossRoads, P]}.

% a first direction is on the right of a second direction
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(D1, M_In, M1), is_, word(on), word(the), word(right), word(of), t_or_w(D2, M1, M_Out),
   {Timeless=..[rightOf, D1, D2]}.

% corrected by Jacinto on 2023-07-27
% a first number and a second number makes a pair fit for a place 
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(C1, M_In, M1), and_, t_or_w(C2, M1, M_Out), word(makes), word(a), word(pair), word(fit), word(for), word(a), word(place),
   {Timeless=..[place, C1, C2]}.

%  a first list followed by a second list gives a new list
literal(M_In,M_Out, Timeless) --> %spypoint,
   t_or_w(L1, M_In, M1), word(followed), word(by), t_or_w(L2, M1, M2),
   word(gives), t_or_w(L3, M2, M_Out), {Timeless=..[append, L1, L2, L3]}.

% [a street1, a direction1] is a route.
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(C1, M_In, M_Out), is_, word(a), word(route),
   {Timeless=..[is_route, C1]}.

% added on 2023-08-04
% [a street1, a direction1] is a route from a start to a finish.
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(C1, M_In, M1), is_, word(a), word(route), from_, t_or_w(P1, M1, M2), to_, t_or_w(P2, M2, M_Out),
   {Timeless=..[route, C1, P1, P2]}.

% added on 2023-08-04
/* When a vehicle steps from an old place to a next place
   then it becomes no longer the case that the vehicle is at the old place heading a direction 
*/
statement([terminated(happens(Action,_T1,_T2),F, [])]) --> %spypoint,
   when_,  t_or_w(V, [], Map1), action(lambda([V,P1,P2],Action)), from_, t_or_w(P1, Map1, Map2),
        to_, t_or_w(P2, Map2, Map3),
   then_, it_becomes_no_longer_the_case_that_, t_or_w(V, Map3, Map4), fluent(lambda([V,P,D],F)), 
   t_or_w(P, Map4, Map5), word(heading), t_or_w(D, Map5, _Map6), period.

% added by YB on 2023-08-04
% a first direction is defined as horizontal.
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(D1, M_In, M_Out), is_, word(defined), word(as), word(H),
   {Timeless=..[H, D1]}.

% added by YB on 2023-08-04
% [X,Y] is defined as a place.
literal(M_In, M_Out, Timeless) --> %spypoint,
   t_or_w(P, M_In, M_Out), is_, word(defined), word(as), word(a), word(H),
   {Timeless=..[H, P]}.

% added by YB on 2023-08-08
% point at a X and a Y heading northward is mapped to a XX and a YY with horizontal size a  Xcar and vertical size a Ycar.
literal(M_In, M_Out, Timeless) --> %spypoint,
   word(point), at_,
   t_or_w(C11, M_In, M1), and_, t_or_w(C12, M1, M2), word(heading), t_or_w(D, M2, M3), is_, word(mapped), to_, t_or_w(C21, M3, M4), and_, t_or_w(C22, M4, M5),
   with_, word(horizontal), word(size), t_or_w(C31, M5, M6), and_,  word(vertical), word(size), t_or_w(C32, M6, M_Out),
   {Timeless=..[position, C11, C12, D, C21, C22, C31, C32]}.


