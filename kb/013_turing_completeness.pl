:- module('013+http://tests.com',[]).

% based on: https://en.wikipedia.org/wiki/Prolog#Turing_completeness

en("the target language is: prolog.

the templates are:
	the head of *a tape* is *a symbol* leaving *the rest of the tape*,
	some tm goes from *a tape one* to *a tape two*,
	it changes *a state* from *a left tape* to *a new left tape* and from *a right tape* to *a new right tape*,
	*a rule* says that *a state* and *a symbol* lead to *a new state* and *a new symbol* after performing *an action*,
	*an action* moves *a left tape* to *a new left tape* and *a rigth tape* to *a new right tape*,
	*a left list* is left after *a first left list* and *a right list* is left after *a first right list*.

the knowledge base turing complete includes:

some tm goes from a tape one to a tape two
	if it changes q0 from [] to a left list and from the tape one to a right list
	and the left list is the reverse of a second left list
	and one appends the second left list to the right list to obtain the tape two.

%turing(Tape0, Tape) :-
%    perform(q0, [], Ls, Tape0, Rs),
%    reverse(Ls, Ls1),
%    append(Ls1, Rs, Tape).

it changes qf from a left list to the left list and from a right list to the right list.

it changes a state Q0 from a Ls0 to a Ls and from a Rs0 to a Rs
	if the head of Rs0 is a Sym leaving a RsRest
	and a rule says that the state Q0 and Sym lead to a Q1 and a NewSym after performing an action
	and a NewLs has NewSym as head before RsRest
	and the action moves Ls0 to a Ls1 and NewLs to a Rs1
	and it changes Q1 from Ls1 to Ls and from Rs1 to Rs.

%perform(qf, Ls, Ls, Rs, Rs) :- !.
%perform(Q0, Ls0, Ls, Rs0, Rs) :-
%    symbol(Rs0, Sym, RsRest),
%    once(rule(Q0, Sym, Q1, NewSym, Action)),
%    action(Action, Ls0, Ls1, [NewSym|RsRest], Rs1),
%    perform(Q1, Ls1, Ls, Rs1, Rs).

the head of [] is b leaving [].
the head of a list is a Sym leaving a Rs
  if the list has Sym as head before Rs.

%symbol([], b, []).
%symbol([Sym|Rs], Sym, Rs).

left moves a Ls0 to a Ls and a Rs0 to a Rs
	if Ls0 is left after Ls and Rs0 is left after Rs.
stay moves a Ls to Ls and a Rs to Rs.
right moves a Ls0 to a NLs and a DRs to a Rs
	if NLs has a Sym as head before Ls0
	and DRs has Sym as head before Rs.

%action(left, Ls0, Ls, Rs0, Rs) :- left(Ls0, Ls, Rs0, Rs).
%action(stay, Ls, Ls, Rs, Rs).
%action(right, Ls0, [Sym|Ls0], [Sym|Rs], Rs).

[] is left after [] and a Rs0 is left after a NRs0
	if NRs0 has b as head before Rs0.
A LLs is left after a Ls and a Rs is left after a LRs
	if LLs has a L as head before Ls
	and LRs has L as head before Rs.

%left([], [], Rs0, [b|Rs0]).
%left([L|Ls], Ls, Rs, [L|Rs]).

%A simple example Turing machine is specified by the facts:
% rule(q0, 1, q0, 1, right).
%rule(q0, b, qf, 1, stay).

scenario machine one is:
	rule1 says that q0 and 1 lead to q0 and 1 after performing right.
	rule2 says that q0 and b lead to qf and 1 after performing stay.

query run is:
	some tm goes from [1,1,1] to which tape.


").

/** <examples>
?- answer("query run with scenario machine one").
*/