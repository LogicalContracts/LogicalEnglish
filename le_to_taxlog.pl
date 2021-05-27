:- module(le2taxlog, [document/3]).
:- use_module(library(tokenize)).
:- thread_local literal/5, text_size/1, notice/3, dict/3.
:- discontiguous statement/3, declaration/4, predicate/3, action/3.

% 	text_to_logic(+String,-Errors,-Clauses) is det
text_to_logic(String, Error, Translation) :-
    tokenize(String, Tokens, [cased(true), spaces(true)]),
    ( phrase(document(Output), Tokens) -> 
        (Translation=Output, Error=[])
    ;   (Error=Output, Translation=[])). 
    
% document(-Translation, In, Rest)
% a DCG predicate to translate a LE document into Taxlog prolog terms
document(Translation, In, Rest) :-
    length(In, TextSize), 
    ( settings(Rules, Settings, In, Next) -> true
    ; (Rules = [], Settings = [])),
    RulesforErrors =
      [ (text_size(TextSize)),
        (literal(M, M, _, Rest, _R1)
         :- text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
            asserterror('Syntax error found at position ', Pos), fail)],
    append(Rules, RulesforErrors, MRules),
    assertall(MRules), % asserting parsing rules for predicates and actions
    rules_previous(Next, NextNext), 
    content(Content, NextNext, Rest), append(Settings, Content, Translation).
  document(Error,_,_) :- showerror(Error), fail.
  
  settings(AllR, AllS) --> declaration(Rules,Setting), settings(RRules, RS),
      {append(Setting, RS, AllS),
     append(Rules, RRules, AllR)}.
  settings([],[]) --> [].
  
  content(C) --> statement(S), content(R), {append(S,R,C)}.
  content([]) --> [].


declaration(Rules, [predicates(Fluents)]) -->
    predicate_previous, list_of_predicates_decl(Rules, Fluents).

predicate_previous --> [word(the),space(' '),word(predicates),space(' '),word(are),punct(':'),cntrl('\n')].

rules_previous --> [word(the),space(' '),word(rules),space(' '),word(are),punct(':'),cntrl('\n')]. 

list_of_predicates_decl([Ru|R1], [F|R2]) --> predicate_decl(Ru,F), list_of_predicates_decl(R1,R2).
list_of_predicates_decl([],[]) --> [].

predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), comma_or_period, 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.

template_decl(RestW, [space(_)|RestIn], Out) :- % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, [cntrl(_)|RestIn], Out) :- % skip cntrl in template
    template_decl(RestW, RestIn, Out).
template_decl([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,[word(the), punct('.'), punct(',')])), !, 
    template_decl(RestW, RestIn, Out).
template_decl([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,[word(the), punct('.'), punct(',')]). 

build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template) :-
    build_template_elements(RawTemplate, Arguments, TypesAndNames, OtherWords, Template),
    name_predicate(OtherWords, Predicate).

build_template_elements([], [], [], [], []).     
build_template_elements([Word|RestOfWords], [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    ind_det_C(Word), 
    extract_variable(RestOfWords, Var, NameWords, TypeWords, NextWords),
    name_predicate(NameWords, Name), 
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, RestVars, RestTypes, Others, RestTemplate).

% extract_variable(+ListOfWords, Var, ListOfNameWords, ListOfTypeWords, NextWordsInText)
extract_variable([], _, [], [], []) :- !.                                % stop at when words run out
extract_variable([Word|RestOfWords], _, [], [], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    (reserved_word(Word); verb(Word); preposition(Word)), !.
extract_variable([Word|RestOfWords], Var, [Word|RestName], Type, NextWords) :- % ordinals are not part of the name
    ordinal(Word), !,
    extract_variable(RestOfWords, Var, RestName, Type, NextWords).
extract_variable([Word|RestOfWords], Var, [Word|RestName], [Word|RestType], NextWords) :-
    is_a_type(Word),
    extract_variable(RestOfWords, Var, RestName, RestType, NextWords).

name_predicate(Words, Predicate) :-
    concat_atom(Words, '_', Predicate). 

statement([Prolog]) --> prolog_fact(Prolog).

prolog_fact(Prolog) -->  % binary predicate
    t_or_w(X, [], M2), word(Predicate), t_or_w(Y, M2, _M_Out), period,
    {Prolog =.. [Predicate, X, Y]}.

statement([if(Head,and(Conditions))]) -->
    literal([], Map1, Head), [cntrl('\n')],
    spaces(_), if_, conjunction(_, Map1, _, Conditions), period.

statement([if(Head,and(Conditions))]) -->
    literal([], Map1, Head), [cntrl('\n')],
    spaces(_), if_, disjunction(_, Map1, _, Conditions), period.

%
conjunction(Ind, Map1, Map2, [C|CC]) --> conjunct(Ind, Map1, Map3, C),
    spaces(Ind), and_, conjunction(Ind, Map3, Map2, CC).
conjunction(_, Map1, Map2, [C]) --> literal(Map1, Map2, C).

conjunt(AndInd, Map1, MapN, or(D)) --> disjunction(OrInd, Map1, MapN, D), {AndInd < OrInd}.
conjunct(_, Map1, MapN, C) --> literal(Map1, MapN, C).

disjunction(Ind, Map1, MapN, [D|DD]) --> disjunct(Ind, Map1, Map3, D),
    spaces(Ind), or_, disjunction(Ind, Map3, MapN, DD).
disjunction(Ind, Map1, MapN, [D]) --> disjunct(Ind, Map1, MapN, D).

disjunct(OrInd, Map1, MapN, and(D)) --> conjunction(AndInd, Map1, MapN, D),  {OrInd < AndInd}.
disjunct(_, Map1, MapN, C) --> literal(Map1, MapN, C).

literal(Map1, MapN, Literal) --> 
    predicate_statement(PossibleTemplate),
    {match_template(PossibleTemplate, Map1, MapN, Literal)}.

predicate_statement(RestW, [space(_)|RestIn], Out) :-  % skip spaces in template
    predicate_statement(RestW, RestIn, Out).
predicate_statement(RestW, [cntrl('\t')|RestIn], Out) :- % skip tabs in template
    predicate_statement(RestW, RestIn, Out).
predicate_statement([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,[cntrl('\n'), word(if), word(and), word(or), puntc('.')])),
    predicate_statement(RestW, RestIn, Out).
predicate_statement([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,[cntrl('\n'), word(if), word(and), word(or), puntc('.')]). 

match_template(PossibleTemplate, Map1, MapN, Literal) :-
    rebuild_template(PossibleTemplate, Map1, MapN, Template),
    dict(Predicate, _, Template),
    Literal =.. Predicate. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, Template).

template_elements([], Map1, Map1, []).     
template_elements([Word|RestOfWords], Map1, MapN, [Var|RestTemplate]) :-
    (ind_det_C(Word); def_det_C(Word)), 
    extract_variable(RestOfWords, Var, NameWords, _, NextWords),
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, [Word|RestTemplate]) :-
    template_elements(RestOfWords, Map1, MapN, RestTemplate).

update_map(V, Name, InMap, InMap) :- % unify V with same named variable in current map
    member(map(V,Name),InMap), !.
update_map(V, Name, InMap, OutMap) :- % updates the map by adding a new variable into it. 
    OutMap = [map(V,Name)|InMap]. 

% a voter votes for a first candidate in a ballot
% the voter votes for a second candidate in the ballot at..
%literal(M_In,M_Out, happens(Action, T1, T2)) -->
%    t_or_w(X, M_In, M2), action(lambda([X,Y,B], Action)), for,
%    t_or_w(Y, M2, M3), in, t_or_w(B, M3, M4),
%    mapped_time_expression([T1,T2], M4, M_Out).

spaces(N) --> [space(' ')], !, spaces(M), {N is M + 1}.
spaces(N) --> [cntrl('\t')], !, spaces(M), {N is M + 1}. % counting tab as one space
spaces(0) --> []. 

if_ --> [word(if)].

period --> [punct('.')].
comma --> [punct(',')].

comma_or_period --> period, !, [cntrl('\n')].
comma_or_period --> period, !.
comma_or_period --> comma, !, [cntrl('\n')]. 
comma_or_period --> comma. 

and_ --> [word(and)].

or_ --> [word(or)].

%mapped_time_expression([T1,T2], Mi, Mo) -->
%    from_, t_or_w(T1, Mi, M2), to_, t_or_w(T2, M2, Mo).
% mapped_time_expression([_T0,T], Mi, Mo) -->
 %   at_, t_or_w(T, Mi, Mo).


/* --------------------------------------------------------- Utils in Prolog */

ordinal(word(Ord)) :-
    ordinal(_, Ord). 

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
% "the number of votes" is a special case of anaphora
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
   ground(T),
   not(T=number(_)), not(punctuation(T)),
   not(reserved_word(T)),
   not(verb(T)),
   not(preposition(T)). 

ind_det_C(word('A')).
ind_det_C(word('An')).
ind_det_C(word('Some')).

def_det_C(word('The')).

ind_det(word(a)).
ind_det(word(an)).
ind_det(word(some)).

def_det(word(the)).

reserved_word(word(W)) :- % more reserved words pending
  W= 'A', W='An', W = 'is'; W ='not'; W='When'; W='when'; W='if'; W='If'; W='then';
  W = 'at'; W= 'from'; W='to'; W='and'; W='half'; W='or'.
reserved_word(P) :- punctuation(P).

punctuation(punct(_P)).

%punctuation('.').
%punctuation(',').
%punctuation(';').
%punctuation(':').

verb(word(Verb)) :- present_tense_verb(Verb).

present_tense_verb(is).
present_tense_verb(occurs).
present_tense_verb(can).
present_tense_verb(qualifies).

preposition(word(Prep)) :- preposition_(Prep). 

preposition_(of).
preposition_(on).
preposition_(from).
preposition_(to).
preposition_(at).
preposition_(in).
preposition_(with).

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
   (clause(notice(_,_,_), _) -> retractall(notice(_,_,_));true),
   asserta(notice(error, Me, Pos)).

showerror(Me-Pos) :-
   (clause(notice(error, Me,Pos), _) ->
        ( nl, nl, write('Error: '), writeq(Me), writeq(Pos), nl, nl)
    ; nl, nl, writeln('No error reported')).

first_words([W1,W2,W3], [W1,W2,W3|R],R).
%first_words([W1,W2], [W1,W2|R],R).
%first_words([W1], [W1|R],R).

spypoint(A,A). % for debugging

