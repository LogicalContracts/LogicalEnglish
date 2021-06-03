:- module(le2taxlog, [document/3]).
:- use_module(library(tokenize)).
:- thread_local literal/5, text_size/1, notice/3, dict/3.
:- discontiguous statement/3, declaration/4, predicate/3, action/3.

% 	text_to_logic(+String,-Errors,-Clauses) is det
text_to_logic(String, Error, Translation) :-
    tokenize(String, Tokens, [cased(true), spaces(true)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    write(CTokens), 
    ( phrase(document(Output), CTokens) -> 
        (Translation=Output, Error=[])
    ;   (Error=Output, Translation=[])). 

clean_comments([], []) :- !.
clean_comments(['%'|Rest], New) :- % like in prolog comments start with %
    jump_comment(Rest, Next), 
    clean_comments(Next, New). 
clean_comments([Code|Rest], [Code|New]) :-
    clean_comments(Rest, New).

jump_comment([], []).
jump_comment(['\n'|Rest], ['\n'|Rest]). % leaving the end of line in place
jump_comment([_|R1], R2) :-
    jump_comment(R1, R2). 

% document(-Translation, In, Rest)
% a DCG predicate to translate a LE document into Taxlog prolog terms
document(Translation, In, Rest) :-
    header(Settings, In, Next), 
    spaces_or_newlines(_, Next, Intermediate),
    rules_previous(Intermediate, NextNext), 
    content(Content, NextNext, Rest), append(Settings, Content, Translation).
document(Error,_,_) :- showerror(Error), fail.

header(Settings, In, Next) :-
    length(In, TextSize), 
    ( settings(Rules, Settings, In, Next) -> true
    ; (Rules = [], Settings = [])),
    RulesforErrors =
      [ (text_size(TextSize)),
        (literal(M, M, _, Rest, _R1)
         :- text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
            asserterror('Syntax error found at position ', Pos), fail)],
    append(Rules, RulesforErrors, MRules),
    assertall(MRules). % asserting parsing rules for predicates and actions
  
settings(AllR, AllS) --> declaration(Rules,Setting), settings(RRules, RS),
      {append(Setting, RS, AllS),
     append(Rules, RRules, AllR)}.
settings([],[]) --> [].
  
content(C) --> 
    spaces_or_newlines(_), 
    statement(S), 
    spaces_or_newlines(_), 
    content(R), 
    spaces_or_newlines(_), {append(S,R,C)}.
content([]) --> [].

declaration(Rules, [predicates(Fluents)]) -->
    predicate_previous, list_of_predicates_decl(Rules, Fluents).

predicate_previous --> 
    spaces(_), [the], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces(_), ['\n'].

rules_previous --> 
    spaces(_), [the], spaces(_), [rules], spaces(_), [are], spaces(_), [':'], spaces(_), ['\n'].

list_of_predicates_decl([Ru|R1], [F|R2]) --> predicate_decl(Ru,F), rest_list_of_predicates_decl(R1,R2).

rest_list_of_predicates_decl(L1, L2) --> comma, list_of_predicates_decl(L1, L2).
rest_list_of_predicates_decl([],[]) --> period.

predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.

template_decl(RestW, [' '|RestIn], Out) :- % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\t'|RestIn], Out) :- % skip cntrl \t in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\n'|RestIn], Out) :- % skip cntrl \n in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\r'|RestIn], Out) :- % skip cntrl \r in template
    template_decl(RestW, RestIn, Out).
template_decl([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,['.', ','])), !,  % only . and , as boundaries. Beware!
    template_decl(RestW, RestIn, Out).
template_decl([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,['.', ',']). 

build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template) :-
    build_template_elements(RawTemplate, [], Arguments, TypesAndNames, OtherWords, Template),
    name_predicate(OtherWords, Predicate).

% build_template_elements(+Input, +Previous, -Args, -TypesNames, -OtherWords, -Template)
build_template_elements([], _, [], [], [], []).     
build_template_elements([Word|RestOfWords], Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable(RestOfWords, Var, NameWords, TypeWords, NextWords),
    name_predicate(NameWords, Name), 
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], Previous, RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, [Word|Previous], RestVars, RestTypes, Others, RestTemplate).

% extract_variable(+ListOfWords, Var, ListOfNameWords, ListOfTypeWords, NextWordsInText)
extract_variable([], _, [], [], []) :- !.                                % stop at when words run out
extract_variable([Word|RestOfWords], _, [], [], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    (reserved_word(Word); verb(Word); preposition(Word); punctuation(Word)).  % or punctuation
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

statement([if(Head,Conditions)]) -->
    literal([], Map1, Head), newline,
    spaces(Ind), if_, conditions(Ind, Map1, _MapN, ListOfConds), 
    {map_to_conds(ListOfConds, Conditions)},  spaces_or_newlines(_), period.

conditions(Ind0, Map1, MapN, [Type-Ind-Cond|RestC], Input, Rest) :-
    literal(Map1, Map2, Cond, Input, I1), 
    more_conds(Ind0, Ind, Map2, MapN, Type, RestC, I1, Rest).

more_conds(_, Ind, Map2, MapN, Type, RestC, I1, Rest) :-
    newline(I1, I2),
    spaces(Ind, I2, I3), 
    operator(Type, I3, I4), 
    conditions(Ind,Map2, MapN, RestC, I4, Rest). 
more_conds(Ind, Ind, Map, Map, last, [], In, In). 

% map_to_conds(+ListOfConds, -LogicallyOrderedConditions)
% fist the last condition always fits in
map_to_conds([last-_-C1], C1) :- !. 
map_to_conds([and-_-C1, last-_-C2], (C1,C2)) :- !. 
map_to_conds([or-_-C1, last-_-C2], (C1;C2)) :- !.
% from and to and
map_to_conds([and-Ind-C1, and-Ind-C2|RestC], (C1, C2, RestMapped) ) :- !, 
    map_to_conds(RestC, RestMapped).
% from or to ord
map_to_conds([or-Ind-C1, or-Ind-C2|RestC], (C1; C2; RestMapped) ) :- !, 
    map_to_conds(RestC, RestMapped).
% from and to deeper or
map_to_conds([and-Ind1-C1, or-Ind2-C2|RestC], (C1, (C2; RestMapped)) ) :-  
    Ind1 < Ind2, !, 
    map_to_conds(RestC, RestMapped).
% from deeper or to and
map_to_conds([or-Ind1-C1, and-Ind2-C2|RestC], ((C1; C2), RestMapped) ) :-
    Ind1 > Ind2, !, 
    map_to_conds([and-Ind2-C2|RestC], RestMapped).
% from or to deeper and
map_to_conds([or-Ind1-C1, and-Ind2-C2|RestC], (C1; (C2, RestMapped)) ) :-
    Ind1 < Ind2, !, 
    map_to_conds(RestC, RestMapped).
% from deeper and to or
map_to_conds([and-Ind1-C1, or-Ind2-C2|RestC], ((C1, C2);RestMapped ) ) :-
    Ind1 > Ind2, 
    map_to_conds(RestC, RestMapped).

operator(and, In, Out) :- and_(In, Out).
operator(or, In, Out) :- or_(In, Out).

literal(Map1, MapN, Literal) --> 
    predicate_statement(PossibleTemplate),
    {match_template(PossibleTemplate, Map1, MapN, Literal)}.

predicate_statement(RestW, [' '|RestIn], Out) :-  % skip spaces in template
    predicate_statement(RestW, RestIn, Out).
predicate_statement(RestW, ['\t'|RestIn], Out) :- % skip tabs in template
    predicate_statement(RestW, RestIn, Out).
predicate_statement([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,['\n', if, and, or, '.'])),
    predicate_statement(RestW, RestIn, Out).
predicate_statement([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,['\n', if, and, or, '.']). 

match_template(PossibleLiteral, Map1, MapN, Literal) :-
    dict(Predicate, _, Candidate),
    match(Candidate, PossibleLiteral, Map1, MapN, Template), 
    dict(Predicate, _, Template),
    Literal =.. Predicate. 

% match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
match([], [], Map, Map, []). % success! It succeds iff PossibleLiteral is totally consumed
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
    nonvar(Element), Word = Element, 
    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    determiner(Det), 
    % extract_variable(+ListOfWords, Var, ListOfNameWords, ListOfTypeWords, NextWordsInText)
    extract_variable(PossibleLiteral, Var, NameWords, _, NextWords), % <- leave that _ unbound!
    name_predicate(NameWords, Name),
    update_map(Var, Name, Map1, Map2),
    match(RestElements, NextWords, Map2, MapN, RestSelected).  
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Constant|RestSelected]) :-
    var(Element), 
    extract_constant([Word|PossibleLiteral], NameWords, NextWords),
    name_predicate(NameWords, Constant),
    update_map(Element, Constant, Map1, Map2),
    match(RestElements, NextWords, Map2, MapN, RestSelected). 

% extract_constant(+ListOfWords, ListOfNameWords, NextWordsInText)
extract_constant([], [], []) :- !.                                % stop at when words run out
extract_constant([Word|RestOfWords], [], [Word|RestOfWords]) :-   % stop at reserved words, verbs? or prepositions?. 
    (reserved_word(Word); verb(Word); preposition(Word); punctuation(Word)), !.  % or punctuation
%extract_constant([Word|RestOfWords], [Word|RestName], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestOfWords, RestName, NextWords).
extract_constant([Word|RestOfWords], [Word|RestName], NextWords) :-
    %is_a_type(Word),
    extract_constant(RestOfWords, RestName, NextWords).

determiner(Det) :-
    (ind_det(Det); ind_det_C(Det); def_det(Det); def_det_C(Det)), !. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, [], Template).

% template_elements(+Input,+InMap, -OutMap, +Previous, -Template)
template_elements([], Map1, Map1, _, []).     
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable(RestOfWords, Var, NameWords, _, NextWords),
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (def_det_C(Word); def_det(Word)), Previous \= [is|_], 
    extract_variable(RestOfWords, Var, NameWords, _, NextWords),
    name_predicate(NameWords, Name), 
    member(map(Var,Name), Map1),  % confirming it is an existing variable and unifying
    template_elements(NextWords, Map1, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Word|RestTemplate]) :-
    template_elements(RestOfWords, Map1, MapN, [Word|Previous], RestTemplate).

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

spaces(N) --> [' '], !, spaces(M), {N is M + 1}.
spaces(N) --> ['\t'], !, spaces(M), {N is M + 1}. % counting tab as one space
spaces(0) --> []. 

spaces_or_newlines(N) --> [' '], !, spaces_or_newlines(M), {N is M + 1}.
spaces_or_newlines(N) --> ['\t'], !, spaces_or_newlines(M), {N is M + 1}. % counting tab as one space
spaces_or_newlines(N) --> ['\r'], !, spaces_or_newlines(M), {N is M + 1}. % counting \r as one space
spaces_or_newlines(N) --> ['\n'], !, spaces_or_newlines(M), {N is M + 1}. % counting \n as one space
spaces_or_newlines(0) --> [].

newline --> ['\n'].
newline --> ['\r'].

if_ --> [if].

period --> ['.'].
comma --> [','].

comma_or_period --> period, !, ['\n'].
comma_or_period --> period, !.
comma_or_period --> comma, !, ['\n']. 
comma_or_period --> comma. 

and_ --> [and].

or_ --> [or].

%mapped_time_expression([T1,T2], Mi, Mo) -->
%    from_, t_or_w(T1, Mi, M2), to_, t_or_w(T2, M2, Mo).
% mapped_time_expression([_T0,T], Mi, Mo) -->
 %   at_, t_or_w(T, Mi, Mo).


/* --------------------------------------------------------- Utils in Prolog */
unpack_tokens([], []).
unpack_tokens([First|Rest], [New|NewRest]) :-
    (First = word(New); First=cntrl(New); First=punct(New); 
     First=space(New); First=number(New); First=string(New)), !,
    unpack_tokens(Rest, NewRest).  

ordinal(Ord) :-
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
   not(number(T)), not(punctuation(T)),
   not(reserved_word(T)),
   not(verb(T)),
   not(preposition(T)). 

ind_det_C('A').
ind_det_C('An').
ind_det_C('Some').

def_det_C('The').

ind_det(a).
ind_det(an).
ind_det(some).

def_det(the).

reserved_word(W) :- % more reserved words pending
    W = 'is'; W ='not'; W='When'; W='when'; W='if'; W='If'; W='then';
    W = 'at'; W= 'from'; W='to'; W='and'; W='half'; W='or'; 
    W = 'else'; W = 'otherwise'; 
    W = '<'; W = '='; W = '>'; W = '+'; W = '-'; W = '/'; W = '*';
    W = '{' ; W = '}' ; W = '(' ; W = ')' ; W = '[' ; W = ']';
    W = ':', W = ','; W = ';'.
reserved_word(P) :- punctuation(P).

%punctuation(punct(_P)).

punctuation('.').
punctuation(',').
punctuation(';').
punctuation(':').
punctuation('\'').

verb(Verb) :- present_tense_verb(Verb); continuous_tense_verb(Verb); past_tense_verb(Verb). 

present_tense_verb(is).
present_tense_verb(occurs).
present_tense_verb(can).
present_tense_verb(qualifies).
present_tense_verb(has).
present_tense_verb(satisfies).
present_tense_verb(owns).
present_tense_verb(belongs).

continuous_tense_verb(according).

past_tense_verb(looked).
past_tense_verb(could).
past_tense_verb(had).
past_tense_verb(tried).
past_tense_verb(explained).
 
preposition(of).
preposition(on).
preposition(from).
preposition(to).
preposition(at).
preposition(in).
preposition(with).
preposition(plus).
preposition(as).

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

write_taxlog_code(Source, Readable) :-
    Source = [predicates(_)|Clauses],
    write_taxlog_clauses(Clauses, Readable).

write_taxlog_clauses([], []).
write_taxlog_clauses([If|RestClauses], [ReadableIf|RestReadable]) :-
    write_taxlog_if(If, ReadableIf),
    write_taxlog_clauses(RestClauses, RestReadable).

write_taxlog_if(Rule, if(ReadableHead, ReadableBody)) :-
    Rule = if(Head, Body),
    write_taxlog_literal(Head, ReadableHead),
    write_taxlog_body(Body, ReadableBody).

write_taxlog_literal(Literal, ReadableLiteral) :-
    Literal =.. [Pred|ArgVars],
    dict([Pred|ArgVars], Names, _),
    replace_varnames(ArgVars, Names, NewArgs),
    ReadableLiteral =.. [Pred|NewArgs].

replace_varnames([], [], []) :- !. 
replace_varnames([Var|RestVar], [VarName-_|RestVarNames], [Name|NewRest]) :-
    var(Var),
    capitalize(VarName, Name), 
    replace_varnames(RestVar, RestVarNames, NewRest). 
replace_varnames([V|RestVar], [_|RestVarNames], [V|NewRest]) :-
    nonvar(V),
    replace_varnames(RestVar, RestVarNames, NewRest). 

% from drafter.pl
capitalize(X,NewX) :- 
    name(X,[First|Codes]), to_upper(First,U), name(NewX,[U|Codes]).

write_taxlog_body((A;B), or(NewA,NewB)) :-
    write_taxlog_body(A, NewA ),
    write_taxlog_body(B, NewB).

write_taxlog_body((A,B), and(NewA, NewB)) :-
    write_taxlog_body(A, NewA),
    write_taxlog_body(B, NewB).

write_taxlog_body(Lit, Readable) :-
    write_taxlog_literal(Lit, Readable). 
    