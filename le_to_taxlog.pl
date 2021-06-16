/* le_to_taxlog: a prolog module with predicates to translate from an 
extended version of Logical English into the Taxlog programming language. 

Main predicate: text_to_logic(String to be translated, Error, Translation)

Main DCG nonterminal: document(Translation)

See at the end the predicate le_taxlog_translate to be used from SWISH

It assumes an entry with the following structure. The expression:

the predicates are:

followed by the declarations of all the predicates involved in the knowledge base.
Each declarations define a template with the variables and other words required to
describe a relevant relation. It is a comma separated list of templates which ends
with a period. 

After that period, the following statement introduces the knowledge base:

the knowledge base includes: 

And it is followed by the rules and facts written in Logical English or in 
Taxlog/Prolog relational syntax. Each rule must end with a period. 

Indentation is used to organize the and/or list of conditions by strict
observance of one condition per line with a level of indentation that 
correspond to each operator and corresponding conditions. 

*/

:- module(le_to_taxlog, [document/3]).
:- use_module('./tokenize/prolog/tokenize.pl').
:- thread_local literal/5, text_size/1, notice/3, dict/3.
:- discontiguous statement/3, declaration/4, predicate/3, action/3.

% Main clause: text_to_logic(+String,-Errors,-Clauses) is det
text_to_logic(String, Error, Translation) :-
    tokenize(String, Tokens, [cased(true), spaces(true)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    %print_message(informational, String), 
    ( phrase(document(Translation), CTokens) -> 
        ( print_message(informational, Translation), Error=[] )
    ;   ( showerror(Error), Translation=[])). 

% document(-Translation, In, Rest)
% a DCG predicate to translate a LE document into Taxlog prolog terms
document(Translation, In, Rest) :-
    spaces_or_newlines(_, In, In1),
    header(Settings, In1, In2), % print_message(informational, Settings), 
    spaces_or_newlines(_, In2, In3),
    rules_previous(In3, In4),  %print_message(informational, 'this is the knowledge base:'), 
    content(Content, In4, Rest), %print_message(informational, 'got the content'),
    append(Settings, Content, Translation).

% header parses all the declarations and assert them into memory to be invoked by the rules. 
header(Settings, In, Next) :-
    length(In, TextSize), 
    ( settings(Rules, Settings, In, Next) -> true
    ; (Rules = [], Settings = [])),
    RulesforErrors = % rules for error have been statically added
      [(text_size(TextSize))],
    append(Rules, RulesforErrors, MRules),
    assertall(MRules). % asserting contextual information

/* --------------------------------------------------------- LE DCGs */

settings(AllR, AllS) --> declaration(Rules,Setting), settings(RRules, RS),
      {append(Setting, RS, AllS),
     append(Rules, RRules, AllR)}.
settings([],[]) --> [].
 
content([S|R]) --> 
    spaces_or_newlines(_), 
    statement(S), !, 
    content(R).
content([]) --> 
    spaces_or_newlines(_), []. 

declaration(Rules, [predicates(Fluents)]) -->
    predicate_previous, list_of_predicates_decl(Rules, Fluents).

predicate_previous --> 
    spaces(_), [the], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces(_), newline.

list_of_predicates_decl([Ru|R1], [F|R2]) --> predicate_decl(Ru,F), rest_list_of_predicates_decl(R1,R2).

rules_previous --> 
    spaces(_), [the], spaces(_), [rules], spaces(_), [are], spaces(_), [':'], spaces(_), newline.
rules_previous --> 
    spaces(_), [the], spaces(_), ['knowledge'], spaces(_), [base], spaces(_), [includes], spaces(_), [':'], spaces(_), newline.

rest_list_of_predicates_decl(L1, L2) --> comma, list_of_predicates_decl(L1, L2).
rest_list_of_predicates_decl([],[]) --> period.

predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.

% we are using this resource of the last clause to record the error and its details
% not very useful with loops, of course. 
% error clause
predicate_decl(_, _, Rest, _) :- 
    text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
    asserterror('Syntax error found in a declaration ', Pos, Rest), fail.

% statement: the different types of statements in a LE text
statement(Statement) --> 
    literal_([], Map1, Head), body_(Body, Map1, _), period, !, % <-- CUT 
    {(Body = [] -> Statement = [Head]; Statement = [if(Head, Body)])}. 

statement([Fact]) --> 
    spaces(_), prolog_literal_(Fact, [], _), spaces_or_newlines(_), period.

body_([], _, _) --> spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    newline, spaces(Ind), if_, conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    if_, newline, spaces(Ind), conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).

% literal_ reads a list of words until it finds one of these: ['\n', if, and, or, '.', ',']
% it then tries to match those words against a template in memory (see dict/3 predicate)
literal_(Map1, MapN, Literal) --> 
    predicate_template(PossibleTemplate),
    {match_template(PossibleTemplate, Map1, MapN, Literal)}.
% rewritten to use in swish. Fixed! It was a name clash. Apparently "literal" is used somewhere else
%literal_(Map1, MapN, Literal, In, Out) :-  print_message(informational, '  inside a literal'),
%        predicate_template(PossibleTemplate, In, Out), print_message(informational, PossibleTemplate),
%        match_template(PossibleTemplate, Map1, MapN, Literal).
% error clause
literal_(M, M, _, Rest, _) :- 
    text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
    asserterror('Syntax error found in a literal ', Pos, Rest), fail.

conditions(Ind0, Map1, MapN, Conds) --> 
    condition(Cond, Ind0, Map1, Map2), 
    more_conds(Ind0, Ind0, _IndF, Map2, MapN, Cond, Conds).
    %{Ind0=<IndF}. % 

% more_conds(PreviosInd, CurrentInd, MapIn, MapOut, InCond, OutConds)
more_conds(Ind0, Ind1, Ind, Map1, MapN, Cond, Conditions) --> 
    newline, spaces(Ind), {Ind0 =< Ind}, % if the new indentation is deeper, it goes on as before. 
    operator(Op), condition(Cond2, Ind, Map1, MapN), 
    {add_cond(Op, Ind1, Ind, Cond, Cond2, Conditions)}.
% three conditions look ahead
more_conds(Ind0, Ind1, Ind4, Map1, MapN, C1, RestMapped, In1, Out) :-
    newline(In1, In2), spaces(Ind2, In2, In3), Ind0=<Ind2, operator(Op1, In3, In4), condition(C2, Ind1, Map1, Map2, In4, In5), 
    newline(In5, In6), spaces(Ind3, In6, In7), Ind0=<Ind3, operator(Op2, In7, In8), condition(C3, Ind2, Map2, Map3, In8, In9), 
    adjust_op(Ind2, Ind3, C1, Op1, C2, Op2, C3, Conditions), 
    more_conds(Ind0, Ind3, Ind4, Map3, MapN, Conditions, RestMapped, In9, Out). 
more_conds(_, Ind, Ind, Map, Map, Cond, Cond, Rest, Rest).  
 
% this naive definition of term is problematic
term_(Term, Map1, MapN) --> 
    (variable(Term, Map1, MapN), !); (constant(Term, Map1, MapN), !); (list_(Term, Map1, MapN), !). %; (compound_(Term, Map1, MapN), !).

list_(List, Map1, MapN) --> 
    spaces(_), bracket_open_, !, extract_list(List, Map1, MapN), bracket_close.   

compound_(V1/V2, Map1, MapN) --> 
    term_(V1, Map1, Map2), ['/'], term_(V2, Map2, MapN). 

expression((X - Y), Map1, MapN) --> 
    term_(X, Map1, Map2), spaces(_), minus_, spaces(_), expression(Y, Map2, MapN), spaces(_), !. 
expression((X + Y), Map1, MapN) --> 
    term_(X, Map1, Map2), spaces(_), plus_, spaces(_), expression(Y, Map2, MapN), spaces(_), !. 
expression(Y, Map1, MapN) --> 
    term_(Y, Map1, MapN), spaces(_).
% error clause
expression(_, _, _, Rest, _) :- 
    text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
    asserterror('Syntax error found at an expression ', Pos, Rest), fail.

% this produces a Taxlog condition with the form: 
% setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on Before, SetOfPreviousOwners)
% from a set of word such as: 
%     and a set of previous owners is a collection of an owner / a share 
%           where the asset is ultimately owned by the share with the owner at the previous time
condition(FinalExpression, _, Map1, MapN) --> spypoint, 
    %variable(Set, Map1, Map2), is_a_collection_of_, term_(Term, Map2, Map3), where_,
    variable(Set, Map1, Map2), is_a_collection_of_, literal_(Map2, Map3, Term), % moved where to the following line
    newline, spaces(Ind2), where_, conditions(Ind2, Map3, Map4, Goals),
    modifiers(setof(Term,Goals,Set), Map4, MapN, FinalExpression).

% for every a party is a party in the event, it is the case that:
condition(FinalExpression, _, Map1, MapN) --> spypoint, 
    for_all_cases_in_which_, newline, 
    spaces(Ind2), conditions(Ind2, Map1, Map2, Conds), spaces_or_newlines(_), 
    it_is_the_case_that_colon_, newline, 
    spaces(Ind3), conditions(Ind3, Map2, Map3, Goals),
    modifiers(forall(Conds,Goals), Map3, MapN, FinalExpression).

% the Value is the sum of each Asset Net such that
%condition(FinalExpression, _, Map1, MapN) --> 
%    variable(Value, Map1, Map2), is_the_sum_of_each_, extract_variable(Each, NameWords, _), such_that_,  
%    { name_predicate(NameWords, Name), update_map(Each, Name, Map2, Map3) }, newline, 
%    spaces(Ind), conditions(Ind, Map3, Map4, Conds), 
%    modifiers(aggregate_all(sum(Each),Conds,Value), Map4, MapN, FinalExpression).
    
% it is not the case that: 
condition(not(Conds), _, Map1, MapN) --> 
    spaces(_), not_, newline,  
    spaces(Ind), conditions(Ind, Map1, MapN, Conds).

%condition(Cond, _, Map1, MapN, R1, RN) :-  
%    print_message(informational, ' condition/literal '),  
%    predicate_template(Possible, R1, RN), print_message(informational, 'Posible '), print_message(informational, Possible),
%    match_template(Possible, Map1, MapN, Cond), !, print_message(informational, 'Literal '), print_message(informational, Cond). 

condition(Cond, _, Map1, MapN) --> spypoint, 
    literal_(Map1, MapN, Cond). 

condition(assert(Prolog), _, Map1, MapN) -->
    this_information_, prolog_literal_(Prolog, Map1, MapN), has_been_recorded_. 

% condition(-Cond, ?Ind, +InMap, -OutMap)
condition(InfixBuiltIn, _, Map1, MapN) --> spypoint, 
    term_(Term, Map1, Map2), spaces(_), builtin_(BuiltIn), !, 
    spaces(_), expression(Expression, Map2, MapN), {InfixBuiltIn =.. [BuiltIn, Term, Expression]}. 

% error clause
condition(_, _Ind, Map, Map, Rest, _) :- 
        text_size(Size), length(Rest, Rsize), Pos is Size - Rsize, 
        asserterror('Syntax error found at a condition ', Pos, Rest), fail.

% modifiers add reifying predicates to an expression. 
% modifiers(+MainExpression, +MapIn, -MapOut, -FinalExpression)
modifiers(MainExpression, Map1, MapN, on(MainExpression, Var) ) -->
    newline, spaces(_), at_, variable(Var, Map1, MapN). % newline before a reifying expression
modifiers(MainExpression, Map, Map, MainExpression) --> [].  

variable(Var, Map1, MapN) --> 
    spaces(_), [Det], {determiner(Det)}, extract_variable(Var, NameWords, _), !, % <-- CUT!
    { name_predicate(NameWords, Name), update_map(Var, Name, Map1, MapN) }. 

constant(Constant, Map, Map) -->
    extract_constant(NameWords), !, { name_predicate(NameWords, Constant) }. % <-- CUT!

prolog_literal_(Prolog, Map1, MapN) -->
    predicate_name_(Predicate), parentesis_open_, extract_list(Arguments, Map1, MapN), parentesis_close_,
    {Prolog =.. [Predicate|Arguments]}.

predicate_name_(Module:Predicate) --> 
    [Module], colon_, extract_constant(NameWords), { name_predicate(NameWords, Predicate) }, !.
predicate_name_(Predicate) --> extract_constant(NameWords), { name_predicate(NameWords, Predicate) }.

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

one_or_many_newlines --> newline, spaces(_), one_or_many_newlines, !. 
one_or_many_newlines --> [].

if_ --> [if], spaces_or_newlines(_).  % so that if can be written many lines away from the rest

period --> ['.'].
comma --> [','].
colon_ --> [':'].

comma_or_period --> period, !.
comma_or_period --> comma. 

and_ --> [and].

or_ --> [or].

not_ --> [it], spaces(_), [is], spaces(_), [not], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_). 

is_the_sum_of_each_ --> [is], spaces(_), [the], spaces(_), [sum], spaces(_), [of], spaces(_), [each], spaces(_) .

such_that_ --> [such], spaces(_), [that], spaces(_). 

at_ --> [at], spaces(_). 

minus_ --> ['-'], spaces(_).

plus_ --> ['+'], spaces(_).

divide_ --> ['/'], spaces(_).

times_ --> ['*'], spaces(_).

bracket_open_ --> ['['], spaces(_). 
bracket_close --> [']'], spaces(_). 

parentesis_open_ --> ['('], spaces(_).
parentesis_close_ --> [')'], spaces(_). 

this_information_ --> [this], spaces(_), [information], spaces(_).

has_been_recorded_ --> [has], spaces(_), [been], spaces(_), [recorded], spaces(_).

for_all_cases_in_which_ --> spaces_or_newlines(_), [for], spaces(_), [all], spaces(_), [cases], spaces(_), [in], spaces(_), [which], spaces(_).

it_is_the_case_that_colon_ --> [it], spaces(_), [is], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_), [':'], spaces(_).

is_a_collection_of_ --> [is], spaces(_), [a], spaces(_), [collection], spaces(_), [of], spaces(_). 

where_ --> [where], spaces(_). 


/* --------------------------------------------------- Supporting code */
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

% cuts added to improve efficiency
template_decl(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\t'|RestIn], Out) :- !, % skip cntrl \t in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\n'|RestIn], Out) :- !, % skip cntrl \n in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\r'|RestIn], Out) :- !, % skip cntrl \r in template
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
    extract_variable(Var, NameWords, TypeWords, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], Previous, RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, [Word|Previous], RestVars, RestTypes, Others, RestTemplate).

% extract_variable(-Var, -ListOfNameWords, -ListOfTypeWords, +ListOfWords, -NextWordsInText)
% refactored as a dcg predicate
extract_variable(_, [], [], [], []) :- !.                                % stop at when words run out
extract_variable(_, [], [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    (reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
extract_variable(Var, RestName, RestType, [' '|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable(Var, RestName, RestType, RestOfWords, NextWords).
extract_variable(Var, RestName, RestType, ['\t'|RestOfWords], NextWords) :- !,  % skipping spaces
    extract_variable(Var, RestName, RestType, RestOfWords, NextWords).  
extract_variable(Var, [Word|RestName], Type, [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
    ordinal(Word), !, 
    extract_variable(Var, RestName, Type, RestOfWords, NextWords).
extract_variable(Var, [Word|RestName], [Word|RestType], [Word|RestOfWords], NextWords) :-
    is_a_type(Word),
    extract_variable(Var, RestName, RestType, RestOfWords, NextWords).

name_predicate(Words, Predicate) :-
    concat_atom(Words, '_', Predicate). 

% map_to_conds(+ListOfConds, -LogicallyOrderedConditions)
% the last condition always fits in
map_to_conds([last-_-C1], C1) :- !.   
map_to_conds([and-_-C1, last-_-C2], (C1,C2)) :- !. 
map_to_conds([or-_-C1, last-_-C2], (C1;C2)) :- !.
% from and to and
map_to_conds([and-Ind-C1, and-Ind-C2|RestC], ((C1, C2), RestMapped) ) :- !, 
    map_to_conds(RestC, RestMapped).
% from or to ord
map_to_conds([or-Ind-C1, or-Ind-C2|RestC], ((C1; C2); RestMapped) ) :- !, 
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

add_cond(and, Ind1, Ind2,  (C; C3), C4, (C; (C3, C4))) :-
    Ind1 =< Ind2, !. 
add_cond(and, Ind1, Ind2,  (C; C3), C4, ((C; C3), C4)) :-
    Ind1 > Ind2, !.     
add_cond(and,_, _, (C, C3), C4, (C, (C3, C4))) :- !. 
add_cond(and,_, _, Cond, RestC, (Cond, RestC)) :- !. 
add_cond(or, Ind1, Ind2, (C, C3), C4, (C, (C3; C4))) :- 
    Ind1 =< Ind2, !. 
add_cond(or, Ind1, Ind2, (C, C3), C4, ((C, C3); C4)) :- 
    Ind1 > Ind2, !. 
add_cond(or, _, _, (C; C3), C4, (C; (C3; C4))) :- !. 
add_cond(or, _,_, Cond, RestC, (Cond; RestC)).

% adjust_op(Ind1, Ind2, PreviousCond, Op1, Cond2, Op2, Rest, RestMapped, Conditions)
% from and to and
adjust_op(Ind1, Ind2, C1, and, C2, and, C3, ((C1, C2), C3) ) :- 
    Ind1 =< Ind2, !.
adjust_op(Ind1, Ind2, C1, and, C2, and, C3, ((C1, C2), C3) ) :- 
    Ind1 > Ind2, !.
% from or to ord
adjust_op(Ind1, Ind2, C1, or, C2, or, C3, ((C1; C2); C3) ) :- 
    Ind1 =< Ind2, !.
adjust_op(Ind1, Ind2, C1, or, C2, or, C3, ((C1; C2); C3) ) :- 
    Ind1 > Ind2, !.
% from and to deeper or
adjust_op(Ind1, Ind2, C1, and, C2, or, C3, (C1, (C2; C3)) ) :- 
    Ind1 < Ind2, !.
% from deeper or to and
adjust_op(Ind1, Ind2, C1, or, C2, and, C3, ((C1; C2), C3) ) :- 
    Ind1 > Ind2, !.
% from or to deeper and
adjust_op(Ind1, Ind2, C1, or, C2, and, C3, (C1; (C2, C3)) ) :- 
    Ind1 < Ind2, !.
% from deeper and to or
adjust_op(Ind1, Ind2, C1, and, C2, or, C3, ((C1, C2); C3) ) :- 
    Ind1 > Ind2.

operator(and, In, Out) :- and_(In, Out).
operator(or, In, Out) :- or_(In, Out).

% cuts added to improve efficiency
predicate_template(RestW, [' '|RestIn], Out) :- !, %print_message(informational, ' '), % skip spaces in template
    predicate_template(RestW, RestIn, Out).
predicate_template(RestW, ['\t'|RestIn], Out) :- !, % print_message(informational, '\t'), % skip tabs in template
    predicate_template(RestW, RestIn, Out).
predicate_template([Word|RestW], [Word|RestIn], Out) :- 
    not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, %print_message(informational, Word), 
    predicate_template(RestW, RestIn, Out).
predicate_template([], [], []). 
predicate_template([], [Word|Rest], [Word|Rest]) :- 
    lists:member(Word,['\n', if, and, or, '.', ',']). 

match_template(PossibleLiteral, Map1, MapN, Literal) :- 
    dict(Predicate, _, Candidate),
    match(Candidate, PossibleLiteral, Map1, MapN, Template), 
    dict(Predicate, _, Template), 
    Literal =.. Predicate, print_message(informational, 'Match!! with '-[Literal]). 

% match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
match([], [], Map, Map, []) :- !.  % success! It succeds iff PossibleLiteral is totally consumed
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
    nonvar(Element), Word = Element, % print_message(informational, ' '), print_message(informational, Word),
    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    determiner(Det), 
    extract_variable(Var, NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], !, % <-- CUT! % it is not empty % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), %print_message(informational, 'found a variable '), print_message(informational, Name),
    match(RestElements, NextWords, Map2, MapN, RestSelected).  
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Constant|RestSelected]) :-
    var(Element), 
    extract_constant(NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [], % <-- CUT!  % it is not empty
    name_predicate(NameWords, Constant),
    %update_map(Element, Constant, Map1, Map2), % print_message(informational, 'found a constant '), print_message(informational, Constant),
    match(RestElements, NextWords, Map1, MapN, RestSelected). 
match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [List|RestSelected]) :-
    var(Element), 
    extract_list(List, Map1, Map2, PossibleLiteral, NextWords),
    match(RestElements, NextWords, Map2, MapN, RestSelected). 

% extract_constant(ListOfNameWords, +ListOfWords, NextWordsInText)
extract_constant([], [], []) :- !.                                % stop at when words run out
extract_constant([], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs? or prepositions?. 
    (reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
%extract_constant([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_constant(RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_constant(RestName, RestOfWords, NextWords).
extract_constant(RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_constant(RestName, RestOfWords, NextWords).
extract_constant([Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    not(determiner(Word)), % no determiners inside constants!
    extract_constant(RestName, RestOfWords, NextWords).

% extract_list(-List, +Map1, -Map2, +[Word|PossibleLiteral], -NextWords),
extract_list([], Map, Map, [']'|Rest], [']'|Rest]) :- !. % stop but leave the symbol for further verification
extract_list([], Map, Map, [')'|Rest], [')'|Rest]) :- !. 
extract_list(RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(RestList, Map1, MapN, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_list(RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(RestList, Map1, MapN, [','|RestOfWords],  NextWords) :- !, 
    extract_list(RestList, Map1, MapN, RestOfWords, NextWords).
extract_list([Var|RestList], Map1, MapN, [Det|InWords], LeftWords) :-
    determiner(Det), 
    extract_variable(Var, NameWords, _, InWords, NextWords), NameWords \= [], % <-- CUT! % <- leave that _ unbound!
    name_predicate(NameWords, Name),  !,
    update_map(Var, Name, Map1, Map2),
    extract_list(RestList, Map2, MapN, NextWords, LeftWords).
extract_list([Member|RestList], Map1, MapN, InWords, LeftWords) :-
    extract_constant(NameWords, InWords, NextWords), NameWords \= [],
    name_predicate(NameWords, Member),
    extract_list(RestList, Map1, MapN, NextWords, LeftWords).

determiner(Det) :-
    (ind_det(Det); ind_det_C(Det); def_det(Det); def_det_C(Det)), !. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, [], Template).

% template_elements(+Input,+InMap, -OutMap, +Previous, -Template)
template_elements([], Map1, Map1, _, []).     
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable(Var, NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (def_det_C(Word); def_det(Word)), Previous \= [is|_], 
    extract_variable(Var, NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    member(map(Var,Name), Map1),  % confirming it is an existing variable and unifying
    template_elements(NextWords, Map1, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Word|RestTemplate]) :-
    template_elements(RestOfWords, Map1, MapN, [Word|Previous], RestTemplate).

% update_map(?V, +Name, +InMap, -OutMap)
update_map(V, Name, InMap, InMap) :- % unify V with the variable with the same name in the current map
    var(V), nonvar(Name), nonvar(InMap), 
    member(map(V,Name),InMap).
update_map(V, Name, InMap, OutMap) :-  % updates the map by adding a new variable into it. 
    var(V), nonvar(Name), nonvar(InMap), 
    OutMap = [map(V,Name)|InMap]. 
%update_map(V, _, Map, Map) :-
%    nonvar(V). 

% consult_map(+V, -Name, +Inmap, -OutMap)
consult_map(V, Name, InMap, InMap) :-
    member(map(Var, SomeName), InMap), Var == V, Name = SomeName, !.  
consult_map(V, V, Map, Map). % leave the name unassigned

builtin_(BuiltIn, [BuiltIn1, BuiltIn2|RestWords], RestWords) :- 
    atom_concat(BuiltIn1, BuiltIn2, BuiltIn), 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in), !.
builtin_(BuiltIn, [BuiltIn|RestWords], RestWords) :- 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in). 

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

is_a_type(T) :- % pending integration with wei2nlen:is_a_type/1
   ground(T),
   not(number(T)), not(punctuation(T)),
   not(reserved_word(T)),
   not(verb(T)),
   not(preposition(T)). 

/* ------------------------------------------------ determiners */

ind_det_C('A').
ind_det_C('An').
% ind_det_C('Some').

def_det_C('The').

ind_det(a).
ind_det(an).
% ind_det(some).

def_det(the).

/* ------------------------------------------------ reserved words */
reserved_word(W) :- % more reserved words pending
    W = 'is'; W ='not'; W='if'; W='If'; W='then'; W = 'where';  
    W = 'at'; W= 'from'; W='to'; W='and'; W='half'; W='or'; 
    W = 'else'; W = 'otherwise'; 
    W = such ; 
    W = '<'; W = '='; W = '>'; W = '+'; W = '-'; W = '/'; W = '*';
    W = '{' ; W = '}' ; W = '(' ; W = ')' ; W = '[' ; W = ']';
    W = ':', W = ','; W = ';'.
reserved_word(P) :- punctuation(P).

/* ------------------------------------------------ punctuation */
%punctuation(punct(_P)).

punctuation('.').
punctuation(',').
punctuation(';').
punctuation(':').
punctuation('\'').

/* ------------------------------------------------ verbs */
verb(Verb) :- present_tense_verb(Verb); continuous_tense_verb(Verb); past_tense_verb(Verb). 

present_tense_verb(is).
present_tense_verb(occurs).
present_tense_verb(meets).
present_tense_verb(relates).
present_tense_verb(can).
present_tense_verb(qualifies).
present_tense_verb(has).
present_tense_verb(satisfies).
present_tense_verb(owns).
present_tense_verb(belongs).
present_tense_verb(applies).

continuous_tense_verb(according).

past_tense_verb(looked).
past_tense_verb(could).
past_tense_verb(had).
past_tense_verb(tried).
past_tense_verb(explained).
past_tense_verb(ocurred).
 
/* ------------------------------------------------- prepositions */
preposition(of).
preposition(on).
preposition(from).
preposition(to).
preposition(at).
preposition(in).
preposition(with).
preposition(plus).
preposition(as).

/* ------------------------------------------------- memory handling */
assertall([]).
assertall([F|R]) :-
    not(asserted(F)),
    assertz(F), !,
    % print_message(informational, 'Asserting .. '), print_message(informational, F), nl,
    assertall(R).
assertall([_F|R]) :-
    % print_message(informational, ' Already there .. '), print_message(informational, F), nl,
    assertall(R).

asserted(F :- B) :- clause(F, B). % as a rule with a body
asserted(F) :- clause(F,true). % as a fact

/* -------------------------------------------------- error handling */
asserterror(Me, Pos, Rest) :-
   select_first_section(Rest, 40, Context), 
   (clause(notice(_,_,_, _), _) -> retractall(notice(_,_,_,_));true),
   asserta(notice(error, Me, Pos, Context)).

select_first_section([], _, []) :- !.
select_first_section(_, 0, []) :- !. 
select_first_section([E|R], N, [E|NR]) :-
    N > 0, NN is N - 1,
    select_first_section(R, NN, NR). 

showerror(Me-Pos-Context) :-
   (clause(notice(error, Me,Pos, Context), _) ->
      print_message(error, [Me, at, Pos,' just before \"',Context, '\"']) 
    ; print_message(error,'No error reported')  ).

write_words([]) :- !.
write_words([Word|RestW]) :- print_message(informational, Word), 
    write_words(RestW). 

explain_error(String, Me-Pos, Message) :-
    Start is Pos - 20, % assuming a window of 40 characters. 
    (Start>0 -> 
        ( sub_string(String,Start,40, _, Windows),
          sub_string(Windows, 0, 20, _, Left),
          sub_string(Windows, 20, 20, _, Right) )
    ;   ( sub_string(String,Pos,40, _, Windows), 
          Left = "", Right = Windows ) ),
    Message = [Me, at , Pos, near, ':', Left, '<-HERE->', Right]. 

spypoint(A,A). % for debugging

/* ------------------------------------------ producing readable taxlog code */
write_taxlog_code(Source, Readable) :-
    Source = [predicates(_)|Clauses],
    write_taxlog_clauses(Clauses, Readable).

write_taxlog_clauses([], []) :- !. 
write_taxlog_clauses([If|RestClauses], [ReadableIf|RestReadable]) :-
    write_taxlog_if(If, ReadableIf), !, 
    write_taxlog_clauses(RestClauses, RestReadable).
write_taxlog_clauses([Fact|RestClauses], [ReadableFact|RestReadable]) :-
    write_taxlog_literal(Fact, ReadableFact, [], _),
    write_taxlog_clauses(RestClauses, RestReadable). 

write_taxlog_if(Rule, if(ReadableHead, ReadableBody)) :-
    Rule = if(Head, Body),
    write_taxlog_literal(Head, ReadableHead, [], Map2),
    write_taxlog_body(Body, ReadableBody, Map2, _).

write_taxlog_literal(not(Body), not(ReadableLiteral), Map1, MapN) :- !, 
    write_taxlog_body(Body, ReadableLiteral, Map1, MapN). 

write_taxlog_literal(aggregate_all(sum(Each),Conds,Value), aggregate_all(sum(EachName),NewConds,ValueName), Map1, MapN) :-
    consult_map(Value, ValueName, Map1, Map2), 
    consult_map(Each, EachName, Map2, Map3), 
    write_taxlog_body(Conds, NewConds, Map3, MapN). 

write_taxlog_literal(Literal, ReadableLiteral, Map1, MapN) :-
    Literal =.. [Pred|ArgVars],
    dict([Pred|ArgVars], Names, _), % to use that information
    replace_varnames(ArgVars, Names, NewArgs, Map1, MapN),
    ReadableLiteral =.. [Pred|NewArgs].

write_taxlog_literal(InLit, OutLit, Map1, MapN) :-
    %predicate_property(system:InBuiltIn, built_in),
    InLit =.. [Pred|Args],
    write_taxlog_args(Map1, MapN, Args, NewArgs),
    OutLit =.. [Pred|NewArgs]. 

write_taxlog_args(Map, Map, [], []).
write_taxlog_args(Map1, MapN, [Arg1|RestArg], [NArg1|RestNArg]) :-
    (var(Arg1) -> (consult_map(Arg1, Name1, Map1, Map2), NArg1 = Name1) % does name appears in Map1?
    ; (compound(Arg1) -> write_taxlog_literal(Arg1, NArg1, Map1, Map2)
        ; (NArg1 = Arg1, Map2 = Map1) ) ),
    write_taxlog_args(Map2, MapN, RestArg, RestNArg). 

replace_varnames([], [], [], Map, Map) :- !. 
replace_varnames([Var|RestVar], [VarName-_|RestVarNames], [Name|NewRest], Map1, MapN) :-
    var(Var),
    capitalize(VarName, Name), 
    update_map(Var, Name, Map1, Map2), 
    replace_varnames(RestVar, RestVarNames, NewRest, Map2, MapN). 
replace_varnames([V|RestVar], [_|RestVarNames], [V|NewRest], Map1, MapN) :-
    nonvar(V),
    replace_varnames(RestVar, RestVarNames, NewRest, Map1, MapN). 

% from drafter.pl
capitalize(X,NewX) :- 
    name(X,[First|Codes]), to_upper(First,U), name(NewX,[U|Codes]).

write_taxlog_body((A;B), or(NewA,NewB), Map1, MapN) :-
    write_taxlog_body(A, NewA, Map1, Map2),
    write_taxlog_body(B, NewB, Map2, MapN).

write_taxlog_body((A,B), and(NewA, NewB), Map1, MapN) :-
    write_taxlog_body(A, NewA, Map1, Map2),
    write_taxlog_body(B, NewB, Map2, MapN).

write_taxlog_body(Lit, Readable, Map1, MapN) :-
    write_taxlog_literal(Lit, Readable, Map1, MapN). 


%%% ------------------------------------------------ Swish Interface to logical english
%% based on logicalcontracts' lc_server.pl

:- multifile prolog_colour:term_colours/2.
prolog_colour:term_colours(en(_Text),lps_delimiter-[classify]). % let 'en' stand out with other taxlog keywords
prolog_colour:term_colours(en_decl(_Text),lps_delimiter-[classify]). % let 'en_decl' stand out with other taxlog keywords

user:le_taxlog_translate( en(Text), Terms) :- le_taxlog_translate( en(Text), Terms).

le_taxlog_translate( en(Text), Terms) :-
	%findall(Decl, psyntax:lps_swish_clause(en_decl(Decl),_Body,_Vars), Decls),
    %combine_list_into_string(Decls, StringDecl),
	%string_concat(StringDecl, Text, Whole_Text),
	% Can't print here, this was getting sent into a HTTP response...: writeq(Whole_Text),
    once( text_to_logic(Text, Error, Translation) ),
    (Translation=[]-> Terms=Error; Terms = Translation ). 
        %write_taxlog_code(Translation, Terms)). 

combine_list_into_string(List, String) :-
    combine_list_into_string(List, "", String).

combine_list_into_string([], String, String).
combine_list_into_string([HS|RestS], Previous, OutS) :-
    string_concat(Previous, HS, NewS),
    combine_list_into_string(RestS, NewS, OutS).

user:showtaxlog :- showtaxlog.

showtaxlog:-
    % ?????????????????????????????????????????
	% psyntax:lps_swish_clause(en(Text),Body,_Vars),
	once(text_to_logic(_,Error,Taxlog)),
    writeln(Error), 
	writeln(Taxlog),
	fail.
showtaxlog.

% print_message(_, String) :- write(String).

sandbox:safe_primitive(le_to_taxlog:showtaxlog).
sandbox:safe_primitive(le_to_taxlog:le_taxlog_translate( _EnText, _Terms)).
