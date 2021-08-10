/* le_to_taxlog: a prolog module with predicates to translate from an 
extended version of Logical English into the Taxlog programming language. 

Main predicate: text_to_logic(String to be translated, Translation)

Main DCG nonterminal: document(Translation)

See at the end the predicate le_taxlog_translate to be used from SWISH

It assumes an entry with the following structure. One of these expressions:

the predicates are:
the templates are:
the timeless predicates are:
the event predicates are:
the fluents are:
the time-varying predicates are:

followed by the declarations of all the corresponding predicates mentioned in the 
knowledge base. 

Each declarations define a template with the variables and other words required to
describe a relevant relation. It is a comma separated list of templates which ends
with a period. 

After that period, one of the following statement introduces the knowledge base:

the knowledge base includes: 
the knowledge base <Name> includes: 

And it is followed by the rules and facts written in Logical English syntax. 
Each rule must end with a period. 

Indentation is used to organize the and/or list of conditions by strict
observance of one condition per line with a level of indentation that 
correspond to each operator and corresponding conditions. 

Similarly, there may be sections for scenarios and queries, like:

--
scenario test2 is:
   borrower pays a amount to lender on 20150601000000. 
--

and

--
query one is:
for which event:
 the small business restructure rollover applies to the event.

query two is:
 which tax payer is a party of which event.

query three is:
 A first time is after a second time
 and the second time is immediately before the first time.
--

which can then be used in the new command language interface of LE
(e.g. answer/1, is_it_true/2 meta-predicates)


*/

:- module(le_to_taxlog, [document/3, le_taxlog_translate/4, op(1195,fx, user:(++))]).
:- use_module('./tokenize/prolog/tokenize.pl').
:- use_module(library(pengines)).
:- use_module('reasoner.pl').
:- thread_local literal/5, text_size/1, error_notice/4, dict/3, 
                last_nl_parsed/1, kbname/1, happens/2, initiates/3, terminates/3,
                predicates/1, events/1, fluents/1. 
:- discontiguous statement/3, declaration/4.

% Main clause: text_to_logic(+String,-Clauses) is det
% Errors are added to error_notice 
text_to_logic(String_, Translation) :-
    % hack to ensure a newline at the end, for the sake of error reporting:
    ((sub_atom(String_,_,1,0,NL), memberchk(NL,['\n','\r']) ) -> String=String_ ; atom_concat(String_,'\n',String)),
    tokenize(String, Tokens, [cased(true), spaces(true), numbers(false)]),
    retractall(last_nl_parsed(_)), asserta(last_nl_parsed(1)), % preparing line counting
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), !, 
    phrase(document(Translation), CTokens). 
    %( phrase(document(Translation), CTokens) -> 
    %    ( print_message(informational, "Translation: ~w"-[Translation]) )
    %;   ( print_message(informational, "Translation failed: "-[]), Translation=[], fail)). 

document(Translation) --> 
    spaces_or_newlines(_),
    header(Settings), %{print_message(informational, "Settings: ~w"-[Settings]), trace}, 
    %spaces_or_newlines(_),
    %rules_previous,  
    content(Content), 
    {append(Settings, Content, Translation)}.

% header parses all the declarations and assert them into memory to be invoked by the rules. 
header(Settings, In, Next) :-
    length(In, TextSize), % after comments were removed
    ( phrase(settings(Rules, Settings_), In, Next) -> 
        ( member(target(_), Settings_) -> Settings = Settings_ ; Settings = [target(taxlog)|Settings_] )  % taxlog as default
    ; (Rules = [], Settings = [target(taxlog)])), % taxlog as default
    RulesforErrors = % rules for error have been statically added
      [(text_size(TextSize))|Settings], % is text_size being used? % asserting the Settings too! predicates, events and fluents
    append(Rules, RulesforErrors, MRules),
    assertall(MRules). % asserting contextual information

/* --------------------------------------------------------- LE DCGs */

settings(AllR, AllS) --> 
    spaces_or_newlines(_), declaration(Rules,Setting), settings(RRules, RS),
      {append(Setting, RS, AllS), append(Rules, RRules, AllR)}.
settings([],[]) --> [].
 
% content structure: cuts added to avoid search loop
content(T) --> %{print_message(informational, "going for KB:"-[])},  
    spaces_or_newlines(_), rules_previous(Kbname), %{print_message(informational, "KBName: ~w"-[Kbname])}, 
    kbbase_content(S), !, %{print_message(informational, "KB: ~w"-[S])}, 
    content(R), 
    {append([kbname(Kbname)|S], R, T)}.
content(T) --> %{print_message(informational, "going for scenario:"-[])},
    spaces_or_newlines(_), scenario_content(S), !, %{print_message(informational, "scenario: ~w"-[S])},
    content(R), 
    {append(S, R, T)}.
content(T) --> %{print_message(informational, "going for query:"-[])},
    spaces_or_newlines(_), query_content(S), !, content(R), 
    {append(S, R, T)}.
content([]) --> 
    spaces_or_newlines(_), []. 

kbbase_content(T) --> 
    spaces_or_newlines(_),  statement(S),  kbbase_content(R),
    {append(S, R, T)}, !. 
kbbase_content([]) --> 
    spaces_or_newlines(_), [].

% declarations
% target
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [the], spaces(_), [target], spaces(_), [language], spaces(_), [is], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period.
%timeless 
declaration(Rules, [predicates(Relations)]) -->
    predicate_previous, !, list_of_predicates_decl(Rules, Relations).
%events
declaration(Rules, [events(EventTypes)]) -->
    event_predicate_previous, !, list_of_predicates_decl(Rules, EventTypes).
%time varying
declaration(Rules, [fluents(Fluents)]) -->
    fluent_predicate_previous, !, list_of_predicates_decl(Rules, Fluents).

colon_or_not_ --> [':'], spaces(_).
colon_or_not_ --> []. 

predicate_previous --> 
    spaces(_), [the], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
predicate_previous --> 
    spaces(_), [the], spaces(_), [templates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
predicate_previous --> 
    spaces(_), [the], spaces(_), [timeless], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

event_predicate_previous --> 
    spaces(_), [the], spaces(_), [event], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

fluent_predicate_previous --> 
    spaces(_), [the], spaces(_), [fluents], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
fluent_predicate_previous --> 
    spaces(_), [the], spaces(_), [time], ['-'], [varying], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

% at least one predicate declaration required
list_of_predicates_decl([], []) --> spaces_or_newlines(_), next_section, !. 
list_of_predicates_decl([Ru|Rin], [F|Rout]) --> predicate_decl(Ru,F), comma_or_period, list_of_predicates_decl(Rin, Rout).

% a hack to avoid superflous searches
next_section(StopHere, StopHere)  :-
    phrase(predicate_previous, StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(event_predicate_previous, StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(fluent_predicate_previous, StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(rules_previous(_), StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(scenario_, StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(query_, StopHere, _).

predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.

% we are using this resource of the last clause to record the error and its details
% not very useful with loops, of course. 
% error clause
predicate_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a declaration ', Rest), 
    fail.

rules_previous(default) --> 
    spaces_or_newlines(_), [the], spaces(_), [rules], spaces(_), [are], spaces(_), [':'], spaces(_), newline, !.
rules_previous(KBName) --> 
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], extract_constant([includes], NameWords), [includes], spaces(_), [':'], !, spaces(_), newline,
    {name_as_atom(NameWords, KBName)}.
rules_previous(default) -->  % backward compatibility
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], spaces(_), [includes], spaces(_), [':'], spaces(_), newline. 

% a scenario description: assuming one example -> one scenario -> one list of facts.
scenario_content(Scenario) -->
    scenario_, extract_constant([is], NameWords), is_colon_, newline,
    %list_of_facts(Facts), period, !, 
    spaces(_), assumptions_([], _, Assumptions), !, % period is gone
    {name_as_atom(NameWords, Name), Scenario = [example( Name, [scenario(Assumptions, true)])]}.

% statement: the different types of statements in a LE text
% a query
query_content(Query) -->
    query_, extract_constant([is], NameWords), is_colon_, newline,
    query_header(Ind0, Map1),
    conditions(Ind0, Map1, _, Conds), period, !, % period stays!
    {name_as_atom(NameWords, Name), Query = [query(Name, Conds)]}. 

query_content(_, Rest, _) :- 
    asserterror('LE error found around this expression: ', Rest), fail.

% (holds_at(_149428,_149434) if 
% (happens_at(_150138,_150144),
%           initiates_at(_150138,_149428,_150144)),
%           _150144 before _149434,
%           not ((terminates_at(_152720,_149428,_152732),_150144 before _152732),_152732 before _149434))

% it becomes the case that
%   fluent
% when
%   event
% if  
statement(Statement) --> 
    it_becomes_the_case_that_, spaces_or_newlines(_), 
        literal_([], Map1, holds(Fluent, _)), spaces_or_newlines(_), 
    when_, spaces_or_newlines(_), 
        literal_(Map1, Map2, happens(Event, T)), spaces_or_newlines(_),
    body_(Body, Map2,_), period,  
        {(Body = [] -> Statement = [if(initiates(Event, Fluent, T), true)]; 
            (Statement = [if(initiates(Event, Fluent, T), Body)]))}, !.

% it becomes not the case that
%   fluent
% when
%   event
% if  
statement(Statement) --> 
    it_becomes_not_the_case_that_, spaces_or_newlines(_), 
        literal_([], Map1, holds(Fluent, _)), spaces_or_newlines(_),
    when_, spaces_or_newlines(_),
        literal_(Map1, Map2, happens(Event, T)), spaces_or_newlines(_),
    body_(Body, Map2,_), period,  
        {(Body = [] -> Statement = [if(terminates(Event, Fluent, T), true)];  
            (Statement = [if(initiates(Event, Fluent, T), Body)]))}, !.

% it is illegal that
%   event
% if ... 
statement(Statement) -->
    it_is_illegal_that_, spaces_or_newlines(_), 
    literal_([], Map1, happens(Event, T)), body_(Body, Map1, _), period,
    {(Body = [] -> Statement = [if(it_is_illegal(Event, T), true)]; 
      Statement = [if(it_is_illegal(Event, T), Body)])},!. 

% a fact or a rule
statement(Statement) --> 
    literal_([], Map1, Head), body_(Body, Map1, _), period,  
    {(Body = [] -> Statement = [if(Head, true)]; Statement = [if(Head, Body)])}. 

list_of_facts([F|R1]) --> literal_([], _,F), rest_list_of_facts(R1).

rest_list_of_facts(L1) --> comma, spaces_or_newlines(_), list_of_facts(L1).
rest_list_of_facts([]) --> [].

% assumptions
assumptions_(InMap, OutMap, [A|R]) --> 
        spaces_or_newlines(_),  rule_(InMap, Map2, A), !, assumptions_(Map2, OutMap, R).
assumptions_(Map, Map, []) --> 
        spaces_or_newlines(_), []. 

rule_(InMap, OutMap, Rule) --> 
    literal_(InMap, Map1, Head), body_(Body, Map1, OutMap), period,  
    %spaces(Ind), condition(Head, Ind, InMap, Map1), body_(Body, Map1, OutMap), period, 
    {(Body = [] -> Rule = (Head :-true); Rule = (Head :- Body))}. 

rule_(M, M, _, Rest, _) :- 
    asserterror('LE error found in an assumption, near to ', Rest), fail.

% no prolog inside LE!
%statement([Fact]) --> 
%    spaces(_), prolog_literal_(Fact, [], _), spaces_or_newlines(_), period.

body_([], Map, Map) --> spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    newline, spaces(Ind), if_, !, conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    if_, newline_or_nothing, spaces(Ind), conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).

newline_or_nothing --> newline.
newline_or_nothing --> []. 

% literal_ reads a list of words until it finds one of these: ['\n', if, '.']
% it then tries to match those words against a template in memory (see dict/3 predicate).
% The output is then contigent to the type of literal according to the declarations. 
literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'at time, literal') },
    at_time(T, Map1, Map2), comma, predicate_template(PossibleTemplate),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'literal, at time') },
    predicate_template(PossibleTemplate), comma, at_time(T, Map1, Map2),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'just literal') },
    predicate_template(PossibleTemplate),
    {match_template(PossibleTemplate, Map1, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, _T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, _T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

% rewritten to use in swish. Fixed! It was a name clash. Apparently "literal" is used somewhere else
%literal_(Map1, MapN, Literal, In, Out) :-  print_message(informational, '  inside a literal'),
%        predicate_template(PossibleTemplate, In, Out), print_message(informational, PossibleTemplate),
%        match_template(PossibleTemplate, Map1, MapN, Literal).
% error clause
literal_(M, M, _, Rest, _) :- 
    asserterror('LE error found in a literal ', Rest), fail.

conditions(Ind0, Map1, MapN, Conds) --> 
    condition(Cond, Ind0, Map1, Map2), 
    more_conds(Ind0, Ind0, _IndF, Map2, MapN, Cond, Conds).
    %{Ind0=<IndF}. % 

% three conditions look ahead
more_conds(Ind0, Ind1, Ind4, Map1, MapN, C1, RestMapped, In1, Out) :-
    newline(In1, In2), spaces(Ind2, In2, In3), Ind0=<Ind2, operator(Op1, In3, In4), condition(C2, Ind1, Map1, Map2, In4, In5), 
    newline(In5, In6), spaces(Ind3, In6, In7), Ind0=<Ind3, operator(Op2, In7, In8), condition(C3, Ind2, Map2, Map3, In8, In9), 
    adjust_op(Ind2, Ind3, C1, Op1, C2, Op2, C3, Conditions), !, 
    more_conds(Ind0, Ind3, Ind4, Map3, MapN, Conditions, RestMapped, In9, Out). 
% more_conds(PreviosInd, CurrentInd, MapIn, MapOut, InCond, OutConds)
more_conds(Ind0, Ind1, Ind, Map1, MapN, Cond, Conditions) --> 
    newline, spaces(Ind), {Ind0 =< Ind}, % if the new indentation is deeper, it goes on as before. 
    operator(Op), condition(Cond2, Ind, Map1, MapN), 
    {add_cond(Op, Ind1, Ind, Cond, Cond2, Conditions)}, !.
more_conds(_, Ind, Ind, Map, Map, Cond, Cond, Rest, Rest).  
 
% this naive definition of term is problematic
term_(Term, Map1, MapN) --> 
    (variable(Term, Map1, MapN), !); (constant(Term, Map1, MapN), !); (list_(Term, Map1, MapN), !). %; (compound_(Term, Map1, MapN), !).

list_(List, Map1, MapN) --> 
    spaces(_), bracket_open_, !, extract_list([']'], List, Map1, MapN), bracket_close.   

compound_(V1/V2, Map1, MapN) --> 
    term_(V1, Map1, Map2), ['/'], term_(V2, Map2, MapN). 

% event observations
condition(happens(Event), _, Map1, MapN) -->
    observe_,  literal_(Map1, MapN, Event), !.

% this produces a Taxlog condition with the form: 
% setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on Before, SetOfPreviousOwners)
% from a set of word such as: 
%     and a set of previous owners is a collection of an owner / a share 
%           where the asset is ultimately owned by the share with the owner at the previous time
condition(FinalExpression, _, Map1, MapN) --> 
    %variable(Set, Map1, Map2), is_a_collection_of_, term_(Term, Map2, Map3), where_,
    variable(Set, Map1, Map2), is_a_collection_of_, literal_(Map2, Map3, Term), !, % moved where to the following line
    newline, spaces(Ind2), where_, conditions(Ind2, Map3, Map4, Goals),
    modifiers(setof(Term,Goals,Set), Map4, MapN, FinalExpression).

% for every a party is a party in the event, it is the case that:
condition(FinalExpression, _, Map1, MapN) -->  
    for_all_cases_in_which_, newline, !, 
    spaces(Ind2), conditions(Ind2, Map1, Map2, Conds), spaces_or_newlines(_), 
    it_is_the_case_that_colon_, newline, 
    spaces(Ind3), conditions(Ind3, Map2, Map3, Goals),
    modifiers(forall(Conds,Goals), Map3, MapN, FinalExpression).

% the Value is the sum of each Asset Net such that
condition(FinalExpression, _, Map1, MapN) --> 
    variable(Value, Map1, Map2), is_the_sum_of_each_, extract_variable([such], Each, NameWords, _), such_that_, !, 
    { name_predicate(NameWords, Name), update_map(Each, Name, Map2, Map3) }, newline, 
    spaces(Ind), conditions(Ind, Map3, Map4, Conds), 
    modifiers(aggregate_all(sum(Each),Conds,Value), Map4, MapN, FinalExpression).
    
% it is not the case that 
condition(not(Conds), _, Map1, MapN) --> 
    spaces(_), not_, newline,  !, % forget other choices. We know it is a not case
    spaces(Ind), conditions(Ind, Map1, MapN, Conds).

condition(Cond, _, Map1, MapN) -->  
    literal_(Map1, MapN, Cond), !. 

%condition(assert(Prolog), _, Map1, MapN) -->
%    this_information_, !, prolog_literal_(Prolog, Map1, MapN), has_been_recorded_. 

% condition(-Cond, ?Ind, +InMap, -OutMap)
condition(InfixBuiltIn, _, Map1, MapN) --> 
    term_(Term, Map1, Map2), spaces(_), builtin_(BuiltIn), !, 
    spaces(_), expression_(Expression, Map2, MapN), {InfixBuiltIn =.. [BuiltIn, Term, Expression]}. 

% error clause
condition(_, _Ind, Map, Map, Rest, _) :- 
        asserterror('LE error found at a condition ', Rest), fail.

% modifiers add reifying predicates to an expression. 
% modifiers(+MainExpression, +MapIn, -MapOut, -FinalExpression)
modifiers(MainExpression, Map1, MapN, on(MainExpression, Var) ) -->
    newline, spaces(_), at_, variable(Var, Map1, MapN). % newline before a reifying expression
modifiers(MainExpression, Map, Map, MainExpression) --> [].  

variable(Var, Map1, MapN) --> 
    spaces(_), [Det], {determiner(Det)}, extract_variable([], Var, NameWords, _), !, % <-- CUT!
    { name_predicate(NameWords, Name), update_map(Var, Name, Map1, MapN) }. 
% allowing for symbolic variables: 
variable(Var, Map1, MapN) --> 
    spaces(_), extract_variable([], Var, NameWords, _),
    { name_predicate(NameWords, Name), consult_map(Var, Name, Map1, MapN) }. 

constant(Constant, Map, Map) -->
    extract_constant([], NameWords), { NameWords\=[], name_predicate(NameWords, Constant) }. 

prolog_literal_(Prolog, Map1, MapN) -->
    predicate_name_(Predicate), parentesis_open_, extract_list([], Arguments, Map1, MapN), parentesis_close_,
    {Prolog =.. [Predicate|Arguments]}.

predicate_name_(Module:Predicate) --> 
    [Module], colon_, extract_constant([], NameWords), { name_predicate(NameWords, Predicate) }, !.
predicate_name_(Predicate) --> extract_constant([], NameWords), { name_predicate(NameWords, Predicate) }.

at_time(T, Map1, MapN) --> spaces_or_newlines(_), at_, expression_(T, Map1, MapN), spaces_or_newlines(_).

spaces(N) --> [' '], !, spaces(M), {N is M + 1}.
spaces(N) --> ['\t'], !, spaces(M), {N is M + 1}. % counting tab as one space
spaces(0) --> []. 

spaces_or_newlines(N) --> [' '], !, spaces_or_newlines(M), {N is M + 1}.
spaces_or_newlines(N) --> ['\t'], !, spaces_or_newlines(M), {N is M + 1}. % counting tab as one space
spaces_or_newlines(N) --> newline, !, spaces_or_newlines(M), {N is M + 1}. % counting \r as one space
spaces_or_newlines(0) --> [].

newline --> [newline(_Next)].

one_or_many_newlines --> newline, spaces(_), one_or_many_newlines, !. 
one_or_many_newlines --> [].

if_ --> [if], spaces_or_newlines(_).  % so that if can be written many lines away from the rest

period --> ['.'].
comma --> [','].
colon_ --> [':'], spaces(_). 

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

scenario_ -->  spaces_or_newlines(_), ['Scenario'], !, spaces(_).
scenario_ -->  spaces_or_newlines(_), [scenario], spaces(_). 

is_colon_ -->  [is], spaces(_), [':'], spaces(_).

query_ --> spaces_or_newlines(_), [query], spaces(_).

for_which_ --> [for], spaces(_), [which], spaces(_). 

query_header(Ind, Map) --> spaces(Ind), for_which_, list_of_vars([], Map), colon_, newline.
query_header(0, []) --> []. 

list_of_vars(Map1, MapN) --> 
    extract_variable([',', and, ':'], Var, NameWords, _), 
    { name_predicate(NameWords, Name), update_map(Var, Name, Map1, Map2) },
    rest_of_list_of_vars(Map2, MapN).

rest_of_list_of_vars(Map1, MapN) --> and_or_comma_, list_of_vars(Map1, MapN).
rest_of_list_of_vars(Map, Map) --> []. 

and_or_comma_ --> [','], spaces(_). 
and_or_comma_ --> [and], spaces(_).

it_becomes_the_case_that_ --> 
    it_, [becomes], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).

it_becomes_not_the_case_that_ -->
    it_, [becomes], spaces(_), [not], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).
it_becomes_not_the_case_that_ -->
    it_, [becomes], spaces(_), [no], spaces(_), [longer], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).

when_ --> [when], spaces(_).

it_ --> [it], spaces(_), !.
it_ --> ['It'], spaces(_). 

observe_ --> [observe], spaces(_). 

it_is_illegal_that_  -->
    it_, [is], spaces(_), [illegal], spaces(_), [that], spaces(_).

/* --------------------------------------------------- Supporting code */
clean_comments([], []) :- !.
clean_comments(['%'|Rest], New) :- % like in prolog comments start with %
    jump_comment(Rest, Next), 
    clean_comments(Next, New). 
clean_comments([Code|Rest], [Code|New]) :-
    clean_comments(Rest, New).

jump_comment([], []).
jump_comment([newline(N)|Rest], [newline(N)|Rest]). % leaving the end of line in place
jump_comment([_|R1], R2) :-
    jump_comment(R1, R2). 

% cuts added to improve efficiency
template_decl(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\t'|RestIn], Out) :- !, % skip cntrl \t in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, [newline(_)|RestIn], Out) :- !, % skip cntrl \n in template
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
build_template_elements([], _, [], [], [], []) :- !. 
% a variable signalled by a *
build_template_elements(['*', Word|RestOfWords], _Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), % Previous \= [is|_], % removing this requirement when * is used
    extract_variable(['*'], Var, NameWords, TypeWords, RestOfWords, ['*'|NextWords]), !, % <-- it must end with * too
    name_predicate(NameWords, Name), 
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate). 
% a variable not signalled by a *    
build_template_elements([Word|RestOfWords], Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable(['*'], Var, NameWords, TypeWords, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], Previous, RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, [Word|Previous], RestVars, RestTypes, Others, RestTemplate).

% extract_variable(+StopWords, -Var, -ListOfNameWords, -ListOfTypeWords, +ListOfWords, -NextWordsInText)
% refactored as a dcg predicate
extract_variable(_, _, [], [], [], []) :- !.                                % stop at when words run out
extract_variable(StopWords, _, [], [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    (member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
extract_variable(SW, Var, RestName, RestType, [' '|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable(SW, Var, RestName, RestType, RestOfWords, NextWords).
extract_variable(SW, Var, RestName, RestType, ['\t'|RestOfWords], NextWords) :- !,  % skipping spaces
    extract_variable(SW, Var, RestName, RestType, RestOfWords, NextWords).  
extract_variable(SW, Var, [Word|RestName], Type, [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
    ordinal(Word), !, 
    extract_variable(SW, Var, RestName, Type, RestOfWords, NextWords).
extract_variable(SW, Var, [Word|RestName], [Word|RestType], [Word|RestOfWords], NextWords) :-
    is_a_type(Word),
    extract_variable(SW, Var, RestName, RestType, RestOfWords, NextWords).

%name_predicate([AtomNum,'.',AtomDecimal], Number) :-  
%    atomic_list_concat([AtomNum,'.',AtomDecimal], Atom), atom_number(Atom, Number). 
%name_predicate([Atom, '-'|Rest], Number-NRest) :- 
%    atom_number(Atom, Number), name_predicate(Rest, NRest), !. 
% 2021-02-06T08:25:34
%name_predicate([Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], Date) :-
%    concat_atom([Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], '', Date), !.
%name_predicate([Year,'-', Month, '-', Day], Date) :-
%    concat_atom([Year, Month, Day], '', Date), !.
%name_predicate([Atom], Number) :- atom_number(Atom, Number), number(Number), !. % a quick fix for numbers extracted as constants
name_predicate(Words, Predicate) :-
    concat_atom(Words, '_', Predicate). 

name_as_atom([Number], Number) :-
    number(Number), !. 
name_as_atom([Atom], Number) :- 
    atom_number(Atom, Number), !. 
name_as_atom(Words, Name) :-
    replace_vars(Words, Atoms), concat_atom(Atoms, ' ', Name). 

replace_vars([],[]) :- !.
replace_vars([W|RI], [A|RO]) :- term_to_atom(W, A), replace_vars(RI,RO).   

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
% skipping a list
predicate_template([], [], []) :- !. 
predicate_template(Final, ['['|RestIn], Out) :- !, 
    predicate_template_for_lists(List, RestIn, Next),  
    predicate_template(RestW, Next, Out),
    append(['['|List], RestW, Final).  
predicate_template(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    predicate_template(RestW, RestIn, Out).
predicate_template(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    predicate_template(RestW, RestIn, Out).
predicate_template([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    not(lists:member(Word,[newline(_), if, '.', ','])), 
    % leaving the comma in as well (for lists and sets we will have to modify this)
    predicate_template(RestW, RestIn, Out).
predicate_template([], [Word|Rest], [Word|Rest]) :- 
    lists:member(Word,[newline(_), if, '.', ',']). % leaving or/and out of this

% using [ and ] for list and set only
predicate_template_for_lists([], [], []) :- !.
predicate_template_for_lists([']'], [']'|Out], Out) :- !. 
predicate_template_for_lists(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    predicate_template_for_lists(RestW, RestIn, Out).
predicate_template_for_lists(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    predicate_template_for_lists(RestW, RestIn, Out).
predicate_template_for_lists([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    predicate_template_for_lists(RestW, RestIn, Out).
predicate_template_for_lists([], [Word|Rest], [Word|Rest]) :- 
    lists:member(Word,[',', newline(_), if, '.']). % leaving or/and out of this

match_template(PossibleLiteral, Map1, MapN, Literal) :- 
    dictionary(Predicate, _, Candidate),
    match(Candidate, PossibleLiteral, Map1, MapN, Template), 
    dictionary(Predicate, _, Template), 
    Literal =.. Predicate, !. 
    %print_message(informational,'Match!! with ~w'-[Literal]), !. 

% match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
match([], [], Map, Map, []) :- !.  % success! It succeds iff PossibleLiteral is totally consumed
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
    nonvar(Element), Word = Element, 
    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    determiner(Det), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    match(RestElements, NextWords, Map2, MapN, RestSelected). 
% handling symbolic variables (as long as they have been previously defined and included in the map!) 
match([Element|RestElements], PossibleLiteral, Map1, MapN, [Var|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !, % <-- CUT!  % if the variables has been previously registered
    match(RestElements, NextWords, Map2, MapN, RestSelected).
match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [List|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_list(StopWords, List, Map1, Map2, PossibleLiteral, [']'|NextWords]), !, % matching brackets verified
    match(RestElements, NextWords, Map2, MapN, RestSelected).
% enabling expressions and constants
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Expression|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_expression(StopWords, NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [], 
    ( phrase(expression_(Expression, Map1, Map2), NameWords) -> true ; ( Map1 = Map2, name_predicate(NameWords, Expression) ) ),
    %print_message(informational, 'found a constant or an expression '), print_message(informational, Expression),
    match(RestElements, NextWords, Map2, MapN, RestSelected). 

% expression_ resolve simple math (non boolean) expressions fttb. 
%expression_((X - Y), Map1, MapN) --> 
%    term_(X, Map1, Map2), spaces(_), minus_, spaces(_), expression(Y, Map2, MapN), spaces(_), !. 
%expression_((X + Y), Map1, MapN) --> 
%    term_(X, Map1, Map2), spaces(_), plus_, spaces(_), expression(Y, Map2, MapN), spaces(_), !. 
% dates must be dealt with first  
% 2021-02-06T08:25:34
expression_(Date, Map, Map) --> 
    [Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], spaces(_),
    { concat_atom([Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], '', Date) }, !.
% 2021-02-06
expression_(Date, Map, Map) -->  [Year,'-', Month, '-', Day],  spaces(_),
    { concat_atom([Year, Month, Day], '', Date) }, !. 
% basic float
expression_(Float, Map, Map) --> [AtomNum,'.',AtomDecimal],
        { atom(AtomNum), atom(AtomDecimal), atomic_list_concat([AtomNum,'.',AtomDecimal], Atom), atom_number(Atom, Float) }, !.
% mathematical expressions
expression_(InfixBuiltIn, Map1, MapN) --> 
    term_(Term, Map1, Map2), spaces(_), binary_op(BuiltIn), !, 
    spaces(_), expression_(Expression, Map2, MapN), spaces(_), {InfixBuiltIn =.. [BuiltIn, Term, Expression]}.  
% a quick fix for numbers extracted as constants
expression_(Number, Map, Map) --> [Atom],  spaces(_), { atom(Atom), atom_number(Atom, Number) }, !. 
expression_(Var, Map1, Map2) -->   
    extract_variable([], Var, NameWords, _), spaces(_),
    { NameWords \= [], name_predicate(NameWords, Name),  consult_map(Var, Name, Map1, Map2)}, !. 
expression_(Constant, Map1, Map2) -->  constant(Constant, Map1, Map2).     
% error clause
expression(_, _, _, Rest, _) :- 
    asserterror('LE error found in an expression ', Rest), fail.

% only one word operators
binary_op(Op) --> [Op], { atom(Op), current_op(_Prec, Fix, Op),
    Op \= '.',
    (Fix = 'xfx'; Fix='yfx'; Fix='xfy'; Fix='yfy') }.

stop_words([], []).
stop_words([Word|_], [Word]) :- nonvar(Word). % only the next word for now
stop_words([Word|_], []) :- var(Word).

% extract_expression(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
% it does not stop at reserved words!
extract_expression(_, [], [], []) :- !.                                % stop at when words run out
extract_expression(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at  verbs? or prepositions?. 
    (member(Word, StopWords); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
%extract_expression([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_expression(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_expression(SW, RestName, RestOfWords, NextWords).
extract_expression(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_expression(SW, RestName, RestOfWords, NextWords).
extract_expression(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    %not(determiner(Word)), % no determiners inside constants!
    extract_expression(SW, RestName, RestOfWords, NextWords).

% extract_constant(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
extract_constant(_, [], [], []) :- !.                                % stop at when words run out
extract_constant(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs? or prepositions?. 
    (member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
%extract_constant([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    not(determiner(Word)), % no determiners inside constants!
    extract_constant(SW, RestName, RestOfWords, NextWords).

% extract_list(+StopWords, -List, +Map1, -Map2, +[Word|PossibleLiteral], -NextWords),
extract_list(_, [], Map, Map, [']'|Rest], [']'|Rest]) :- !. % stop but leave the symbol for further verification
extract_list(_, [], Map, Map, [')'|Rest], [')'|Rest]) :- !. 
extract_list(SW, RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, [','|RestOfWords],  NextWords) :- !, 
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(StopWords, [Var|RestList], Map1, MapN, [Det|InWords], LeftWords) :-
    determiner(Det), 
    extract_variable(StopWords, Var, NameWords, _, InWords, NextWords), NameWords \= [], % <-- CUT! % <- leave that _ unbound!
    name_predicate(NameWords, Name),  !,
    update_map(Var, Name, Map1, Map2),
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords).
extract_list(StopWords, [Member|RestList], Map1, MapN, InWords, LeftWords) :-
    extract_constant(StopWords, NameWords, InWords, NextWords), NameWords \= [],
    name_predicate(NameWords, Member),
    extract_list(StopWords, RestList, Map1, MapN, NextWords, LeftWords).

determiner(Det) :-
    (ind_det(Det); ind_det_C(Det); def_det(Det); def_det_C(Det)), !. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, [], Template).

% template_elements(+Input,+InMap, -OutMap, +Previous, -Template)
template_elements([], Map1, Map1, _, []).     
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable([], Var, NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (def_det_C(Word); def_det(Word)), Previous \= [is|_], 
    extract_variable([], Var, NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
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
    member(map(Var, SomeName), InMap), (Name = SomeName -> Var = V; ( Var == V -> Name = SomeName ; fail ) ),  !.  
%consult_map(V, V, Map, Map). % leave the name unassigned % deprecated to be used inside match

builtin_(BuiltIn, [BuiltIn1, BuiltIn2|RestWords], RestWords) :- 
    atom_concat(BuiltIn1, BuiltIn2, BuiltIn), 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in), !.
builtin_(BuiltIn, [BuiltIn|RestWords], RestWords) :- 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in). 

/* --------------------------------------------------------- Utils in Prolog */
time_of(P, T) :- P=..[_|Arguments], lists:append(_, [T], Arguments). % it assumes time as the last argument

% Unwraps tokens, excelt for newlines which become newline(NextLineNumber)
unpack_tokens([], []).
unpack_tokens([cntrl(Char)|Rest], [newline(Next)|NewRest]) :- (Char=='\n' ; Char=='\r'), !,
    %not sure what will happens on env that use \n\r
    update_nl_count(Next), unpack_tokens(Rest, NewRest).
unpack_tokens([First|Rest], [New|NewRest]) :-
    (First = word(New); First=cntrl(New); First=punct(New); First=space(New); First=number(New); First=string(New)), 
     !,
    unpack_tokens(Rest, NewRest).  

% increments the next line number
update_nl_count(NN) :- retract(last_nl_parsed(N)), !, NN is N + 1, assert(last_nl_parsed(NN)). 

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
ind_det_C('Which').  % added experimentally

def_det_C('The').

ind_det(a).
ind_det(an).
ind_det(another). % added experimentally
ind_det(which).  % added experimentally
% ind_det(some).

def_det(the).

/* ------------------------------------------------ reserved words */
reserved_word(W) :- % more reserved words pending??
    W = 'is'; W ='not'; W='if'; W='If'; W='then'; W = 'where';  W = '&'; % <- hack!
    W = 'at'; W= 'from'; W='to';  W='half'; % W='or'; W='and'; % leaving and/or out of this for now
    W = 'else'; W = 'otherwise'; 
    W = such ; 
    W = '<'; W = '='; W = '>';  W = '+'; W = '-'; W = '/'; W = '*'; % these are handled by extract_expression
    W = '{' ; W = '}' ; W = '(' ; W = ')' ; W = '[' ; W = ']',
    W = ':', W = ','; W = ';'. % these must be handled by parsing
reserved_word(P) :- punctuation(P).

/* ------------------------------------------------ punctuation */
%punctuation(punct(_P)).

punctuation('.').
punctuation(',').
punctuation(';').
%punctuation(':').
punctuation('\'').

/* ------------------------------------------------ verbs */
verb(Verb) :- present_tense_verb(Verb); continuous_tense_verb(Verb); past_tense_verb(Verb). 

present_tense_verb(is).
present_tense_verb(complies). 
present_tense_verb(does). 
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
present_tense_verb(must).
present_tense_verb(acts).
present_tense_verb(falls).
present_tense_verb(corresponds). 
present_tense_verb(likes). 

continuous_tense_verb(according).
continuous_tense_verb(beginning).
continuous_tense_verb(ending).

past_tense_verb(spent). 
past_tense_verb(looked).
past_tense_verb(could).
past_tense_verb(had).
past_tense_verb(tried).
past_tense_verb(explained).
past_tense_verb(ocurred).
 
/* ------------------------------------------------- prepositions */
preposition(of).
%preposition(on).
preposition(from).
preposition(to).
preposition(at).
preposition(in).
preposition(with).
preposition(plus).
preposition(as).
preposition(by).

/* ------------------------------------------------- memory handling */
assertall([]).
assertall([F|R]) :-
    not(asserted(F)),
    assertz(F), !,
    assertall(R).
assertall([_F|R]) :-
    assertall(R).

asserted(F :- B) :- clause(F, B). % as a rule with a body
asserted(F) :- clause(F,true). % as a fact

/* -------------------------------------------------- error handling */
asserterror(Me, Rest) :-
    %print_message(error, ' Error found'), 
    %select_first_section(Rest, 40, Context), 
    %retractall(error_notice(_,_,_,_)), % we will report only the last
    once( nth1(N,Rest,newline(NextLine)) ), LineNumber is NextLine-2,
    RelevantN is N-1,
    length(Relevant,RelevantN), append(Relevant,_,Rest),
    findall(Token, (member(T,Relevant), (T=newline(_) -> Token='\n' ; Token=T)), Tokens),
    asserta(error_notice(error, Me, LineNumber, Tokens)). % asserting the last first!

% to select just a chunck of Rest to show. 
select_first_section([], _, []) :- !.
select_first_section(_, 0, []) :- !. 
select_first_section([E|R], N, [E|NR]) :-
    N > 0, NN is N - 1,
    select_first_section(R, NN, NR). 

showErrors(File,Baseline) :- % showing only the last message!
    error_notice(error, Me,Pos, ContextTokens), 
    atomic_list_concat([Me,': '|ContextTokens],ContextTokens_),
    Line is Pos+Baseline,
    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char))).
    % to show them all
    %forall(error_notice(error, Me,Pos, ContextTokens), (
    %    atomic_list_concat([Me,': '|ContextTokens],ContextTokens_),
    %    Line is Pos+Baseline,
    %    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char)))
    %    )).

% to pinpoint exactly the error. But position is not right
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

% dictionary(?LiteralElements, ?NamesAndTypes, ?Template)
% this is a multimodal predicate used to associate a Template with its particular other of the words for LE
% with the Prolog expression of that relation in LiteralElements (not yet a predicate, =.. is done elsewhere).
% NamesAndTypes contains the external name and type (name-type) of each variable just in the other in 
% which the variables appear in LiteralElement. 
dictionary(Predicate, VariablesNames, Template) :- 
    predef_dict(Predicate, VariablesNames, Template); dict(Predicate, VariablesNames, Template).

:- discontiguous predef_dict/3.
% predefined entries:
predef_dict([member, Member, List], [member-object, list-list], [Member, is, in, List]).
predef_dict([assert,Information], [info-clause], [this, information, Information, ' has', been, recorded]).
predef_dict([is_a, Object, Type], [object-object, type-type], [Object, is, of, type, Type]).
predef_dict([before, T1, T2], [time1-time, time2-time], [T1, is, before, T2]). % see reasoner.pl before/2
predef_dict([after, T1, T2], [time1-time, time2-time], [T1, is, after, T2]).  % see reasoner.pl before/2
predef_dict([is_not_before, T1, T2], [time1-time, time2-time], [T1, is, not, before, T2]). % see reasoner.pl before/2
predef_dict([immediately_before, T1, T2], [time1-time, time2-time], [T1, is, immediately, before, T2]). % see reasoner.pl before/2
predef_dict([same_date, T1, T2], [time1-time, time2-time], [T1, is, the, same, date, as, T2]). % see reasoner.pl before/2
predef_dict([=, T1, T2], [time1-time, time2-time], [T1, is, equal, to, T2]).
predef_dict([\=, T1, T2], [time1-time, time2-time], [T1, is, different, from, T2]).
%predef_dict([=<, T1, T2], [time1-time, time2-time], [T1, =, <, T2]).
%predef_dict([>=, T1, T2], [time1-time, time2-time], [T1, >, =, T2]).
predef_dict([between,Minimum,Maximum,Middle], [min-date, max-date, middle-date], 
    [Middle, is, between, Minimum, &, Maximum]).
predef_dict([must_be, Type, Term], [type-type, term-term], [Term, must, be, Type]).
predef_dict([must_not_be, A, B], [term-term, variable-variable], [A, must, not, be, B]). 
predef_dict([myDB_entities:is_individual_or_company_on, A, B],
                  [affiliate-affiliate, date-date],
                  [A, is, an, individual, or, is, a, company, at, B]).
predef_dict([uk_tax_year_for_date,Date,Year,Start,End], [first_date-date, year-year, second_date-date, third_date-date], 
                    [in, the, 'UK', Date, falls, in, Year, beginning, at, Start, &, ending, at, End]).
predef_dict([days_spent_in_uk,Individual,Start,End,TotalDays], [who-person,start-date,end-date,total-number], 
                    [Individual, spent, TotalDays, in, the, 'UK', starting, at, Start, &, ending, at, End]). 

% support predicates
must_be(A, var) :- var(A).
must_be(A, nonvar) :- nonvar(A).
must_be_nonvar(A) :- nonvar(A).
must_not_be(A,B) :- not(must_be(A,B)). 

% see reasoner.pl
%before(Year1-(_-(_-(_-(_-_)))), Year2-(_-(_-(_-(_-_))))) :- nonvar(Year1), nonvar(Year2), Year1 < Year2, !. 
%before(Year-(Month1-(_-(_-(_-_)))), Year-(Month2-(_-(_-(_-_))))) :- nonvar(Month1), nonvar(Month2), Month1 < Month2, !. 
%before(Year-(Month-(Day1-(_-(_-_)))), Year-(Month-(Day2-(_-(_-_))))) :- nonvar(Day1), nonvar(Day2), Day1 < Day2, !. 
%before(Year-(Month-(Day-(Hour1-(_-_)))), Year-(Month-(Day-(Hour2-(_-_))))) :- nonvar(Hour1), nonvar(Hour2), Hour1 < Hour2, !.
%before(Year-(Month-(Day-(Hour-(Min1-_)))), Year-(Month-(Day-(Hour-(Min2-_))))) :-nonvar(Min1), nonvar(Min2), Min1 < Min2, !.
%before(Year-(Month-(Day-(Hour-(Min-Sec1)))), Year-(Month-(Day-(Hour-(Min-Sec2))))) :- nonvar(Sec1), nonvar(Sec2), Sec1 < Sec2, !.
%before(A,B) :- nonvar(A), nonvar(B), number(A), number(B), A < B. 

/* ---------------------------------------------------------------  meta predicates CLI */

is_it_illegal(English, Scenario) :- % only event as possibly illegal for the time being
    translate_query(English, happens(Goal, T)), % later -->, Kbs),
    %print_message(informational, "Goal Name: ~w"-[GoalName]),
    pengine_self(SwishModule), %SwishModule:query(GoalName, Goal), 
    %extract_goal_command(Question, SwishModule, Goal, Command), 
    copy_term(Goal, CopyOfGoal), 
    get_answer_from_goal(CopyOfGoal, EnglishQuestion), 
    print_message(informational, "Testing illegality: ~w"-[EnglishQuestion]),
    print_message(informational, "Scenario: ~w"-[Scenario]),
    get_assumptions_from_scenario(Scenario, SwishModule, Assumptions), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Assumptions), 
            %catch(SwishModule:holds(Goal), Error, ( print_message(error, Error), fail ) ), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            catch(SwishModule:it_is_illegal(Goal, T), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Assumptions)), 
    get_answer_from_goal(Goal, EnglishAnswer),  
    print_message(informational, "Answers: ~w"-[EnglishAnswer]).

extract_goal_command(holds(Goal,T), _, Goal, le_to_taxlog:holds(Goal,T)) :- !.
extract_goal_command(happens(Goal,T), M, Goal, M:happens(Goal,T)) :- !.
extract_goal_command(Goal, M, Goal, M:Goal).

get_assumptions_from_scenario(noscenario, _, []) :- !.  
get_assumptions_from_scenario(Scenario, SwishModule, Assumptions) :-
    SwishModule:example(Scenario, [scenario(Assumptions, _)]).

translate_query(English_String, Goal) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    phrase(literal_([], _, Goal), CTokens) -> true 
    ; ( error_notice(error, Me,Pos, ContextTokens), print_message(error, [Me,Pos,ContextTokens]) ). 

/* ----------------------------------------------------------------- Event Calculus  */

holds(Fluent, T) :- print_message(informational, "It is true ~w at ~w ?"-[Fluent, T]), 
    pengine_self(SwishModule),
    SwishModule:happens(Event, T1), 
    SwishModule:initiates(Event, Fluent, T1), 
    (nonvar(T) -> T1 =< T; true ), 
    not(interrupted(T1, Fluent, T)), !.

% temporary hack while we find a way to signal fluents from the rest
%holds(NoFluent) :- 
%    pengine_self(SwishModule),
%   call(SwishModule:NoFluent). 

interrupted(T1, Fluent, T) :-
    pengine_self(SwishModule),
    SwishModule:happens(Event2, T2), 
    SwishModule:terminates(Event2, Fluent, T2), T1 =< T2, 
    (nonvar(T) -> T2 =< T; true ).  

happens(it_is_the_end_of(T), _) :- T = _-_-_-00-00-00.  % needed?

/* ----------------------------------------------------------------- CLI English */
answer(English) :-  
    translate_command(English, GoalName, Scenario), % later -->, Kbs),
    %print_message(informational, "Goal Name: ~w"-[GoalName]),
    pengine_self(SwishModule), SwishModule:query(GoalName, Goal), 
    copy_term(Goal, CopyOfGoal), 
    get_answer_from_goal(CopyOfGoal, EnglishQuestion), 
    print_message(informational, "Question: ~w"-[EnglishQuestion]),
    print_message(informational, "Scenario: ~w"-[Scenario]),
    % assert facts in scenario
    (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), 
    %print_message(informational, "Facts: ~w"-[Facts]), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch(SwishModule:Goal, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)), 
    %reasoner:query_once_with_facts(Goal,Scenario,_,E,Result),
    %reasoner:query_once_with_facts(at(Goal,'http://tests.com'),Scenario,_,E,Result),
    %query_with_facts(at(Goal,'http://tests.com'),Scenario,_,_,Result),
    get_answer_from_goal(Goal, EnglishAnswer), 
    print_message(informational, "Answers: ~w"-[EnglishAnswer]).
    %print_message(informational, "Result: ~w"-[Result]).
    %print_message(informational, "Explanation: ~w"-[E]).

% answer(+English, -Goal, -Result)
answer(English, EnglishQuestion, Answers) :-
    translate_command(English, GoalName, Scenario), % later -->, Kbs),
    pengine_self(SwishModule), SwishModule:query(GoalName, Goal),
    copy_term(Goal, CopyOfGoal), 
    get_answer_from_goal(CopyOfGoal, EnglishQuestion), 
    (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch(SwishModule:Goal, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)),
    get_answer_from_goal(Goal, Answers). 
    %reasoner:query_once_with_facts(Goal,Scenario,_,_E,Result).

get_answer_from_goal((G,R), [Answer|RestAnswers]) :- 
    G =.. GoalElements, dict(GoalElements, _, WordsAnswer), name_as_atom(WordsAnswer, Answer),
    get_answer_from_goal(R, RestAnswers).
get_answer_from_goal(Goal, [Answer]) :-
    Goal =.. GoalElements, dict(GoalElements, _, WordsAnswer), name_as_atom(WordsAnswer, Answer). 

assert_facts(_, []) :- !. 
assert_facts(SwishModule, [F|R]) :- nonvar(F), % print_message(informational, "asserting: ~w"-[SwishModule:F]),
    asserta(SwishModule:F), assert_facts(SwishModule, R).

retract_facts(_, []) :- !. 
retract_facts(SwishModule, [F|R]) :- nonvar(F), % print_message(informational, "retracting: ~w"-[SwishModule:F]),
    retract(SwishModule:F), retract_facts(SwishModule, R). 

translate_command(English_String, Goal, Scenario) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    phrase(command_(Goal, Scenario), CTokens) -> true 
    ; ( error_notice(error, Me,Pos, ContextTokens), print_message(error, [Me,Pos,ContextTokens]) ). 

command_(Goal, Scenario) --> 
    %order_, goal_(Goal), with_, scenario_name_(Scenario). 
    goal_(Goal), with_, scenario_name_(Scenario).
command_(Goal, noscenario) --> 
    goal_(Goal).

%order_ --> [answer], spaces(_).
%order_ --> [run], spaces(_).
%order_ --> [solve], spaces(_).
%order_ --> [resolve], spaces(_).

goal_(Goal) --> query_or_empty, extract_constant([with], GoalWords), spaces(_), 
    {name_as_atom(GoalWords, Goal)}. % goal by name

query_or_empty --> query_.
query_or_empty --> []. 

with_ --> [with], spaces(_).

scenario_name_(Scenario) -->  scenario_or_empty_, extract_constant([], ScenarioWords), spaces(_), 
{name_as_atom(ScenarioWords, Scenario)}. % Scenario by name

scenario_or_empty_ --> [scenario], spaces(_). 
scenario_or_empty_ --> spaces(_). 

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

user:resolve(Command) :- answer(Command). 
user:resolve(Command, Question, Answers) :- resolve(Command, Question, Answers). 
user:answer( EnText) :- answer( EnText).
user:answer( EnText, Goal, Result) :- answer( EnText, Goal, Result).

user:is_it_illegal( EnText, Scenario) :- is_it_illegal( EnText, Scenario).
user:holds(Fluent, T) :- holds(Fluent, T). 

user:query(Name, Goal) :- query(Name, Goal).

user:le_taxlog_translate( en(Text), Terms) :- le_taxlog_translate( en(Text), Terms).

le_taxlog_translate( EnText, Terms) :- le_taxlog_translate( EnText, someFile, 1, Terms).

% Baseline is the line number of the start of Logical English text
le_taxlog_translate( en(Text), File, BaseLine, Terms) :-
	%findall(Decl, psyntax:lps_swish_clause(en_decl(Decl),_Body,_Vars), Decls),
    %combine_list_into_string(Decls, StringDecl),
	%string_concat(StringDecl, Text, Whole_Text),
    %once( text_to_logic(Text, Terms) ),
    %catch(text_to_logic(Text, Terms), Error, ( print_message(error, Error),  showErrors(File,BaseLine) ) ).
    text_to_logic(Text, Terms) -> true; (!, showErrors(File,BaseLine)).
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
	once(text_to_logic(_,Taxlog)),
    showErrors(someFile,0), 
	writeln(Taxlog),
	fail.
showtaxlog.

sandbox:safe_primitive(le_to_taxlog:showtaxlog).
sandbox:safe_primitive(le_to_taxlog:answer( _EnText)).
sandbox:safe_primitive(le_to_taxlog:answer( _EnText, _Goal, _Result)).
sandbox:safe_primitive(le_to_taxlog:le_taxlog_translate( _EnText, _Terms)).
