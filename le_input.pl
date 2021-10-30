/* le_input: a prolog module with predicates to translate from an 
extended version of Logical English into the Taxlog programming language. 

Main predicate: text_to_logic(String to be translated, Translation)

Main DCG nonterminal: document(Translation)

See at the end the predicate le_taxlog_translate to be used from SWISH

It assumes an entry with the following structure. One of these expressions:

the meta predicates are:
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
corresponds to each operator and corresponding conditions. 

Similarly, there may be sections for scenarios and queries, like:

--
scenario test2 is:
   borrower pays an amount to lender on 2015-06-01T00:00:00. 
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

 which can then be used on the new command interface of LE on SWISH
(e.g. answer/1 and others querying predicates):

? answer("query one with scenario test"). 

*/

:- module(le_input, 
    [document/3, le_taxlog_translate/4, 
    op(1000,xfx,user:and),  % to support querying
    op(800,fx,user:resolve), % to support querying
    op(800,fx,user:answer), % to support querying
    op(850,xfx,user:with), % to support querying
    op(800,fx,user:show), % to support querying
    op(850,xfx,user:of) % to support querying
    ]).
:- use_module('./tokenize/prolog/tokenize.pl').
:- use_module(library(pengines)).
:- use_module('reasoner.pl').
:- thread_local text_size/1, error_notice/4, dict/3, meta_dict/3, 
                last_nl_parsed/1, kbname/1, happens/2, initiates/3, terminates/3, 
                predicates/1, events/1, fluents/1, metapredicates/1, parsed/0.  
:- discontiguous statement/3, declaration/4.

% Main clause: text_to_logic(+String,-Clauses) is det
% Errors are added to error_notice 
% text_to_logic/2
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

% document/3 (or document/1 in dcg)
document(Translation, In, Rest) :- 
    (parsed -> retract(parsed); true), 
    phrase(header(Settings), In, AfterHeader), !, %print_message(informational, "Declarations completed: ~w"-[Settings]), 
    phrase(content(Content), AfterHeader, Rest), 
    append(Settings, Content, Translation), !,  
    assertz(parsed). 

% header parses all the declarations and assert them into memory to be invoked by the rules. 
% header/3
header(Settings, In, Next) :-
    length(In, TextSize), % after comments were removed
    ( phrase(settings(DictEntries, Settings_), In, Next) -> 
        ( member(target(_), Settings_) -> Settings1 = Settings_ ; Settings1 = [target(taxlog)|Settings_] )  % taxlog as default
    ; (DictEntries = [], Settings1 = [target(taxlog)] ) ), % taxlog as default
    Settings = [query(null, true), example(null, [])|Settings1], % a hack to stop the loop when query is empty
    RulesforErrors = % rules for errors that have been statically added
      [(text_size(TextSize))|Settings], % is text_size being used? % asserting the Settings too! predicates, events and fluents
    order_templates(DictEntries, OrderedEntries), 
    append(OrderedEntries, RulesforErrors, MRules),
    assertall(MRules), !. % asserting contextual information
header(_, Rest, _) :- 
    asserterror('LE error in the header ', Rest), 
    fail.

% Experimental rules for reordering of templates
% order_templates/2
order_templates(NonOrdered, Ordered) :-
	predsort(compare_templates, NonOrdered, Ordered).

compare_templates(<, meta_dict(_,_,_), dict(_,_,_)). 

compare_templates(=, dict(_,_,T1), dict(_,_,T2)) :- T1 =@= T2. 
compare_templates(<, dict(_,_,T1), dict(_,_,T2)) :- length(T1, N1), length(T2, N2), N1>N2. 
compare_templates(<, dict(_,_,T1), dict(_,_,T2)) :- length(T1, N), length(T2, N), template_before(T1, T2).  

compare_templates(>, Dict1, Dict2) :- not(compare_templates(=, Dict1, Dict2)), not(compare_templates(<, Dict1, Dict2)). 

compare_templates(=, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- T1 =@= T2. 
compare_templates(<, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- length(T1, N1), length(T2, N2), N1>N2. 
compare_templates(<, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- length(T1, N), length(T2, N), template_before(T1, T2).  

template_before([H1], [H2]) :- H1 =@= H2. 
template_before([H1|_R1], [H2|_R2]) :- nonvar(H1), var(H2).
template_before([H1|_R1], [H2|_R2]) :- H1 @> H2. 
template_before([H1|R1], [H2|R2]) :- H1=@=H2, template_before(R1, R2). 


/* --------------------------------------------------------- LE DCGs */
% settings/2 or /4
settings(AllR, AllS) --> 
    spaces_or_newlines(_), declaration(Rules,Setting), settings(RRules, RS), 
    {append(Setting, RS, AllS), append(Rules, RRules, AllR)}, !.
settings([], [], Stay, Stay) :-
    ( phrase(rules_previous(_), Stay, _) ; 
      phrase(scenario_, Stay, _)  ;  
      phrase(query_, Stay, _) ), !.  
    % settings ending with the start of the knowledge base or scenarios or queries. 
settings(_, _, Rest, _) :- 
    asserterror('LE error in the declarations ', Rest), 
    fail.
settings([], [], Stay, Stay).

% content structure: cuts added to avoid search loop
% content/1 or /3
content(T) --> %{print_message(informational, "going for KB:"-[])},  
    spaces_or_newlines(_), rules_previous(Kbname), %{print_message(informational, "KBName: ~w"-[Kbname])}, 
    kbase_content(S),  %{print_message(informational, "KB: ~w"-[S])}, 
    content(R), 
    {append([kbname(Kbname)|S], R, T)}, !.
content(T) --> %{print_message(informational, "going for scenario:"-[])},
    spaces_or_newlines(_), scenario_content(S), !, %{print_message(informational, "scenario: ~w"-[S])},
    content(R), 
    {append(S, R, T)}, !.
content(T) --> %{print_message(informational, "going for query:"-[])},
    spaces_or_newlines(_), query_content(S), !, content(R), 
    {append(S, R, T)}, !.
content([]) --> 
    spaces_or_newlines(_), []. 
content(_, Rest, _) :- 
    asserterror('LE error in the content ', Rest), 
    fail.

% kbase_content/1 or /3
kbase_content(T) --> 
    spaces_or_newlines(_),  statement(S),  kbase_content(R),
    {append(S, R, T)}, !. 
kbase_content([]) --> 
    spaces_or_newlines(_), [].
kbase_content(_, Rest, _) :- 
    asserterror('LE error in a knowledge base ', Rest), 
    fail.

% declaration/2 or /4
% target
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [the], spaces(_), [target], spaces(_), [language], spaces(_), [is], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period, !.
% meta predicates
declaration(Rules, [metapredicates(MetaTemplates)]) -->
    meta_predicate_previous, list_of_meta_predicates_decl(Rules, MetaTemplates), !.
%timeless 
declaration(Rules, [predicates(Templates)]) -->
    predicate_previous, list_of_predicates_decl(Rules, Templates), !.
%events
declaration(Rules, [events(EventTypes)]) -->
    event_predicate_previous, list_of_predicates_decl(Rules, EventTypes), !.
%time varying
declaration(Rules, [fluents(Fluents)]) -->
    fluent_predicate_previous, list_of_predicates_decl(Rules, Fluents), !.
%
declaration(_, _, Rest, _) :- 
    asserterror('LE error in a declaration ', Rest), 
    fail.

colon_or_not_ --> [':'], spaces(_).
colon_or_not_ --> []. 

meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [metapredicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [meta], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [meta], spaces(_), ['-'], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

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
list_of_predicates_decl([Ru|Rin], [F|Rout]) --> spaces_or_newlines(_), predicate_decl(Ru,F), comma_or_period, list_of_predicates_decl(Rin, Rout), !.
list_of_predicates_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a declaration ', Rest), 
    fail.

% at least one predicate declaration required
list_of_meta_predicates_decl([], []) --> spaces_or_newlines(_), next_section, !. 
list_of_meta_predicates_decl([Ru|Rin], [F|Rout]) --> 
    spaces_or_newlines(_), meta_predicate_decl(Ru,F), comma_or_period, list_of_meta_predicates_decl(Rin, Rout).
list_of_meta_predicates_decl(_, _, Rest, _) :- 
    asserterror('LE error found in the declaration of a meta template ', Rest), 
    fail.

% next_section/2
% a hack to avoid superflous searches  format(string(Mess), "~w", [StopHere]), print_message(informational, Message), 
next_section(StopHere, StopHere)  :-
    phrase(meta_predicate_previous, StopHere, _), !. % format(string(Message), "Next meta predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(predicate_previous, StopHere, _), !. % format(string(Message), "Next predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(event_predicate_previous, StopHere, _), !. % format(string(Message), "Next ecent predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(fluent_predicate_previous, StopHere, _), !. % format(string(Message), "Next fluents", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(rules_previous(_), StopHere, _), !. % format(string(Message), "Next knowledge base", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(scenario_, StopHere, _), !. % format(string(Message), "Next scenario", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(query_, StopHere, _).  % format(string(Message), "Next query", []), print_message(informational, Message).

% predicate_decl/2
predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}, !.
% we are using this resource of the last clause to record the error and its details
% not very useful with loops, of course. 
% error clause
predicate_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a declaration ', Rest), 
    fail.

% meta_predicate_decl/2
meta_predicate_decl(meta_dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.
meta_predicate_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a meta template declaration ', Rest), 
    fail.

rules_previous(default) --> 
    spaces_or_newlines(_), [the], spaces(_), [rules], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_), !.
rules_previous(KBName) --> 
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], extract_constant([includes], NameWords), [includes], spaces(_), [':'], !, spaces_or_newlines(_),
    {name_as_atom(NameWords, KBName)}.
rules_previous(default) -->  % backward compatibility
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], spaces(_), [includes], spaces(_), [':'], spaces_or_newlines(_). 

% scenario_content/1 or /3
% a scenario description: assuming one example -> one scenario -> one list of facts.
scenario_content(Scenario) -->
    scenario_, extract_constant([is], NameWords), is_colon_, newline,
    %list_of_facts(Facts), period, !, 
    spaces(_), assumptions_(Assumptions), !, % period is gone
    {name_as_atom(NameWords, Name), Scenario = [example( Name, [scenario(Assumptions, true)])]}.

scenario_content(_,  Rest, _) :- 
    asserterror('LE error found around this scenario expression: ', Rest), fail.

% query_content/1 or /3
% statement: the different types of statements in a LE text
% a query
query_content(Query) -->
    query_, extract_constant([is], NameWords), is_colon_, spaces_or_newlines(_),
    query_header(Ind0, Map1),  
    conditions(Ind0, Map1, _, Conds), !, period,  % period stays!
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
% statement/1 or /3 
statement(Statement) --> 
    it_becomes_the_case_that_, spaces_or_newlines(_), 
        literal_([], Map1, holds(Fluent, _)), spaces_or_newlines(_), 
    when_, spaces_or_newlines(_), 
        literal_(Map1, Map2, happens(Event, T)), spaces_or_newlines(_),
    body_(Body, [map(T, '_change_time')|Map2],_), period,  
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
    body_(Body, [map(T, '_change_time')|Map2],_), period,  
        {(Body = [] -> Statement = [if(terminates(Event, Fluent, T), true)];  
            (Statement = [if(terminates(Event, Fluent, T), Body)] %, print_message(informational, "~w"-Statement)
            ))}, !.

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

% error
statement(_, Rest, _) :- 
    asserterror('LE error found around this statement: ', Rest), fail.

list_of_facts([F|R1]) --> literal_([], _,F), rest_list_of_facts(R1).

rest_list_of_facts(L1) --> comma, spaces_or_newlines(_), list_of_facts(L1).
rest_list_of_facts([]) --> [].

% assumptions_/3 or /5
assumptions_([A|R]) --> 
        spaces_or_newlines(_),  rule_([], _, A), !, assumptions_(R).
assumptions_([]) --> 
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
% body/3 or /5
body_([], Map, Map) --> spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    newline, spaces(Ind), if_, !, conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    if_, newline_or_nothing, spaces(Ind), conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).

newline_or_nothing --> newline.
newline_or_nothing --> []. 

% literal_/3 or /5
% literal_ reads a list of words until it finds one of these: ['\n', if, '.']
% it then tries to match those words against a template in memory (see dict/3 predicate).
% The output is then contigent to the type of literal according to the declarations. 
literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'at time, literal') },
    at_time(T, Map1, Map2), comma, possible_instance(PossibleTemplate),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'literal, at time') },
    possible_instance(PossibleTemplate), comma, at_time(T, Map1, Map2),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) -->  
    possible_instance(PossibleTemplate), %{ print_message(informational, "~w"-[PossibleTemplate]) },
    {match_template(PossibleTemplate, Map1, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (consult_map(Time, '_change_time', Map1, _MapF) -> T=Time; true), 
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; (FinalLiteral = Literal)))
      %print_message(informational, "~w with ~w"-[FinalLiteral, MapF])
     }, !. % by default (including builtins) they are timeless!

% rewritten to use in swish. Fixed! It was a name clash. Apparently "literal" is used somewhere else
%literal_(Map1, MapN, Literal, In, Out) :-  print_message(informational, '  inside a literal'),
%        possible_instance(PossibleTemplate, In, Out), print_message(informational, PossibleTemplate),
%        match_template(PossibleTemplate, Map1, MapN, Literal).
% error clause
literal_(M, M, _, Rest, _) :- 
    asserterror('LE error found in a literal ', Rest), fail.

% conditions/4 or /6
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
% term_/4 or /6
term_(StopWords, Term, Map1, MapN) --> 
    (variable(StopWords, Term, Map1, MapN), !); (constant(StopWords, Term, Map1, MapN), !); (list_(Term, Map1, MapN), !). %; (compound_(Term, Map1, MapN), !).

% list_/3 or /5
list_(List, Map1, MapN) --> 
    spaces(_), bracket_open_, !, extract_list([']'], List, Map1, MapN), bracket_close.   

compound_(V1/V2, Map1, MapN) --> 
    term_(['/'], V1, Map1, Map2), ['/'], term_([], V2, Map2, MapN). 

% event observations
%condition(happens(Event), _, Map1, MapN) -->
%    observe_,  literal_(Map1, MapN, Event), !.

% condition/4 or /6
% this produces a Taxlog condition with the form: 
% setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on Before, SetOfPreviousOwners)
% from a set of word such as: 
%     and a record of previous owners is a set of [an owner, a share] 
%           where the asset is ultimately owned by the share with the owner at the previous time
condition(FinalExpression, _, Map1, MapN) --> 
    variable([is], Set, Map1, Map2), is_a_set_of_, term_([], Term, Map2, Map3), !, % moved where to the following line
    newline, spaces(Ind2), where_, conditions(Ind2, Map3, Map4, Goals),
    modifiers(setof(Term,Goals,Set), Map4, MapN, FinalExpression).

% for every a party is a party in the event, it is the case that:
condition(FinalExpression, _, Map1, MapN) -->  
    for_all_cases_in_which_, newline, !, 
    spaces(Ind2), conditions(Ind2, Map1, Map2, Conds), spaces_or_newlines(_), 
    it_is_the_case_that_, newline, 
    spaces(Ind3), conditions(Ind3, Map2, Map3, Goals),
    modifiers(forall(Conds,Goals), Map3, MapN, FinalExpression).

% the Value is the sum of each Asset Net such that
condition(FinalExpression, _, Map1, MapN) --> 
    variable([is], Value, Map1, Map2), is_the_sum_of_each_, extract_variable([such], Each, [], NameWords, _), such_that_, !, 
    { name_predicate(NameWords, Name), update_map(Each, Name, Map2, Map3) }, newline, 
    spaces(Ind), conditions(Ind, Map3, Map4, Conds), 
    modifiers(aggregate_all(sum(Each),Conds,Value), Map4, MapN, FinalExpression).
    
% it is not the case that 
%condition((pengine_self(M), not(M:Conds)), _, Map1, MapN) --> 
%condition((true, not(Conds)), _, Map1, MapN) -->
condition(not(Conds), _, Map1, MapN) --> 
%condition(not(Conds), _, Map1, MapN) --> 
    spaces(_), not_, newline,  % forget other choices. We know it is a not case
    spaces(Ind), conditions(Ind, Map1, MapN, Conds), !.

condition(Cond, _, Map1, MapN) -->  
    literal_(Map1, MapN, Cond), !. 

%condition(assert(Prolog), _, Map1, MapN) -->
%    this_information_, !, prolog_literal_(Prolog, Map1, MapN), has_been_recorded_. 

% condition(-Cond, ?Ind, +InMap, -OutMap)
% builtins have been included as predefined templates in the predef_dict
%condition(InfixBuiltIn, _, Map1, MapN) --> 
%    term_(Term, Map1, Map2), spaces_or_newlines(_), builtin_(BuiltIn), 
%    spaces_or_newlines(_), expression_(Expression, Map2, MapN), !, {InfixBuiltIn =.. [BuiltIn, Term, Expression]}. 

% error clause
condition(_, _Ind, Map, Map, Rest, _) :- 
        asserterror('LE error found at a condition ', Rest), fail.

% modifiers add reifying predicates to an expression. 
% modifiers(+MainExpression, +MapIn, -MapOut, -FinalExpression)
modifiers(MainExpression, Map1, MapN, on(MainExpression, Var) ) -->
    newline, spaces(_), at_, variable([], Var, Map1, MapN). % newline before a reifying expression
modifiers(MainExpression, Map, Map, MainExpression) --> [].  

% variable/4 or /6
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), [Det], {indef_determiner(Det)}, extract_variable(StopWords, Var, [], NameWords, _), % <-- CUT!
    {  NameWords\=[], name_predicate(NameWords, Name), update_map(Var, Name, Map1, MapN) }. 
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), [Det], {def_determiner(Det)}, extract_variable(StopWords, Var, [], NameWords, _), % <-- CUT!
    {  NameWords\=[], name_predicate(NameWords, Name), consult_map(Var, Name, Map1, MapN) }. 
% allowing for symbolic variables: 
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), extract_variable(StopWords, Var, [], NameWords, _),
    {  NameWords\=[], name_predicate(NameWords, Name), consult_map(Var, Name, Map1, MapN) }. 

% constant/4 or /6
constant(StopWords, Constant, Map, Map) -->
    extract_constant(StopWords, NameWords), { NameWords\=[], name_predicate(NameWords, Constant) }. 

% deprecated
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
is_the_sum_of_each_ --> [is], spaces(_), [the], spaces(_), [every], spaces(_), [of], spaces(_), [each], spaces(_) .

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

it_is_the_case_that_ --> [it], spaces(_), [is], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).

is_a_set_of_ --> [is], spaces(_), [a], spaces(_), [set], spaces(_), [of], spaces(_). 

where_ --> [where], spaces(_). 

scenario_ -->  spaces_or_newlines(_), ['Scenario'], !, spaces(_).
scenario_ -->  spaces_or_newlines(_), [scenario], spaces(_). 

is_colon_ -->  [is], spaces(_), [':'], spaces(_).

query_ --> spaces_or_newlines(_), ['Query'], !, spaces(_).
query_ --> spaces_or_newlines(_), [query], spaces(_).


for_which_ --> [for], spaces(_), [which], spaces(_). 

query_header(Ind, Map) --> spaces(Ind), for_which_, list_of_vars([], Map), colon_, spaces_or_newlines(_).
query_header(0, []) --> []. 

list_of_vars(Map1, MapN) --> 
    extract_variable([',', and, ':'], Var, [], NameWords, _), 
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

% template_decl/4
% cuts added to improve efficiency
template_decl([], [newline(_)|RestIn], [newline(_)|RestIn]) :- 
    asserterror('LE error: misplaced new line found in a template declaration ', RestIn), !, 
    fail. % cntrl \n should be rejected as part of a template
template_decl(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\t'|RestIn], Out) :- !, % skip cntrl \t in template
    template_decl(RestW, RestIn, Out).
% excluding ends of lines from templates
%template_decl(RestW, [newline(_)|RestIn], Out) :- !, % skip cntrl \n in template
%    template_decl(RestW, RestIn, Out).
template_decl([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,['.', ','])),   % only . and , as boundaries. Beware!
    template_decl(RestW, RestIn, Out), !.
template_decl([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,['.', ',']), !.
template_decl(_, Rest, _) :- 
    asserterror('LE error found in a template declaration ', Rest), fail.

% build_template/5
build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template) :-
    build_template_elements(RawTemplate, [], Arguments, TypesAndNames, OtherWords, Template),
    name_predicate(OtherWords, Predicate).

% build_template_elements(+Input, +Previous, -Args, -TypesNames, -OtherWords, -Template)
build_template_elements([], _, [], [], [], []) :- !. 
% a variable signalled by a *
build_template_elements(['*', Word|RestOfWords], _Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    has_pairing_asteriks([Word|RestOfWords]), 
    %(ind_det(Word); ind_det_C(Word)), % Previous \= [is|_], % removing this requirement when * is used
    determiner(Word), % allows the for variables in templates declarations only
    extract_variable(['*'], Var, [], RNameWords, RTypeWords, RestOfWords, ['*'|NextWords]), !, % <-- it must end with * too
    reverse(RNameWords, NameWords), 
    name_predicate(NameWords, Name),
    reverse(RTypeWords, TypeWords),  
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate). 
build_template_elements(['*', Word|RestOfWords], _Previous,_, _, _, _) :-
    not(has_pairing_asteriks([Word|RestOfWords])), !, fail. % produce an error report if asterisks are not paired
% a variable not signalled by a *  % for backward compatibility  \\ DEPRECATED
%build_template_elements([Word|RestOfWords], Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
%    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
%    extract_variable(['*'], Var, [], NameWords, TypeWords, RestOfWords, NextWords), !, % <-- CUT!
%    name_predicate(NameWords, Name), 
%    name_predicate(TypeWords, Type), 
%    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], Previous, RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, [Word|Previous], RestVars, RestTypes, Others, RestTemplate).

has_pairing_asteriks(RestOfTemplate) :-
    findall('*',member('*', RestOfTemplate), Asteriks), length(Asteriks, N), 1 is mod(N, 2).

name_predicate(Words, Predicate) :-
    concat_atom(Words, '_', Predicate). 

% name_as_atom/2
name_as_atom([Number], Number) :-
    number(Number), !. 
name_as_atom([Atom], Number) :- 
    atom_number(Atom, Number), !. 
name_as_atom(Words, Name) :-
    numbervars(Words, 1, _, [functor_name('unknown')]),
    replace_vars(Words, Atoms), 
    list_words_to_codes(Atoms, Codes),
    replace_ast_a(Codes, CCodes), 
    atom_codes(Name, CCodes).  

replace_ast_a([], []) :- !. 
replace_ast_a([42,32,97|Rest], [42,97|Out]) :- !, 
    replace_final_ast(Rest, Out). 
replace_ast_a([C|Rest], [C|Out]) :-
    replace_ast_a(Rest, Out).

replace_final_ast([], []) :- !. 
replace_final_ast([32,42|Rest], [42|Out]) :- !, 
    replace_ast_a(Rest, Out).
replace_final_ast([C|Rest], [C|Out]) :-
    replace_final_ast(Rest, Out).

% maps a list of words to a list of corresponding codes
% adding an space between words-codes (32). 
% list_word_to_codes/2
list_words_to_codes([], []).
list_words_to_codes([Word|RestW], Out) :-
    atom_codes(Word, Codes),
    remove_quotes(Codes, CleanCodes), 
    list_words_to_codes(RestW, Next),
    (Next=[]-> Out=CleanCodes; append(CleanCodes, [32|Next], Out)), !. 

remove_quotes([], []) :-!.
remove_quotes([39|RestI], RestC) :- remove_quotes(RestI, RestC), !.
% quick fix to remove parentesis and numbers too. 
remove_quotes([40, _, 41|RestI], RestC) :- remove_quotes(RestI, RestC), !.
%remove_quotes([41|RestI], RestC) :- remove_quotes(RestI, RestC), !.
remove_quotes([C|RestI], [C|RestC]) :- remove_quotes(RestI, RestC). 

replace_vars([],[]) :- !.
replace_vars([A|RI], [A|RO]) :- atom(A), replace_vars(RI,RO), !.
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

% possible_instance/3
% cuts added to improve efficiency
% skipping a list
possible_instance([], [], []) :- !. 
possible_instance(Final, ['['|RestIn], Out) :- !, 
    possible_instance_for_lists(List, RestIn, [']'|Next]),  
    possible_instance(RestW, Next, Out),
    append(['['|List], [']'|RestW], Final).  
possible_instance(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    possible_instance(RestW, RestIn, Out).
possible_instance(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    possible_instance(RestW, RestIn, Out).
possible_instance([that|Instance], In, Out) :- % to allow "that" instances to spread over more than one line
    phrase(spaces_or_newlines(_), In, [that|Rest]),
    phrase(spaces_or_newlines(_), Rest, Next), !, 
    possible_instance(Instance, Next, Out).
possible_instance([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    not(lists:member(Word,[newline(_), if, '.', ','])), 
    % leaving the comma in as well (for lists and sets we will have to modify this)
    possible_instance(RestW, RestIn, Out).
possible_instance([], [Word|Rest], [Word|Rest]) :- 
    lists:member(Word,[newline(_), if, '.', ',']). % leaving or/and out of this

% using [ and ] for list and set only to avoid clashes for commas
%possible_instance_for_lists([], [], []) :- !.
possible_instance_for_lists([], [']'|Out], [']'|Out]) :- !. 
possible_instance_for_lists(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    possible_instance_for_lists(RestW, RestIn, Out).
possible_instance_for_lists(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    possible_instance_for_lists(RestW, RestIn, Out).
possible_instance_for_lists([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    possible_instance_for_lists(RestW, RestIn, Out).
%possible_instance_for_lists([], [Word|Rest], [Word|Rest]) :- 
%    lists:member(Word,[',', newline(_), if, '.']). % leaving or/and out of this

% match_template/4
match_template(PossibleLiteral, Map1, MapN, Literal) :-
    %print_message(informational,'Possible Meta Literal ~w'-[PossibleLiteral]),
    meta_dictionary(Predicate, _, MetaCandidate),
    meta_match(MetaCandidate, PossibleLiteral, Map1, MapN, MetaTemplate), !, 
    meta_dictionary(Predicate, _, MetaTemplate),
    Literal =.. Predicate. 

match_template(PossibleLiteral, Map1, MapN, Literal) :- 
    %print_message(informational,'Possible Literal ~w'-[PossibleLiteral]),
    dictionary(Predicate, _, Candidate),
    match(Candidate, PossibleLiteral, Map1, MapN, Template), !, 
    dictionary(Predicate, _, Template), 
    Literal =.. Predicate.
    %print_message(informational,'Match!! with ~w'-[Literal]).% !. 

% meta_match/5
% meta_match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
meta_match([], [], Map, Map, []) :- !.
meta_match([Word|_LastElement], [Word|PossibleLiteral], Map1, MapN, [Word,Literal]) :- % asuming Element is last in template!
    Word = that, % that is a reserved word "inside" templates! -> <meta level> that <object level> 
    (meta_dictionary(Predicate, _, Candidate); dictionary(Predicate, _, Candidate)), % searching for a new inner literal
    match(Candidate, PossibleLiteral, Map1, MapN, InnerTemplate),
    (meta_dictionary(Predicate, _, InnerTemplate); dictionary(Predicate, _, InnerTemplate)), 
    Literal =.. Predicate, !. 
meta_match([MetaElement|RestMetaElements], [MetaWord|RestPossibleLiteral], Map1, MapN, [MetaElement|RestSelected]) :-
    nonvar(MetaElement), MetaWord = MetaElement, !, 
    meta_match(RestMetaElements, RestPossibleLiteral, Map1, MapN, RestSelected).
%meta_match([MetaElement|RestMetaElements], PossibleLiteral, Map1, MapN, [Literal|RestSelected]) :-
%    var(MetaElement), stop_words(RestMetaElements, StopWords), 
%    extract_literal(StopWords, LiteralWords, PossibleLiteral, NextWords),
%    meta_dictionary(Predicate, _, Candidate),
%    match(Candidate, LiteralWords, Map1, Map2, Template),  %only two meta levels! % does not work. 
%    meta_dictionary(Predicate, _, Template), 
%    Literal =.. Predicate, !, 
%    meta_match(RestMetaElements, NextWords, Map2, MapN, RestSelected).  
meta_match([MetaElement|RestMetaElements], PossibleLiteral, Map1, MapN, [Literal|RestSelected]) :-
    var(MetaElement), stop_words(RestMetaElements, StopWords), 
    extract_literal(StopWords, LiteralWords, PossibleLiteral, NextWords),
    dictionary(Predicate, _, Candidate), % this assumes that the "contained" literal is an object level literal. 
    match(Candidate, LiteralWords, Map1, Map2, Template), 
    dictionary(Predicate, _, Template), 
    Literal =.. Predicate, !, 
    meta_match(RestMetaElements, NextWords, Map2, MapN, RestSelected).  
% it could also be an object level matching of other kind
meta_match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    indef_determiner(Det), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected). 
meta_match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    def_determiner(Det), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected). 
% handling symbolic variables (as long as they have been previously defined and included in the map!) 
meta_match([Element|RestElements], PossibleLiteral, Map1, MapN, [Var|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !, % <-- CUT!  % if the variables has been previously registered
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected).
meta_match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [List|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_list([']'|StopWords], List, Map1, Map2, PossibleLiteral, [']'|NextWords]), !, % matching brackets verified
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected).
% enabling expressions and constants
meta_match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Expression|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_expression([','|StopWords], NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [],
    % this expression cannot add variables 
    ( phrase(expression_(Expression, Map1, Map1), NameWords) -> true ; ( name_predicate(NameWords, Expression) ) ),
    %print_message(informational, 'found a constant or an expression '), print_message(informational, Expression),
    meta_match(RestElements, NextWords, Map1, MapN, RestSelected). 

% match/5
% match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
match([], [], Map, Map, []) :- !.  % success! It succeds iff PossibleLiteral is totally consumed
% meta level access: that New Literal
match([Word|_LastElement], [Word|PossibleLiteral], Map1, MapN, [Word,Literal]) :- % asuming Element is last in template!
    Word = that, % that is a reserved word "inside" templates! -> <meta level> that <object level> 
    (meta_dictionary(Predicate, _, Candidate); dictionary(Predicate, _, Candidate)), % searching for a new inner literal
    match(Candidate, PossibleLiteral, Map1, MapN, InnerTemplate),
    (meta_dictionary(Predicate, _, InnerTemplate); dictionary(Predicate, _, InnerTemplate)), 
    Literal =.. Predicate, !. 
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
    nonvar(Element), Word = Element, 
    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    indef_determiner(Det), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    match(RestElements, NextWords, Map2, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    def_determiner(Det), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    match(RestElements, NextWords, Map2, MapN, RestSelected). 
% handling symbolic variables (as long as they have been previously defined and included in the map!) 
match([Element|RestElements], PossibleLiteral, Map1, MapN, [Var|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, Var, [], NameWords, _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !, % <-- CUT!  % if the variables has been previously registered
    match(RestElements, NextWords, Map2, MapN, RestSelected).
match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [Term|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_list([']'|StopWords], List, Map1, Map2, PossibleLiteral, [']'|NextWords]),  % matching brackets verified
    %print_message(informational, "List ~w"-[List]),  
    correct_list(List, Term), 
    match(RestElements, NextWords, Map2, MapN, RestSelected).
% enabling expressions and constants
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Expression|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    %print_message(informational, [Word|PossibleLiteral]),
    extract_expression([','|StopWords], NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [],
    %print_message(informational, "Expression? ~w"-[NameWords]),
    % this expression cannot add variables 
    ( phrase(expression_(Expression, Map1, Map1), NameWords) -> true ; ( name_predicate(NameWords, Expression) ) ),
    %print_message(informational, 'found a constant or an expression '), print_message(informational, Expression),
    match(RestElements, NextWords, Map1, MapN, RestSelected). 

correct_list([], []) :- !. 
correct_list([A,B], [A,B]) :- atom(B), !. % not(is_list(B)), !. 
correct_list([A,B], [A|B] ) :- !. 
correct_list([A|B], [A|NB]) :- correct_list(B, NB). 

% expression/3 or /5
%expression_(List, MapIn, MapOut) --> list_(List, MapIn, MapOut), !. 
% expression_ resolve simple math (non boolean) expressions fttb. 
% dates must be dealt with first  
% 2021-02-06T08:25:34 is transformed into 1612599934.0.
expression_(DateInSeconds, Map, Map) --> 
    [Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], spaces(_),
    { concat_atom([Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], '', Date), 
      parse_time(Date,DateInSeconds) %, print_message(informational, "~w"-[DateInSeconds])  
    }, !.
% 2021-02-06
expression_(DateInSeconds, Map, Map) -->  [Year,'-', Month, '-', Day],  spaces(_),
    { concat_atom([Year, Month, Day], '', Date), parse_time(Date, DateInSeconds) }, !. 
% basic float  extracted from atoms from the tokenizer
expression_(Float, Map, Map) --> [AtomNum,'.',AtomDecimal],
        { atom(AtomNum), atom(AtomDecimal), atomic_list_concat([AtomNum,'.',AtomDecimal], Atom), atom_number(Atom, Float) }, !.
% mathematical expressions
expression_(InfixBuiltIn, Map1, MapN) --> 
    %{print_message(informational, "Binary exp map ~w"-[Map1])}, 
    {op_stop(Stop)}, term_(Stop, Term, Map1, Map2), spaces(_), binary_op(BuiltIn), !, 
    %{print_message(informational, "Binary exp first term ~w and op ~w"-[Term, BuiltIn])}, 
    spaces(_), expression_(Expression, Map2, MapN), spaces(_), 
    {InfixBuiltIn =.. [BuiltIn, Term, Expression]}.%, print_message(informational, "Binary exp ~w"-InfixBuiltIn)}.  
% a quick fix for integer numbers extracted from atoms from the tokenizer
expression_(Number, Map, Map) --> [Atom],  spaces(_), { atom(Atom), atom_number(Atom, Number) }, !. 
expression_(Var, Map1, Map2) -->  {op_stop(Stop)}, variable(Stop, Var, Map1, Map2),!.%, {print_message(informational, "Just var ~w"-Var)}, 
expression_(Constant, Map1, Map2) -->  {op_stop(Stop)}, constant(Stop, Constant, Map1, Map2).%, {print_message(informational, "Constant ~w"-Constant)}.     
% error clause
expression(_, _, _, Rest, _) :- 
    asserterror('LE error found in an expression ', Rest), fail.

% only one word operators
%binary_op(Op) --> [Op], { atom(Op), current_op(_Prec, Fix, Op),
%    Op \= '.',
%    (Fix = 'xfx'; Fix='yfx'; Fix='xfy'; Fix='yfy') }.

% operators with any amout of words/symbols
% binary_op/3
binary_op(Op, In, Out) :-
    op_tokens(Op, OpTokens),
    append(OpTokens, Out, In).

op_tokens(Op, OpTokens) :-
    current_op(_Prec, Fix, Op), Op \= '.',
    (Fix = 'xfx'; Fix='yfx'; Fix='xfy'; Fix='yfy'),
    term_string(Op, OpString), tokenize(OpString, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, OpTokens).

% very inefficient. Better to compute and store. See below
op_stop_words(Words) :-
    op_stop(Words) -> true; (    
        findall(Word, 
            (current_op(_Prec, _, Op), Op \= '.', % don't include the period!
            term_string(Op, OpString), 
            tokenize(OpString, Tokens, [cased(true), spaces(true), numbers(false)]),
            unpack_tokens(Tokens, [Word|_])), Words), % taking only the first word as stop word 
        assertz(op_stop(Words))
        ), !. 

op_stop([ (on), 
        (because),
        (is_not_before),
        (not),
        (before),
        (and),
        (or),
        (at),
        (html_meta),
        (after),
        (in),
        (else),
        (+),
        (then),
        (must),
        (if),
        (if),
        ($),
        (\),
        (=),
        (thread_initialization),
        (:),
        (\),
        '\'',
        (xor),
        (:),
        (rem),
        (\),
        (table),
        (initialization),
        (rdiv),
        (/),
        (>),
        (>),
        (=),
        (=),
        (;),
        (as),
        (is),
        (=),
        @,
        @,
        @,
        @,
        (\),
        (thread_local),
        (>),
        (=),
        (<),
        (*),
        '\'',
        (=),
        (\),
        (\),
        (+),
        (+),
        (:),
        (>),
        (div),
        (discontiguous),
        (<),
        (/),
        (meta_predicate),
        (=),
        (-),
        (-),
        (volatile),
        (public),
        (-),
        (:),
        (:),
        (*),
        ?,
        (/),
        (*),
        (-),
        (multifile),
        (dynamic),
        (mod),
        (^),
        (module_transparent)
      ]).

stop_words([], []).
stop_words([Word|_], [Word]) :- nonvar(Word). % only the next word for now
stop_words([Word|_], []) :- var(Word).

% list_symbol/1: a symbol specific for list that can be used as stop word for others
list_symbol('[').
list_symbol(']'). 

extract_literal(_, [], [], []) :- !. 
extract_literal(StopWords, [],  [Word|RestOfWords],  [Word|RestOfWords]) :-
    (member(Word, StopWords); that_(Word); phrase(newline, [Word])), !. 
extract_literal(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_literal(SW, RestName, RestOfWords, NextWords).
extract_literal(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_literal(SW, RestName, RestOfWords, NextWords).
extract_literal(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    extract_literal(SW, RestName, RestOfWords, NextWords).

% extract_variable/7
% extract_variable(+StopWords, -Var, +InitListOfNameWords, -ListOfNameWords, -ListOfTypeWords, +ListOfWords, -NextWordsInText)
% refactored as a dcg predicate
extract_variable(_, _, Names, Names, [], [], []) :- !.                                % stop at when words run out
extract_variable(StopWords, _, Names, Names, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    %(member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
    (member(Word, StopWords); that_(Word); list_symbol(Word); punctuation(Word); phrase(newline, [Word])), !.
extract_variable(SW, Var, InName, OutName, RestType, [' '|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable(SW, Var, InName, OutName, RestType, RestOfWords, NextWords).
extract_variable(SW, Var, InName, OutName, RestType, ['\t'|RestOfWords], NextWords) :- !,  % skipping spaces
    extract_variable(SW, Var, InName, OutName, RestType, RestOfWords, NextWords).  
extract_variable(SW, Var, InName, OutName, Type, [Word|RestOfWords], NextWords) :- % ordinals are not part of the type
    ordinal(Word), !, 
    extract_variable(SW, Var, [Word|InName], OutName, Type, RestOfWords, NextWords).
extract_variable(SW, Var, InName, OutName, [Word|RestType], [Word|RestOfWords], NextWords) :- % types are not part of the name
    is_a_type(Word),
    extract_variable(SW, Var, InName, NextName, RestType, RestOfWords, NextWords),
    (NextName = [] -> OutName = [Word]; OutName = NextName).
extract_variable(SW, Var, InName, OutName, RestType, [Word|RestOfWords], NextWords) :- % everything else is part of the name
    extract_variable(SW, Var, [Word|InName], OutName, RestType, RestOfWords, NextWords).

% extract_expression/4
% extract_expression(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
% it does not stop at reserved words!
extract_expression(_, [], [], []) :- !.                                % stop at when words run out
extract_expression(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at  verbs? or prepositions?. 
    (member(Word, StopWords); that_(Word); list_symbol(Word); phrase(newline, [Word])), !.  
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

% extract_constant/4
% extract_constant(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
extract_constant(_, [], [], []) :- !.                                % stop at when words run out
extract_constant(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs? or prepositions?. 
    %(member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
    (member(Word, StopWords); that_(Word); list_symbol(Word); punctuation(Word); phrase(newline, [Word])), !.
%extract_constant([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    %not(determiner(Word)), % no determiners inside constants!
    extract_constant(SW, RestName, RestOfWords, NextWords).

% extract_list/6
% extract_list(+StopWords, -List, +Map1, -Map2, +[Word|PossibleLiteral], -NextWords),
extract_list(SW, [], Map, Map, [Word|Rest], [Word|Rest]) :- 
    lists:member(Word, SW), !. % stop but leave the symbol for further verification
%extract_list(_, [], Map, Map, [')'|Rest], [')'|Rest]) :- !. 
extract_list(SW, RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, [','|RestOfWords],  NextWords) :- !, % skip over commas
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, ['|'|RestOfWords],  NextWords) :- !, % skip over commas
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(StopWords, List, Map1, MapN, [Det|InWords], LeftWords) :-
    indef_determiner(Det), 
    extract_variable(['|'|StopWords], Var, [], NameWords, _, InWords, NextWords), NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    update_map(Var, Name, Map1, Map2),
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ; 
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList=[] -> List=[Var|[]]; List=[Var|RestList]), 
    !.
extract_list(StopWords, List, Map1, MapN, [Det|InWords], LeftWords) :-
    def_determiner(Det), 
    extract_variable(['|'|StopWords], Var, [], NameWords, _, InWords, NextWords), NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    consult_map(Var, Name, Map1, Map2), 
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ;
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList=[] -> List=[Var|[]]; List=[Var|RestList]), !.
extract_list(StopWords, List, Map1, MapN, InWords, LeftWords) :- % symbolic variables without determiner
    extract_variable(['|'|StopWords], Var,[], NameWords, _, InWords, NextWords), NameWords \= [],  % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    consult_map(Var, Name, Map1, Map2), 
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ; 
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList=[] -> List=[Var|[]]; List=[Var|RestList]), !.
extract_list(StopWords, List, Map1, MapN, InWords, LeftWords) :-
    extract_expression(['|',','|StopWords], NameWords, InWords, NextWords), NameWords \= [], 
    ( phrase(expression_(Expression, Map1, Map2), NameWords) -> true 
    ; ( Map1 = Map2, name_predicate(NameWords, Expression) ) ),
    ( NextWords = [']'|_] -> ( RestList = [], LeftWords=NextWords, MapN=Map2 ) 
    ;    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList=[] -> List=[Expression|[]]; List=[Expression|RestList]), !.

determiner(Det) :-
    (ind_det(Det); ind_det_C(Det); def_det(Det); def_det_C(Det)), !. 

indef_determiner(Det) :-
    (ind_det(Det); ind_det_C(Det)), !. 

def_determiner(Det) :-
    (def_det(Det); def_det_C(Det)), !. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, [], Template).

% template_elements(+Input,+InMap, -OutMap, +Previous, -Template)
template_elements([], Map1, Map1, _, []).     
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
    extract_variable([], Var, [], NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (def_det_C(Word); def_det(Word)), Previous \= [is|_], 
    extract_variable([], Var, [], NameWords, _, RestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    member(map(Var,Name), Map1),  % confirming it is an existing variable and unifying
    template_elements(NextWords, Map1, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Word|RestTemplate]) :-
    template_elements(RestOfWords, Map1, MapN, [Word|Previous], RestTemplate).

% update_map/4
% update_map(?V, +Name, +InMap, -OutMap)
update_map(V, Name, InMap, InMap) :- 
    var(V), nonvar(Name), nonvar(InMap), 
    member(map(O,Name), InMap), O\==V, fail, !. 
update_map(V, Name, InMap, OutMap) :-  % updates the map by adding a new variable into it. 
    var(V), nonvar(Name), nonvar(InMap), 
    not(member(map(_,Name), InMap)), 
    OutMap = [map(V,Name)|InMap]. 
%update_map(V, _, Map, Map) :-
%    nonvar(V). 

% consult_map/4
% consult_map(+V, -Name, +Inmap, -OutMap)
consult_map(V, Name, InMap, InMap) :-
    member(map(Var, SomeName), InMap), (Name == SomeName -> Var = V; ( Var == V -> Name = SomeName ; fail ) ),  !.  
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
   (T=time; T=date; T=number; T=person; T=day). % primitive types to start with
   %not(number(T)), not(punctuation(T)),
   %not(reserved_word(T)),
   %not(verb(T)),
   %not(preposition(T)). 

/* ------------------------------------------------ determiners */

ind_det_C('A').
ind_det_C('An').
% ind_det_C('Some').
ind_det_C('Each'). % added experimental
ind_det_C('Which').  % added experimentally

def_det_C('The').

ind_det(a).
ind_det(an).
ind_det(another). % added experimentally
ind_det(which).  % added experimentally
ind_det(each).  % added experimentally
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

that_(that).
that_('That'). 

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

showErrors(File,Baseline) :- % showing the deepest message!
    findall(error_notice(error, Me,Pos, ContextTokens), 
        error_notice(error, Me,Pos, ContextTokens), ErrorsList),
    deepest(ErrorsList, 
        error_notice(error, 'None',0, ['There was no syntax error']), 
        error_notice(error, MeMax,PosMax, ContextTokensMax)), 
    atomic_list_concat([MeMax,': '|ContextTokensMax],ContextTokens_),
    Line is PosMax+Baseline,
    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char))).
    % to show them all
    %forall(error_notice(error, Me,Pos, ContextTokens), (
    %    atomic_list_concat([Me,': '|ContextTokens],ContextTokens_),
    %    Line is Pos+Baseline,
    %    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char)))
    %    )).

deepest([], Deepest, Deepest) :- !.
deepest([error_notice(error, Me,Pos, ContextTokens)|Rest], 
        error_notice(error,_Me0, Pos0,_ContextTokens0), Out) :-
    Pos0 < Pos, !, 
    deepest(Rest, error_notice(error, Me,Pos, ContextTokens), Out).
deepest([_|Rest], In, Out) :-
    deepest(Rest, In, Out).

showProgress :-
    findall(error_notice(error, Me,Pos, ContextTokens), 
        error_notice(error, Me,Pos, ContextTokens), ErrorsList),
    deepest(ErrorsList, 
        error_notice(error, 'None',0, ['There was no syntax error']), 
        error_notice(error, MeMax,PosMax, ContextTokensMax)), 
    atomic_list_concat([MeMax,': '|ContextTokensMax],ContextTokens_),
    Line is PosMax+1,
    print_message(informational,error(syntax_error(ContextTokens_),file(someFile,Line,_One,_Char))).


spypoint(A,A). % for debugging

% meta_dictionary(?LiteralElements, ?NamesAndTypes, ?Template)
% for meta templates. See below
meta_dictionary(Predicate, VariablesNames, Template) :- 
    meta_dict(Predicate, VariablesNames, Template) ; predef_meta_dict(Predicate, VariablesNames, Template).

:- discontiguous predef_meta_dict/3.
predef_meta_dict([=, T1, T2], [time1-time, time2-time], [T1, is, equal, to, T2]).
predef_meta_dict([\=, T1, T2], [time1-time, time2-time], [T1, is, different, from, T2]).

% dictionary(?LiteralElements, ?NamesAndTypes, ?Template)
% this is a multimodal predicate used to associate a Template with its particular other of the words for LE
% with the Prolog expression of that relation in LiteralElements (not yet a predicate, =.. is done elsewhere).
% NamesAndTypes contains the external name and type (name-type) of each variable just in the other in 
% which the variables appear in LiteralElement. 
dictionary(Predicate, VariablesNames, Template) :- % dict(Predicate, VariablesNames, Template).
    dict(Predicate, VariablesNames, Template) ; predef_dict(Predicate, VariablesNames, Template).
%    predef_dict(Predicate, VariablesNames, Template); dict(Predicate, VariablesNames, Template).

:- discontiguous predef_dict/3.
% predef_dict/3 is a database with predefined templates for LE
% it must be ordered by the side of the third argument, to allow the system to check first the longer template
% with the corresponding starting words. 
%predef_dict([days_spent_in_uk,Individual,Start,End,TotalDays], [who-person,start-date,end-date,total-number], 
%                    [Individual, spent, TotalDays, in, the, 'UK', starting, at, Start, &, ending, at, End]). 
%predef_dict([uk_tax_year_for_date,Date,Year,Start,End], [first_date-date, year-year, second_date-date, third_date-date], 
%                    [in, the, 'UK', Date, falls, in, Year, beginning, at, Start, &, ending, at, End]).
%predef_dict([myDB_entities:is_individual_or_company_on, A, B],
%                    [affiliate-affiliate, date-date],
%                    [A, is, an, individual, or, is, a, company, at, B]).
% Prolog
predef_dict([has_as_head_before, A, B, C], [list-list, symbol-term, rest_of_list-list], [A, has, B, as, head, before, C]).
predef_dict([append, A, B, C],[first_list-list, second_list-list, third_list-list], [the, concatenation, of, A, then, B, becomes, C]).
predef_dict([reverse, A, B], [list-list, other_list-list], [A, is, the, reverse, of, B]).
predef_dict([same_date, T1, T2], [time_1-time, time_2-time], [T1, is, the, same, date, as, T2]). % see reasoner.pl before/2
predef_dict([between,Minimum,Maximum,Middle], [min-date, max-date, middle-date], 
                [Middle, is, between, Minimum, &, Maximum]).
predef_dict([is_1_day_after, A, B], [date-date, second_date-date],
                [A, is, '1', day, after, B]).
predef_dict([is_days_after, A, B, C], [date-date, number-number, second_date-date],
                  [A, is, B, days, after, C]).
predef_dict([immediately_before, T1, T2], [time_1-time, time_2-time], [T1, is, immediately, before, T2]). % see reasoner.pl before/2
predef_dict([\=, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, different, from, T2]).
predef_dict([==, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, equivalent, to, T2]).
predef_dict([is_a, Object, Type], [object-object, type-type], [Object, is, of, type, Type]).
predef_dict([is_not_before, T1, T2], [time1-time, time2-time], [T1, is, not, before, T2]). % see reasoner.pl before/2
predef_dict([=, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, equal, to, T2]).
predef_dict([before, T1, T2], [time1-time, time2-time], [T1, is, before, T2]). % see reasoner.pl before/2
predef_dict([after, T1, T2], [time1-time, time2-time], [T1, is, after, T2]).  % see reasoner.pl before/2
predef_dict([member, Member, List], [member-object, list-list], [Member, is, in, List]).
predef_dict([is, A, B], [member-object, list-list], [A, is, B]). % builtin Prolog assignment
% predefined entries:
%predef_dict([assert,Information], [info-clause], [this, information, Information, ' has', been, recorded]).
predef_dict([\=@=, T1, T2], [thing_1-thing, thing_2-thing], [T1, \,=,@,=, T2]).
predef_dict([\==, T1, T2], [thing_1-thing, thing_2-thing], [T1, \,=,=, T2]).
predef_dict([=\=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,\,=, T2]).
predef_dict([=@=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,@,=, T2]).
predef_dict([==, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,=, T2]).
predef_dict([=<, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,<, T2]).
predef_dict([=<, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,<, T2]).
predef_dict([>=, T1, T2], [thing_1-thing, thing_2-thing], [T1, >,=, T2]).
predef_dict([=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =, T2]).
predef_dict([<, T1, T2], [thing_1-thing, thing_2-thing], [T1, <, T2]).
predef_dict([>, T1, T2], [thing_1-thing, thing_2-thing], [T1, >, T2]).
predef_dict([unparse_time, Secs, Date], [secs-seconds, date-date], [Secs, corresponds, to, date, Date]).
predef_dict([must_be, Type, Term], [type-type, term-term], [Term, must, be, Type]).
predef_dict([must_not_be, A, B], [term-term, variable-variable], [A, must, not, be, B]). 

% support predicates
must_be(A, var) :- var(A).
must_be(A, nonvar) :- nonvar(A).
must_be_nonvar(A) :- nonvar(A).
must_not_be(A,B) :- not(must_be(A,B)). 

has_as_head_before([B|C], B, C). 

% see reasoner.pl
%before(A,B) :- nonvar(A), nonvar(B), number(A), number(B), A < B. 

/* ---------------------------------------------------------------  meta predicates CLI */

is_it_illegal(English, Scenario) :- % only event as possibly illegal for the time being
    (parsed -> true; fail), !, 
    translate_query(English, happens(Goal, T)), % later -->, Kbs),
    %print_message(informational, "Goal Name: ~w"-[GoalName]),
    pengine_self(SwishModule), %SwishModule:query(GoalName, Goal), 
    %extract_goal_command(Question, SwishModule, Goal, Command), 
    copy_term(Goal, CopyOfGoal), 
    get_answer_from_goal(CopyOfGoal, RawGoal),  name_as_atom(RawGoal, EnglishQuestion), 
    print_message(informational, "Testing illegality: ~w"-[EnglishQuestion]),
    print_message(informational, "Scenario: ~w"-[Scenario]),
    get_assumptions_from_scenario(Scenario, SwishModule, Assumptions), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Assumptions), 
            %catch(SwishModule:holds(Goal), Error, ( print_message(error, Error), fail ) ), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            catch(SwishModule:it_is_illegal(Goal, T), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Assumptions)), 
    get_answer_from_goal(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer),  
    print_message(informational, "Answers: ~w"-[EnglishAnswer]).

% extract_goal_command(WrappedGoal, Module, InnerGoal, RealGoal)
extract_goal_command((A;B), M, (IA;IB), (CA;CB)) :-
    (extract_goal_command(A, M, IA, CA); extract_goal_command(B, M, IB, CB)), !. 
extract_goal_command((A,B), M, (IA,IB), (CA,CB)) :-
    extract_goal_command(A, M, IA, CA), extract_goal_command(B, M, IB, CB), !. 
extract_goal_command(holds(Goal,T), M, Goal, (holds(Goal,T);M:holds(Goal,T))) :- !.
extract_goal_command(happens(Goal,T), M, Goal, (happens(Goal,T);M:happens(Goal,T))) :- !.
extract_goal_command(Goal, M, Goal, M:Goal).

get_assumptions_from_scenario(noscenario, _, []) :- !.  
get_assumptions_from_scenario(Scenario, SwishModule, Assumptions) :-
    SwishModule:example(Scenario, [scenario(Assumptions, _)]).

translate_query(English_String, Goals) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    phrase(conditions(0, [], _, Goals), CTokens) -> true 
    ; ( error_notice(error, Me,Pos, ContextTokens), print_message(error, [Me,Pos,ContextTokens]), fail ). 

/* ----------------------------------------------------------------- Event Calculus  */
% holds/2
holds(Fluent, T) :-
    pengine_self(SwishModule), %trace, 
    SwishModule:happens(Event, T1), 
    rbefore(T1,T),  
    SwishModule:initiates(Event, Fluent, T1), 
    %(nonvar(T) -> rbefore(T1,T); T=(after(T1)-_)),  % T1 is strictly before T 'cos T is not a variable
    %(nonvar(T) -> rbefore(T1,T); true),
    not(interrupted(T1, Fluent, T)).

rbefore(T1, T) :-
    nonvar(T1), nonvar(T), before(T1, T). %, !.
%rbefore(T1, T) :- (var(T1); var(T)), !. % if anyone is a variable, don't compute
%rbefore(T1, (after(T2)-_)) :-
%    nonvar(T1), nonvar(T2), before(T1, T2).

% interrupted/3
interrupted(T1, Fluent, T2) :- %trace, 
    pengine_self(SwishModule),
    SwishModule:happens(Event, T), 
    rbefore(T, T2), 
    SwishModule:terminates(Event, Fluent, T), 
    (rbefore(T1, T); T1=T), !.
    %(nonvar(T2) -> rbefore(T, T2) ; true ), !.  
    %(T2=(after(T1)-_)->T2=(after(T1)-before(T)); rbefore(T,T2)). 

/* ----------------------------------------------------------------- CLI English */
% answer/1
% answer(+Query or Query Expression)
answer(English) :- %trace, 
    (parsed -> true; fail), !, 
    pengine_self(SwishModule), 
    (translate_command(SwishModule, English, GoalName, Goal, Scenario) -> true 
    ; ( print_message(error, "Don't understand this question: ~w "-[English]), !, fail ) ), % later -->, Kbs),
    copy_term(Goal, CopyOfGoal),  
    get_answer_from_goal(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, EnglishQuestion]),
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    % assert facts in scenario
    (Scenario==noscenario -> Facts = [] ; 
        (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))), !,  
    %print_message(informational, "Facts: ~w"-[Facts]), 
    extract_goal_command(Goal, SwishModule, _InnerGoal, Command), 
    %print_message(informational, "Command: ~w"-[Command]),
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch((true, Command), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)),
    get_answer_from_goal(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer),  
    print_message(informational, "Answer: ~w"-[EnglishAnswer]).

% answer/2
% answer(+Query, with(+Scenario))
answer(English, Arg) :- trace, 
    (parsed -> true; fail), !, 
    pengine_self(SwishModule), 
    (translate_command(SwishModule, English, GoalName, Goal, PreScenario) -> true 
    ; ( print_message(error, "Don't understand this question: ~w "-[English]), !, fail ) ), % later -->, Kbs),
    copy_term(Goal, CopyOfGoal),  
    get_answer_from_goal(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),   
    print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, EnglishQuestion]),
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    % assert facts in scenario
    (Scenario==noscenario -> Facts = [] ; 
        (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))), !,  
    %print_message(informational, "Facts: ~w"-[Facts]), 
    extract_goal_command(Goal, SwishModule, _InnerGoal, Command), 
    %print_message(informational, "Command: ~w"-[Command]),
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch((true, Command), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)),
    get_answer_from_goal(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer),  
    print_message(informational, "Answer: ~w"-[EnglishAnswer]).

% answer/3
% answer(+English, with(+Scenario), -Result)
answer(English, Arg, Answers) :-
    (parsed -> true; fail), !, pengine_self(SwishModule), 
    translate_command(SwishModule, English, _, Goal, PreScenario), % later -->, Kbs),
    %copy_term(Goal, CopyOfGoal), 
    %get_answer_from_goal(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    extract_goal_command(Goal, SwishModule, _InnerGoal, Command),
    (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch(Command, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)),
    get_answer_from_goal(Goal, Answers). 
    %reasoner:query_once_with_facts(Goal,Scenario,_,_E,Result).

% get_answer_from_goal/2
get_answer_from_goal((G,R), WholeAnswer) :- 
    get_answer_from_goal(G, Answer), 
    get_answer_from_goal(R, RestAnswers), 
    append(Answer, ['\n','\t',and|RestAnswers], WholeAnswer).
get_answer_from_goal(not(G), [it,is,not,the,case,that,'\n', '\t'|Answer]) :- 
    get_answer_from_goal(G, Answer).
get_answer_from_goal(happens(Goal,T), Answer) :- !,   % simple goals do not return a list, just a literal
    Goal =.. [Pred|GoalElements], dict([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), 
    process_time_term(T, TimeExplain),
    Answer = ['At', TimeExplain, it, occurs, that|ProcessedWordsAnswers].
get_answer_from_goal(holds(Goal,T), Answer) :- !, 
    Goal =.. [Pred|GoalElements], dict([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), 
    process_time_term(T, TimeExplain),
    Answer = ['At', TimeExplain, it, holds, that|ProcessedWordsAnswers], !.
get_answer_from_goal(Goal, ProcessedWordsAnswers) :-  
    Goal =.. [Pred|GoalElements], meta_dictionary([Pred|GoalElements], Types, WordsAnswer),
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), !. 
get_answer_from_goal(Goal, ProcessedWordsAnswers) :-  
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer),
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers). 

process_time_term(T,ExplainT) :- var(T), name_as_atom([a, time, T], ExplainT). % in case of vars
process_time_term(T,T) :- nonvar(T), atom(T), !. 
process_time_term(T,Time) :- nonvar(T), number(T), T>100, unparse_time(T, Time), !.  
process_time_term(T,Time) :- nonvar(T), number(T), T=<100, T=Time, !.  % hack to avoid standard time transformation
process_time_term((after(T)-Var), Explain) :- var(Var), !,
    process_time_term(T, Time), 
    name_as_atom([any, time, after, Time], Explain).
process_time_term((after(T1)-before(T2)), Explain) :- !,
    process_time_term(T1, Time1), process_time_term(T2, Time2),
    name_as_atom([any, time, after, Time1, and, before, Time2], Explain).
    
process_types_or_names([], _, _, []) :- !.
process_types_or_names([Word|RestWords], Elements, Types, PrintExpression ) :- 
    atom(Word), concat_atom(WordList, '_', Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    append(WordList, RestPrintWords, PrintExpression).
process_types_or_names([Word|RestWords], Elements, Types, PrintExpression ) :- 
    var(Word), matches_name(Word, Elements, Types, Name), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    tokenize_atom(Name, NameWords), delete_underscore(NameWords, CNameWords),
    add_determiner(CNameWords, PrintName), append(['*'|PrintName], ['*'|RestPrintWords], PrintExpression).
process_types_or_names([Word|RestWords], Elements, Types, [PrintWord|RestPrintWords] ) :- 
    matches_type(Word, Elements, Types, date), 
    ((nonvar(Word), number(Word)) -> unparse_time(Word, PrintWord); PrintWord = Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords). 
process_types_or_names([Word|RestWords], Elements, Types, [PrintWord|RestPrintWords] ) :- 
    matches_type(Word, Elements, Types, day), 
    ((nonvar(Word), number(Word)) -> unparse_time(Word, PrintWord); PrintWord = Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords). 
process_types_or_names([Word|RestWords],  Elements, Types, Output) :-
    compound(Word), 
    get_answer_from_goal(Word, PrintWord), !, % cut the alternatives
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    append(PrintWord, RestPrintWords, Output). 
process_types_or_names([Word|RestWords],  Elements, Types, [Word|RestPrintWords] ) :-
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords).

add_determiner([Word|RestWords], [Det, Word|RestWords]) :-
    name(Word,[First|_]), proper_det(First, Det).

delete_underscore([], []) :- !. 
delete_underscore(['_'|Rest], Final) :- delete_underscore(Rest, Final), !.  
delete_underscore([W|Rest], [W|Final]) :- delete_underscore(Rest, Final). 

proper_det(97, an) :- !.
proper_det(101, an) :- !.
proper_det(105, an) :- !.
proper_det(111, an) :- !.
proper_det(117, an) :- !.
proper_det(_, a). 

matches_name(Word, [Element|_], [Name-_|_], Name) :- Word == Element, !.
matches_name(Word, [_|RestElem], [_|RestTypes], Name) :-
    matches_name(Word, RestElem, RestTypes, Name). 

matches_type(Word, [Element|_], [_-Type|_], Type) :- Word == Element, !.
matches_type(Word, [_|RestElem], [_|RestTypes], Type) :-
    matches_type(Word, RestElem, RestTypes, Type). 

assert_facts(_, []) :- !. 
assert_facts(SwishModule, [F|R]) :- nonvar(F), % print_message(informational, "asserting: ~w"-[SwishModule:F]),
    assertz(SwishModule:F), assert_facts(SwishModule, R).

retract_facts(_, []) :- !. 
retract_facts(SwishModule, [F|R]) :- nonvar(F), % print_message(informational, "retracting: ~w"-[SwishModule:F]),
    retract(SwishModule:F), retract_facts(SwishModule, R). 

translate_command(SwishModule, English_String, GoalName, Goals, Scenario) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens),
    phrase(command_(GoalName, Scenario), CTokens),
    ( SwishModule:query(GoalName, Goals) -> true; (print_message(informational, "No goal named: ~w"-[GoalName]), fail) ), !. 

translate_command(_, English_String, GoalName, Goals, Scenario) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), Scenario=noscenario, GoalName=nonamed, 
    (phrase(conditions(0, [], _, Goals), CTokens) ->  true  ;
        ( error_notice(error, Me,Pos, ContextTokens), print_message(informational, "~w ~w ~w"-[Me,Pos,ContextTokens]), CTokens=[], fail )
    ). 

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
 
% show/1
show(prolog) :-
    show(rules),
    show(queries),
    show(scenarios). 

show(rules) :- % trace, 
    pengine_self(SwishModule), 
    findall((Pred :- Body), 
        (dict(PredicateElements, _, _), Pred=..PredicateElements, clause(SwishModule:Pred, Body)), Predicates),
    forall(member(Clause, Predicates), portray_clause(Clause)).

show(queries) :- % trace, 
    pengine_self(SwishModule), 
    findall((query(A,B) :- true), 
        (clause(SwishModule:query(A,B), _)), Predicates),
    forall(member(Clause, Predicates), portray_clause(Clause)).

show(scenarios) :- % trace, 
    pengine_self(SwishModule), 
    findall((example(A,B) :- true), 
        (clause(SwishModule:example(A,B), _)), Predicates),
    forall(member(Clause, Predicates), portray_clause(Clause)).

show(templates) :-
    findall(EnglishAnswer, 
        ( dictionary([_|GoalElements], Types, WordsAnswer),
        process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers),
        name_as_atom(ProcessedWordsAnswers, EnglishAnswer)), Templates), 
    forall(member(T, Templates), print_message(informational, "~w"-[T])). 

%%% ------------------------------------------------ Swish Interface to logical english
%% based on logicalcontracts' lc_server.pl

:- multifile prolog_colour:term_colours/2.
prolog_colour:term_colours(en(_Text),lps_delimiter-[classify]). % let 'en' stand out with other taxlog keywords
prolog_colour:term_colours(en_decl(_Text),lps_delimiter-[classify]). % let 'en_decl' stand out with other taxlog keywords

user:(Command1 and Command2) :-
    call(Command1), call(Command2). 
user:(answer Query with Scenario):-
    answer(Query,with(Scenario)). 
user:answer( EnText) :- answer( EnText).
user:answer( EnText, Scenario) :- answer( EnText, Scenario).
user:answer( EnText, Scenario, Result) :- answer( EnText, Scenario, Result).
user:(show Something) :- 
    show(Something). 

user:is_it_illegal( EnText, Scenario) :- is_it_illegal( EnText, Scenario).

user:query(Name, Goal) :- query(Name, Goal).

user:holds(Fluent, Time) :- holds(Fluent, Time). 

user:has_as_head_before(List, Head, Rest) :- has_as_head_before(List, Head, Rest). 

user:le_taxlog_translate( en(Text), Terms) :- le_taxlog_translate( en(Text), Terms).

user:op_stop(StopWords) :- op_stop(StopWords). 

le_taxlog_translate( EnText, Terms) :- le_taxlog_translate( EnText, someFile, 1, Terms).

% Baseline is the line number of the start of Logical English text
le_taxlog_translate( en(Text), File, BaseLine, Terms) :-
	%findall(Decl, psyntax:lps_swish_clause(en_decl(Decl),_Body,_Vars), Decls),
    %combine_list_into_string(Decls, StringDecl),
	%string_concat(StringDecl, Text, Whole_Text),
    %once( text_to_logic(Text, Terms) ),
    %catch(text_to_logic(Text, Terms), Error, ( print_message(error, Error),  showErrors(File,BaseLine) ) ).
    text_to_logic(Text, Terms) -> true; showErrors(File,BaseLine). % cut didnt work.
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

sandbox:safe_primitive(le_input:showtaxlog).
sandbox:safe_primitive(le_input:answer( _EnText)).
sandbox:safe_primitive(le_input:show( _Something)).
sandbox:safe_primitive(le_input:answer( _EnText, _Scenario)).
sandbox:safe_primitive(le_input:answer( _EnText, _Scenario, _Result)).
sandbox:safe_primitive(le_input:le_taxlog_translate( _EnText, _Terms)).
