:- module(_ThisFileName,[query/2,
    after/2, immediately_before/2, same_date/2, this_year/1]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

:- use_module(library(aggregate)).

:- use_module(kp_loader).

% for now assumes KB in user module

% i(+Goal,+LoadedModule,-Unknowns,-Why) 
%  explanation is a proof tree: w(nodeLiteral,childrenNodes); [] denotes.. self-evident; 
%TODO: negative explanations 
% failure means false; success with empty Unknowns list means true; 
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
% _-system is an "unknown" likely irrelevant, a consequence of others
%TODO: expand functional notations, namely prior to system predicates such as between/3: user defined functions, and arithmetic
% i(G,_,_) :- mylog(i-G), fail.
i(G,M,_,_) :- var(G), !, throw(variable_call_at(M)).
i(true, _, [], []) :- !.
i(false, _, [], []) :- !, fail.
i(and(A,B), M, U, E) :- !, i((A,B),M,U,E).
i((A,B), M, U, E) :- !, i(A,M,U1,E1), i(B,M,U2,E2), append(U1,U2,U), append(E1,E2,E).
i(or(A,B), M, U, E) :- !, i((A;B),M, U,E).
i((A;B), M, U, E) :- !, (i(A,M,U,E) ; i(B,M,U,E)).
i(must(I,M), Mod, U, E) :- !, i(then(I,M), Mod, U, E).
i(not(G),M,U,E) :- !, i( \+ G,M,U,E).
i(\+ G, M, [], [w(not(G),[])]) :- !, \+ i( G, M, [], _).
i(!,_,_,_) :- throw(no_cuts_allowed).
i(';'(C->T,Else), M, U, E) :- !, % forbid this?
    ( i(C,M,UC,EC) -> 
        (i(T,M,UT,ET), append(UC,UT,U), append(EC,ET,E))
        ; (i(Else,M,U,EE), E = [w(not(C),[])|EE] )).
i((If->Then),M,U,E) :- !, % forbid this?
    (i(If,M,UI,EI) -> (i(Then,M,UT,ET), append(UI,UT,U), append(EI,ET,E))).
i(then(if(C),else(T,Else)), M, U, E) :- !,
    (   i(C,M,UC,EC), i(T,M,UT,ET), append(UC,UT,U), append(EC,ET,E) ; 
        i( \+ C,M,UC,EC), i(Else,M,UE,EE), append(UC,UE,U), append(EC,EE,E) ).
i(then(if(C),Then),M,U,E) :- !, i(then(if(C),else(Then,true)),M,U,E).
i(forall(A,B),M,U,[w(forall(A,B),Children)]) :- !, 
    findall(X, (i(A,M,UA,EA), (i(B,M,UB,EB) -> (append(UA,UB,Ui),append(EA,EB,Ei),X=Ui/Ei) ; X=failed)), Tuples),
    \+ member(failed,Tuples),
    findall(Ui,member(Ui/_,Tuples),U_), append(U_,U),
    findall(Ei,member(_/Ei,Tuples),Children_), append(Children_,Children).
i(setof(X,G,L),M,U,[w(setof(X,G,L),Children)]) :- !, 
    wrapTemplateGoal(G,M,Ui,Ei,Wrapped),
    setof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(bagof(X,G,L),M,U,[w(bagof(X,G,L),Children)]) :- !, 
    wrapTemplateGoal(G,M,Ui,Ei,Wrapped),
    bagof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(aggregate(Template,G,Result),M,U,[w(aggregate(Template,G,Result),Children)]) :- !, 
    % uses a bit too much of SWI internals at swipl-devel/library/aggregate.pl
    aggregate:template_to_pattern(bag, Template, Pattern, G, Goal, Aggregate),
    i(bagof(Pattern, Goal, List),M,U,[w(_Bagof,Children)]),
    aggregate:aggregate_list(Aggregate, List, Result).
i(findall(X,G,L),M,U,[w(findall(X,G,L),Children)]) :- !, 
    findall(X/Ui/Ei, i(G,M,Ui,Ei), Tuples), 
    squeezeTuples(Tuples,L,U,Children).
i(Q,M,[at(Q,M)],[w(unknown(at(Q,M)))]) :- functor(Q,question,N), (N=1;N=2), !.
i(At,_Mod,U,E) :- At=at(G,M), !, % this may cause loading of the module
    (call_at(true,M) -> i(G,M,U,E) ; ( U=[At],E=[w(unknown(At),[])] )).
i(M:G,_,U,E) :- !, i(G,M,U,E).
i(G,_,U,[]) :- system_predicate(G), !, 
    catch(G, error(instantiation_error,_Cx), U=[at(instantiation_error(G),system)]), 
    (var(U)->U=[];true).
i(G,M,U,E) :- unknown(G,M), !, (U=[at(G,M)],E=[ w(unknown(at(G,M)),[]) ]).
%TODO: on(G,2020) means "G true on some instant in 2020"; who matches that with '20210107' ?
i(G,M,U,E) :- 
    (catch(M:irrelevant_explanation(G),_,fail) -> E=[] ; E= [w(G,Children)]),
    myClause(G,M,B),
    i(B,M,U,Children). 

%wrapTemplateGoal(+Gtemplate,+Module,+Unknowns,+Explanation,-WrappedGtemplate)
% e.g. X^Y^g --> i(X^Y^i(g,Module,Unknowns,Explanation))
wrapTemplateGoal(G,M,U,E,i(G,M,U,E)) :- var(G), !.
wrapTemplateGoal(V^G,M,U,E,V^Wrapped) :- !, wrapTemplateGoal(G,M,U,E,Wrapped).
wrapTemplateGoal(G,M,U,E,i(G,M,U,E)).

%squeezeTuples(+Tuples,-ResultsList,-Unknowns,-Explanations)
squeezeTuples(Tuples,L,U,Es) :-
    findall(X, member(X/_/_,Tuples), L), 
    findall(Ui, member(_/Ui/_,Tuples), U_), append(U_,U),
    findall(Ei, member(_/_/Ei,Tuples), Es_), append(Es_,Es).

% myClause(+Head,+Module,-Body)  also executes Prolog bodies
myClause(on(H,Time),M,Body) :- !, myClause2(H,Time,M,Body).
myClause(H,M,Body) :- myClause2(H,_Time,M,Body).

myClause2(H,Time,M,Body) :- 
    M:clause(H,Body_), 
    (Body_=because(on(Prolog,_Time),_Why) -> ( catch(Prolog,Ex,(print_message(error,Ex), fail)), Body=true); 
        Body_=on(Body,Time) -> true ; 
        Body_=Body).

% unknown(+Goal,+Module) whether the knowledge source is currently unable to provide a result 
unknown(G,M) :- var(G), !, throw(variable_unknown_call_at(M)).
unknown(on(G,_Time),M) :- !, functor(G,F,N),functor(GG,F,N), \+ myClause(GG,M,_).
unknown(G,M) :- functor(G,F,N),functor(GG,F,N), \+ myClause(GG,M,_).

:- multifile prolog:meta_goal/2. % for xref
prolog:meta_goal(at(G,M),[M_:G]) :- nonvar(M), atom_string(M_,M).
% next two handled by declare_our_metas:
%prolog:meta_goal(on(G,_Time),[G]).
%prolog:meta_goal(because(G,_Why),[G]).
prolog:meta_goal(and(A,B),[A,B]).
prolog:meta_goal(or(A,B),[A,B]).
prolog:meta_goal(must(A,B),[A,B]).
prolog:meta_goal(not(A),[A]).
prolog:meta_goal(then(if(C),else(T,Else)),[C,T,Else]).
prolog:meta_goal(then(if(C),Then),[C,Then]) :- Then\=else(_,_).
prolog:meta_goal(aggregate(_,G,_),[G]). % is this necessary...?

:- multifile prolog:called_by/4.
prolog:called_by(on(G,_T), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
prolog:called_by(because(G,_Why), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
%prolog:called_by(aggregate(_,G,_), M, M, [G]). % why is this needed, given meta_goal(on(..))...?

% does NOT fix the "G is not called" bug: prolog:called_by(mainGoal(G,_), M, M, [G]).

system_predicate(G) :- predicate_property(G,built_in). 
system_predicate(G) :- kp_dir(D), predicate_property(G,file(F)), \+ sub_atom(F,_,_,_,D).

% assuming not unknown(...), ask the knowledge source for its result
evaluate_at(at(G,KS)) :- kbModule(M), M:at(G,KS).

query(at(G,M),Questions) :- 
    i(G,M,U,E), 
    findall(at(Q,K),(member(at(Q,K),U), K\=system),Questions),
    explanationHTML(E,EH), myhtml(EH).


% works ok but not inside SWISH because of its style clobbering ways:
explanationHTML(w(G,C),[li(title="Rule inference step","~w"-[G]),ul(CH)]) :- explanationHTML(C,CH).
%explanationHTML(unknown(at(G,K)),[li([style="color:blue",title="Unknown"],a(href=K,"~w"-[G]))]).
explanationHTML(unknown(at(G,K)),[li([p("UNKNOWN: ~w"-[G]),p(i(K))])]).
explanationHTML(failed(G),[li([title="Failed goal"],span(style="color:red","FALSE: ~w"-[G]))]).
%explanationHTML(at(G,K),[li(style="color:green",a(href=K,"~w"-[G]))]).
explanationHTML(at(G,K),[li([p("~w"-[G]),p(i(K))])]).
explanationHTML([C1|Cn],CH) :- explanationHTML(C1,CH1), explanationHTML(Cn,CHn), append(CH1,CHn,CH).
explanationHTML([],[]).

/* Graphviz support, not very promising given the large size of our labels (predicate names)
% experimental; would need unique IDs to avoid large term duplication
explanationChild(w(_,Children),C) :- member(C,Children).

explanationRelation(Root,Parent,Child) :- Parent=Root, explanationChild(Parent,Child).
explanationRelation(Root,Parent,Child) :- explanationChild(Root,X), explanationRelation(X,Parent,Child).

explanationGraph(E,dot(digraph([rankdir='TB'|Items]))) :-
    setof(edge(From->To,[label=""]), E_^From^To^(member(E_,E), explanationRelation(E_,From,To)), Edges),
    setof(node(N,NodeAttrs), Attrs^From^To^(member(edge(From->To,Attrs),Edges), (From=N;To=N), nodeAttributes(N,NodeAttrs)), Nodes),
    append(Edges,Nodes,Items).

nodeAttributes(w(G,_),[label=S]) :- format(string(S),"~w",G).
nodeAttributes(unknown(at(G,K)), [label=S]) :- format(string(S),"~w",G).
nodeAttributes(failed(at(G,K)), [color=red,label=S]) :- format(string(S),"~w",G).
nodeAttributes(at(G,K), [color=green,label=S]) :- format(string(S),"~w",G).
*/

%%%% Common background knowledge, probably to go elsewhere:

%Time predicates; they assume times are atoms in iso_8601 format

%!  after(+Later,+Earlier) is det.
%   Arguments must be atoms in iso_8601 format

after(Later,Earlier) :- 
    parse_time(Later,L), parse_time(Earlier,E), L>E.

% tests/generates previous day:
immediately_before(Earlier,Later) :- 
    parse_time(Later,L), E is L-24*3600, 
    (var(Earlier) -> format_time(string(Earlier),"%FT%T%z",E) ; parse_time(Earlier,E_), same_date(E,E_)).
same_date(T1,T2) :- 
    format_time(string(S),"%F",T1), format_time(string(S),"%F",T2).

this_year(Y) :- get_time(Now), stamp_date_time(Now,date(Y,_M,_D,_,_,_,_,_,_),local).

user:in(X,List) :- member(X,List).

:- if(current_module(swish)). %%%%% On SWISH:

:- use_module(swish(lib/html_output),[html/1]). 
% hack to avoid SWISH errors:
myhtml(H) :-  pengine_self(SwishModule), SwishModule:html(H).

kbModule(M) :- pengine_self(M).


:- else. % On command-line SWI-Prolog, no user restrictions:

:- use_module(library(http/html_write)).
html(Spec) :-
    phrase(html(Spec), Tokens),
    with_output_to(
        string(HTML),
        print_html(current_output, Tokens)),
    format('~w', [HTML]).

myhtml(Out) :- writeln(Out), writeln("---------"), html(Out).

kbModule(user).
:- endif.
