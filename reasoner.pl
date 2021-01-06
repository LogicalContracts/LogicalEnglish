:- module(_ThisFileName,[query/2,
    after/2,immediately_before/2,same_date/2,this_year/1]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

% for now assumes KB in user module

% i(+Goal,-Unknowns,-Why) 
%  explanation is a proof tree: w(nodeLiteral,childrenNodes); [] denotes.. self-evident; no negative explanations yet
% failure means false; success with empty Unknowns list means true; 
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
% _-system is an "unknown" likely irrelevant, a consequence of others
%TODO: expand functional notations, namely prior to system predicates such as between/3: user defined functions, and arithmetic
% i(G,_,_) :- mylog(i-G), fail.
i(true, [], []) :- !.
i(false, [], []) :- !, fail.
i(and(A,B), U, E) :- !, i((A,B),U,E).
i(or(A,B), U, E) :- !, i((A;B),U,E).
i((A,B), U, E) :- !, i(A,U1,E1), i(B,U2,E2), append(U1,U2,U), append(E1,E2,E).
i((A;B), U, E) :- !, (i(A,U,E) ; i(B,U,E)).
i(must(I,M), U, E) :- !, i(then(I,M),U, E).
i(not(G),U,E) :- !, i( \+ G,U,E).
i(\+ G, [], [failed(G)]) :- !, \+ i( G, [], _).
i(!,_,_) :- throw(no_cuts_allowed).
i(';'(C->T,Else), U, E) :- !, % forbid this?
    ( i(C,UC,EC) -> 
        (i(T,UT,ET), append(UC,UT,U), append(EC,ET,E))
        ; (i(Else,U,EE), E = [failed(C)|EE] )).
i(then(if(C),else(T,Else)), U, E) :- !,
    (   i(C,UC,EC), i(T,UT,ET), append(UC,UT,U), append(EC,ET,E) ; 
        i( \+ C,UC,EC), i(Else,UE,EE), append(UC,UE,U), append(EC,EE,E) ).
i(then(if(C),Then),U,E) :- !, i(then(if(C),else(Then,true)),U,E).
% aggregate, setof, forall (cf. syntax cases), and also Module:G (Prolog).
i(At,U,E) :- At=at(_,_), !, 
    (unknown(At) -> U=[At],E=[unknown(At)] ; evaluate_at(At), U=[],E=[At]).
% check because(on(G,T),Why) and on(G,T) clauses first
%TODO: on(G,2020) means "G true on some instant in 2020"
i(G,U,[]) :- system_predicate(G), !, 
    catch(G, error(instantiation_error,_Cx), U=[at(instantiation_error(G),system)]), 
    (var(U)->U=[];true).
i(G,U,E) :- 
    kbModule(M), % **** generalize...
    (M:irrelevant_explanation(G) -> E=[] ; E= [w(G,Children)]),
    M:clause(G,B), i(B,U,Children). 

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
prolog:meta_goal(aggregate(_,G,_),[G]).

:- multifile prolog:called_by/4.
prolog:called_by(on(G,_T), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
prolog:called_by(because(G,_Why), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
%prolog:called_by(aggregate(_,G,_), M, M, [G]). % why is this needed, given meta_goal(on(..))...?

% does NOT fix the "G is not called" bug: prolog:called_by(mainGoal(G,_), M, M, [G]).

system_predicate(G) :- predicate_property(G,built_in). %TODO: exclude $ ; combine with handling of because/2; any Prolog code outside our kps
%system_predicate(G) :- kbDir(D), predicate_property(G,file(F)), \+ sub_atom(F,_,_,_,D).

% toy implementation based on plain undefinedness; the real one will depend on specific arguments for each KS
% unknown(AtGoal) whether the knowledge source is currently unable to provide a result 
unknown(at(G,KS)) :- kbModule(M), functor(G,F,N),functor(GG,F,N), \+ catch(M:at(GG,KS),_,fail).

% assuming not unknown(...), ask the knowledge source for its result
evaluate_at(at(G,KS)) :- kbModule(M), M:at(G,KS).

query(G,Questions) :- 
    i(G,U,E), 
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
