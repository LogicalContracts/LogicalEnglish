:- module(_ThisFileName,[query/2,
    after/2, not_before/2, before/2, immediately_before/2, same_date/2, this_year/1]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

:- use_module(library(aggregate)).

:- use_module(kp_loader).

% i(+AtGoal,-Unknowns,-Why)
i( at(G,KP),U,E) :- !,context_module(M), i(at(G,KP),M,top_goal,U,E).
i( _G,_,_) :- print_message(error,top_goal_must_be_at), fail.

% i(+Goal,+AlreadyLoadedModule,+CallerGoalID,-Unknowns,-Why) 
%  explanation is a proof tree: w(nodeLiteral,ClauseRef,childrenNodes); [] denotes.. self-evident; 
%  if nextGoalID(ID), i(G,...) fails with zero solutions, there will be a failed tree asserted with root failed(ID,...)
%  failures for not(G) goals will also leave asserted a fact failed_success(ID,Why)
%   successes for not(G) goals will have a Why with f(NegatedGoalID,CallerID,FreeVar)
%  there may be orphan failed(ID,...) facts, because we're focusing on solution-less failures only
%  this predicate is NOT thread safe
%TODO: add meta_predicate(i(0,...)) declarations...?
% failure means false; success with empty Unknowns list means true; 
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
%TODO: ?? _-system is an "unknown" likely irrelevant, a consequence of others
% i(G,_,_,_) :- mylog(i-G), fail.
i(G,M,_,_,_) :- var(G), !, throw(variable_call_at(M)).
i(true, _, _, U, E) :- !, U=[], E=[].
i(false, _, _, _U, _E) :- !, fail.
i(and(A,B), M, CID, U, E) :- !, i((A,B),M,CID,U,E).
i((A,B), M, CID, U, E) :- !, i(A,M,CID,U1,E1), i(B,M,CID,U2,E2), append(U1,U2,U), append(E1,E2,E).
i(or(A,B), M, CID, U, E) :- !, i((A;B),M, CID,U,E).
i((A;B), M, CID, U, E) :- !, (i(A,M,CID,U,E) ; i(B,M,CID,U,E)).
i(must(I,M), Mod, CID, U, E) :- !, i(then(I,M), Mod, CID, U, E).
i(\+ G,M,CID, U,E) :- !, i( not(G),M,CID, U,E).
i(not(G), M, CID, U, NotE) :- !, 
    U=[],
    newGoalID(NotID),
    % our negation as failure requires no unknowns:
    ( i( G, M, NotID, [], E) -> (
        assert( failed(NotID,CID,not(G))),
        assert( failed_success(NotID,E)),
        fail
    ) ; (
        NotE = [f(NotID,CID,_NotHere_TheyAreAsserted)]
    )).
i(!,_,_,_,_) :- throw(no_cuts_allowed).
i(';'(C->T,Else), M, CID, U, E) :- !,% should we forbid Prolog if-then-elses..?
    nextGoalID(ID),
    ( i(C,M,CID, UC,EC) -> (
        i(T,M,CID,UT,ET), 
        append(UC,UT,U), append(EC,ET,E)
    ) ; (
        % any further failures under the (failed) condition?
        ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,CID,_NotYet)] ; EC=[]),
        i(Else,M,CID,U,EE), append(EC,EE,E) 
    )).
i((If->Then),M,CID,U,E) :- !, 
    i((If->Then;fail),M,CID,U,E).
i(then(if(C),else(T,Else)), M, CID, U, E) :- !,
    nextGoalID(ID),
    (i(C,M,CID,UC,EC) *-> (
        i(T,M,CID,UT,ET), 
        append(UC,UT,U), append(EC,ET,E)
    ) ; (
        ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,CID,_NotYet)] ; EC=[]),
        i(Else,M,CID,U,EE), % no unknowns under C for sure
        append(EC,EE,E)
    )).
i(then(if(C),Then),M,CID,U,E) :- !, i(then(if(C),else(Then,true)),M,CID,U,E).
i(forall(A,B),M,CID,U,E) :- !, E=[w(forall(A,B),Children)],
    newGoalID(ForID),
    findall(X, (
        i(A,M,ForID,UA,EA), 
        nextGoalID(ID),
        (i(B,M,ForID,UB,EB) -> (
            append(UA,UB,Ui),append(EA,EB,Ei),X=Ui/Ei
        ) ; (
            % failed; was there a relevant failure under B?
            ((nextGoalID(Other), Other\=ID) -> EB=[f(ID,CID,_NotYet)] ; EB=[]),
            append(EA,EB,Ei),
            X=failed(UB/Ei)
            ))
        ), Tuples),
    (member(failed(_/Ei),Tuples) -> (
        assert( failed(ForID,CID,forall(A,B))),
        assert( failed_success(ForID,Ei)), 
        fail
    ) ; (
        findall(Ui,member(Ui/_,Tuples),U_), append(U_,U),
        findall(Ei,member(_/Ei,Tuples),Children_), append(Children_,Children)
    )).
i(setof(X,G,L),M,CID,U,E) :- !, E=[w(setof(X,G,L),Children)],
    wrapTemplateGoal(G,M,CID,Ui,Ei,Wrapped), %TODO: should we introduce an explicit failed node for aggregates?
    setof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(bagof(X,G,L),M,CID,U,E) :- !, E=[w(bagof(X,G,L),Children)],
    wrapTemplateGoal(G,M,CID,Ui,Ei,Wrapped),
    bagof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(aggregate(Template,G,Result),M,CID,U,E) :- !, E=[w(aggregate(Template,G,Result),Children)],
    % uses a bit too much of SWI internals at swipl-devel/library/aggregate.pl
    aggregate:template_to_pattern(bag, Template, Pattern, G, Goal, Aggregate),
    i(bagof(Pattern, Goal, List),M,CID,U,[w(_Bagof,Children)]),
    aggregate:aggregate_list(Aggregate, List, Result).
i(findall(X,G,L),M,CID,U,E) :- !, E=[w(findall(X,G,L),Children)],
    findall(X/Ui/Ei, i(G,M,CID,Ui,Ei), Tuples), 
    squeezeTuples(Tuples,L,U,Children).
i(Q,M,_CID,U,E) :- functor(Q,question,N), (N=1;N=2), !, U=[at(Q,M)], E=[w(unknown(at(Q,M)))].
i(At,_Mod,CID,U,E) :- At=at(G,M_), !, % this may cause loading of the module
    atom_string(M,M_),
    (call_at(true,M) -> i(G,M,CID,U,E) ; ( U=[At],E=[w(unknown(At),[])] )).
i(M:G,_,CID,U,E) :- !, i(G,M,CID,U,E).
i(G,M,CID,U,E) :- system_predicate(G), !, 
    evalArgExpressions(G,M,NewG,CID,Uargs,E),
    % floundering originates unknown:
    catch(M:NewG, error(instantiation_error,_Cx), U_=[at(instantiation_error(G),M)]), 
    (var(U_)->U_=[];true),
    append(Uargs,U_,U).
i(G,M,_CID,U,E) :- unknown(G,M), !, (U=[at(G,M)],E=[ w(unknown(at(G,M)),[]) ]).
%TODO: on(G,2020) means "G true on some instant in 2020"; who matches that with '20210107' ?
i(G,M,CID,U,E) :- 
    newGoalID(NewID), create_counter(Counter),
    (true ;( % before failing, save our failure information if no solutions were found
        get_counter(Counter,0),
        \+ catch(M:irrelevant_explanation(G),_,fail),
        assert( failed(NewID,CID,G)),
        fail
        )),
    evalArgExpressions(G,M,NewG,CID,Uargs,Eargs), % failures in the expression (which would be weird btw...) stay directly under CID
    myClause(G,M,B,_Ref,IsProlog),
    (IsProlog==false -> i(B,M,NewID,U_,Children_) ; (
        catch(B,error(Error,_),U_=[at(Error,M)]),
        (var(U_)->U_=[];true),
        Children_=[]
    )),
    inc_counter(Counter), % one more solution found; this is a nonbacktrackable operation
    append(Uargs,U_,U),
    (catch(M:irrelevant_explanation(NewG),_,fail) -> E=Eargs ; (E=[w(G,Children)], append(Eargs,Children_,Children) )).

:- thread_local last_goal_id/1, failed/3, failed_success/2.

nextGoalID(ID) :- 
    (last_goal_id(Old) -> true ; Old=0), ID is Old+1.
newGoalID(ID) :- 
    (retract(last_goal_id(Old)) -> true ; Old=0), ID is Old+1, assert(last_goal_id(ID)).

% destructive counters, may also be used as destructive variables that do not lose their value during repeat/fail loops
create_counter(counter(0)).
get_counter(counter(N),N).
set_counter(Counter,N) :- Counter=counter(_), nb_setarg(1,Counter,N).
inc_counter(Counter,N) :- get_counter(Counter,N), NewN is N+1, nb_setarg(1,Counter,NewN).
inc_counter(Counter) :- inc_counter(Counter,_).

% Why: w(GoalID,ClauseID,SolutionTerm,Children)
%storeFailure() : failed(ID,CallerID,CallTerm)
%storeSuccess() : succeeded(CallerGoalID,Why)

evalArgExpressions(G,M,NewG,CID,U,E) :- 
    G=..[F|Args], 
    maplist(evalExpression(M,CID),Args,Results,Us,Es),
    NewG=..[F|Results], 
    append(Us,U), append(Es,E).

% evalExpression(+Module,+Expression,-Result,+CallerID,-Unknowns,-WhyExplanation) expands (only) user functions
% TODO: add arithmetic expressions too...?
evalExpression(_M,_CID,X,X,[],[]) :- var(X), !.
evalExpression(M,CID,Exp,R,U,[w(function(Exp),Children)]) :- M:clause(function(Exp,R),Body), !,
    once( i(Body,M,CID,U,Children) ).
evalExpression(_M,_CID,X,X,[],[]).

%wrapTemplateGoal(+Gtemplate,+Module,+CallerID,+Unknowns,+Explanation,-WrappedGtemplate)
% e.g. X^Y^g --> i(X^Y^i(g,Module,CID,Unknowns,Explanation))
wrapTemplateGoal(G,M,CID,U,E,i(G,M,CID,U,E)) :- var(G), !.
wrapTemplateGoal(V^G,M,CID,U,E,V^Wrapped) :- !, wrapTemplateGoal(G,M,CID,U,E,Wrapped).
wrapTemplateGoal(G,M,CID,U,E,i(G,M,CID,U,E)).

%squeezeTuples(+Tuples,-ResultsList,-Unknowns,-Explanations)
squeezeTuples(Tuples,L,U,Es) :-
    findall(X, member(X/_/_,Tuples), L), 
    findall(Ui, member(_/Ui/_,Tuples), U_), append(U_,U),
    findall(Ei, member(_/_/Ei,Tuples), Es_), append(Es_,Es).

% myClause(+Head,+Module,-Body,-IsProlog)  IsProlog is true if the body should be called directly, without interpretation
myClause(on(H,Time),M,Body,Ref,IsProlog) :- !, myClause2(H,Time,M,Body,Ref,IsProlog).
myClause(H,M,Body,Ref,IsProlog) :- myClause2(H,_Time,M,Body,Ref,IsProlog).

myClause2(H,Time,M,Body,Ref,IsProlog) :- 
    M:clause(H,Body_,Ref), 
    (Body_=because(on(Body,_Time),_Why) -> IsProlog=true; 
        Body_=on(Body,Time) -> IsProlog=false ; 
        (Body_=Body,IsProlog=false)).

% unknown(+Goal,+Module) whether the knowledge source is currently unable to provide a result 
unknown(G,M) :- var(G), !, throw(variable_unknown_call_at(M)).
unknown(on(G,_Time),M) :- !, functor(G,F,N),functor(GG,F,N), \+ myClause2(GG,_,M,_,_,_).
unknown(G,M) :- functor(G,F,N),functor(GG,F,N), \+ myClause2(GG,_,M,_,_,_).

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


%%%% Support for automated tests/examples

i_once_with_facts(at(G,M),Facts,U,E) :-
    context_module(Me),
    once_with_facts( Me:i(at(G,M),U,E), M, Facts,true).

% once_with_facts(Goal,Module,AdditionalFacts,+DoUndo)
% asserts the facts and calls Goal, stopping at the first solution, and optionally undoing the fact changes
% if a fact's predicate is undefined or not dynamic, it is declared (forever) as thread_local, 
% to support multiple clients
once_with_facts(G,M_,Facts,DoUndo) :-
    must_be(boolean,DoUndo),
    atom_string(M,M_),
    assert_and_remember(Facts,M,Undo),
    once(M:G),
    (DoUndo==true -> Undo ; true).

assert_and_remember([Fact|Facts],M,(Undo,Undos)) :- must_be(nonvar,Fact),
    canonic_fact(Fact,M,CF), assert_and_remember(CF,Undo),
    assert_and_remember(Facts,M,Undos).
assert_and_remember([],_,true).

assert_and_remember(M:Fact,Undo) :- \+ predicate_property(M:Fact,_), !, 
    % abolish caused 'No permission to modify thread_local_procedure'; weird interaction with yall.pl ..??
    functor(Fact,F,N), Undo=retractall(M:Fact), thread_local(M:F/N), assert(M:Fact).
assert_and_remember(CF,Undo) :- predicate_property(CF,(dynamic)), !, 
    Undo = retractall(CF), assert(CF).
assert_and_remember(M:Fact,Undo) :- functor(Fact,F,N), thread_local(M:F/N),
    Undo = retractall(M:Fact), assert(M:Fact).

canonic_fact(M_:F,_,M:F) :- !, atom_string(M,M_).
canonic_fact(at(F,M),_,M_:F) :- !, atom_string(M_,M).
canonic_fact(F,M,M:F).

%%%%%

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
not_before(Later,Earlier) :-
    parse_time(Later,L), parse_time(Earlier,E), L>=E.
before(Earlier,Later) :-
    parse_time(Later,L), parse_time(Earlier,E), E<L.

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
