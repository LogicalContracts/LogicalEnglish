:- module(_ThisFileName,[query/4, run_examples/0, myClause2/8, niceModule/2,
    after/2, not_before/2, before/2, immediately_before/2, same_date/2, this_year/1]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

:- use_module(library(aggregate)).

:- use_module(kp_loader).

query(at(G,M),Questions,taxlogExplanation(E),Result) :- 
    i(at(G,M),U,Result_), 
    Result_=..[F,E],
    must_be(boolean,F),
    ((F==true,U==[]) -> Result=true; F==true->Result=unknown; Result=false),
    findall(at(Q,K),member(at(Q,K),U),Questions).
    % explanationHTML(E,EH), myhtml(EH).


niceModule(Goal,NiceGoal) :- nonvar(Goal), Goal=at(G,Ugly), moduleMapping(Nice,Ugly), !, NiceGoal=at(G,Nice).
niceModule(G,G).

% i(+AtGoal,-Unknowns,-ExplainedResult) always succeeds, with result true(Explanation) or false(Explanation)
% top level interpreter predicate
i( at(G,KP),U,Result) :- % hack to use the latest module version on the SWISH window
    shouldMapModule(KP,UUID), !,
    %mylog(mapped/(KP->UUID)), 
    i( at(G,UUID),U,Result).
i( at(G,KP),Unknowns,Result) :- !,
    reset_errors,
    context_module(M), 
    nextGoalID(ID),
    IG = call_with_time_limit( 0.5, i(at(G,KP),M,top_goal,top_clause,U__,E__)), % half second max
    ( catch( (IG,E_=E__,U=U__), time_limit_exceeded, (E_=[], U=[time_limit_exceeded]) ) *-> 
        (expand_failure_trees(E_,E), Result=true(E)) ;
        (expand_failure_trees([f(ID,_,_)],E), U=[], Result=false(E)) ),
    maplist(niceModule,U,Unknowns).
i( _G,_,_) :- print_message(error,top_goal_must_be_at), fail.

% i(+Goal,+AlreadyLoadedModule,+CallerGoalID,+CallerClauseRef,-Unknowns,-Why) 
% failure means false; success with empty Unknowns list means true; 
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
%  explanation is a list of proof-like trees: 
%   s(nodeLiteral,ClauseRef,childrenNodes); (s)success) [] denotes.. some self-evident literal; 
%   u(nodeLiteral,CallerClauseRef,[]); u)nknown (or system predicate floundering), basically similar to s
%  if nextGoalID(ID), i(G,...) fails with zero solutions, there will be a failed tree asserted with root failed(ID,...)
%  failures for not(G) goals will also leave asserted a fact failed_success(ID,Why)
%  successes for not(G) goals will have a Why with the underlying failure, f(NegatedGoalID,CallerID,FreeVar); 
%  these can be expanded by expand_failure_trees into f(G,CallerClausRef,Children)
%  there may be orphan failed(ID,...) facts, because we're focusing on solution-less failures only
%  this predicate is NOT thread safe
%TODO: add meta_predicate(i(0,...)) declarations...?
%TODO: ?? _-system is an "unknown" likely irrelevant, a consequence of others
% i(G,_,_,_,_,_) :- nextGoalID(ID), mylog(ID-G), fail.
i(G,M,_,_,_,_) :- var(G), !, throw(variable_call_at(M)).
i(true, _, _, _, U, E) :- !, U=[], E=[].
i(false, _, _, _, _U, _E) :- !, fail.
i(and(A,B), M, CID, Cref, U, E) :- !, i((A,B),M,CID,Cref,U,E).
i((A,B), M, CID, Cref, U, E) :- !, i(A,M,CID,Cref,U1,E1), i(B,M,CID,Cref,U2,E2), append(U1,U2,U), append(E1,E2,E).
i(or(A,B), M, CID, Cref, U, E) :- !, i((A;B),M,CID,Cref,U,E).
i((A;B), M, CID, Cref, U, E) :- !, (i(A,M,CID,Cref,U,E) ; i(B,M,CID,Cref,U,E)).
i(must(I,M), Mod, CID, Cref, U, E) :- !, i(then(I,M), Mod, CID, Cref, U, E).
i(\+ G,M,CID, Cref, U,E) :- !, i( not(G),M,CID,Cref,U,E).
i(not(G), M, CID, Cref, U, NotE) :- !, 
    U=[],
    newGoalID(NotID),
    % our negation as failure requires no unknowns:
    ( i( G, M, NotID, Cref, [], E) -> (
        assert( failed(NotID,CID,Cref,not(G))),
        assert( failed_success(NotID,E)),
        fail
    ) ; (
        NotE = [f(NotID,CID,_NotHere_TheyAreAsserted)]
    )).
i(!,_,_,_,_,_) :- throw(no_cuts_allowed).
i(';'(C->T,Else), M, CID, Cref, U, E) :- !,% should we forbid Prolog if-then-elses..?
    nextGoalID(ID),
    ( i(C,M,CID,Cref,UC,EC) -> (
        i(T,M,CID,Cref,UT,ET), 
        append(UC,UT,U), append(EC,ET,E)
    ) ; (
        % any further failures under the (failed) condition?
        ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,CID,_NotYet)] ; EC=[]),
        i(Else,M,CID,Cref,U,EE), append(EC,EE,E) 
    )).
i((If->Then),M,CID,Cref,U,E) :- !, 
    i((If->Then;fail),M,CID,Cref,U,E).
i(then(if(C),else(T,Else)), M, CID, Cref, U, E) :- !,
    nextGoalID(ID),
    (i(C,M,CID,Cref,UC,EC) *-> (
        i(T,M,CID,Cref,UT,ET), 
        append(UC,UT,U), append(EC,ET,E)
    ) ; (
        ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,CID,_NotYet)] ; EC=[]),
        i(Else,M,CID,Cref,U,EE), % no unknowns under C for sure
        append(EC,EE,E)
    )).
i(then(if(C),Then),M,CID,Cref,U,E) :- !, i(then(if(C),else(Then,true)),M,CID,Cref,U,E).
% this is actually never used... SWI expands forall(X,C) into \+ (X, \+C)
i(forall(A,B),M,CID,Cref,U,E) :- !, E=[s(forall(A,B),meta,Children)],
    newGoalID(ForID),
    findall(X, (
        i(A,M,ForID,Cref,UA,EA), 
        nextGoalID(ID),
        (i(B,M,ForID,Cref,UB,EB) -> (
            append(UA,UB,Ui),append(EA,EB,Ei),X=Ui/Ei
        ) ; (
            % failed; was there a relevant failure under B?
            ((nextGoalID(Other), Other\=ID) -> EB=[f(ID,CID,_NotYet)] ; EB=[]),
            append(EA,EB,Ei),
            X=failed(UB/Ei)
            ))
        ), Tuples),
    (member(failed(_/Ei),Tuples) -> (
        assert( failed(ForID,CID,Cref,forall(A,B))),
        assert( failed_success(ForID,Ei)), 
        fail
    ) ; (
        findall(Ui,member(Ui/_,Tuples),U_), append(U_,U),
        findall(Ei,member(_/Ei,Tuples),Children_), append(Children_,Children)
    )).
i(setof(X,G,L),M,CID,Cref,U,E) :- !, E=[s(setof(X,G,L),meta,Children)],
    wrapTemplateGoal(G,M,CID,Cref,Ui,Ei,Wrapped), %TODO: should we introduce an explicit failed node for aggregates?
    setof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(bagof(X,G,L),M,CID,Cref,U,E) :- !, E=[s(bagof(X,G,L),meta,Children)],
    wrapTemplateGoal(G,M,CID,Cref,Ui,Ei,Wrapped),
    bagof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(aggregate(Template,G,Result),M,CID,Cref,U,E) :- !, E=[s(aggregate(Template,G,Result),meta,Children)],
    % uses a bit too much of SWI internals at swipl-devel/library/aggregate.pl
    aggregate:template_to_pattern(bag, Template, Pattern, G, Goal, Aggregate),
    i(bagof(Pattern, Goal, List),M,CID,Cref,U_,[s(_Bagof,_ClauseRef,Children_)]),
    catch( ( aggregate:aggregate_list(Aggregate, List, Result), U=U_, Children=Children_ ), 
        error(instantiation_error,_Cx), 
        (append(U_,[instantiation_error(G)],U), append(Children_,[u(instantiation_error(G),unknown,[])],Children)) 
        ).
i(findall(X,G,L),M,CID,Cref,U,E) :- !, E=[s(findall(X,G,L),meta,Children)],
    findall(X/Ui/Ei, i(G,M,CID,Cref,Ui,Ei), Tuples), 
    squeezeTuples(Tuples,L,U,Children).
i(Q,M,_CID,Cref,U,E) :- functor(Q,question,N), (N=1;N=2), !, U=[at(Q,M)], E=[u(at(Q,M),Cref,[])].
i(At,_Mod,CID,Cref,U,E) :- At=at(G,M_), !, % this may cause loading of the module
    atom_string(M,M_),
    (call_at(true,M) -> i(G,M,CID,Cref,U,E) ; ( U=[At],E=[u(At,Cref,[])] )).
i(M:G,_,CID,Cref,U,E) :- !, i(G,M,CID,Cref,U,E).
i(G,M,CID,Cref,U,E) :- system_predicate(G), !, 
    evalArgExpressions(G,M,NewG,CID,Cref,Uargs,E_),
    % floundering originates unknown:
    catch((M:NewG, U=Uargs, E=E_), 
        error(instantiation_error,_Cx), 
        (append(Uargs,[at(instantiation_error(G),M)],U), append(E_,[u(instantiation_error(G),Cref,[])],E) )).
i(G,M,_CID,Cref,U,E) :- unknown(G,M), !, (U=[at(G,M)],E=[ u(at(G,M),Cref,[]) ]).
%TODO: on(G,2020) means "G true on some instant in 2020"; who matches that with '20210107' ?
i(G,M,CID,Cref,U,E) :- 
    newGoalID(NewID), create_counter(Counter),
    (true ;( % before failing, save our failure information if no solutions were found
        get_counter(Counter,0),
        \+ catch(M:irrelevant_explanation(G),_,fail),
        assert( failed(NewID,CID,Cref,G)),
        fail
        )),
    evalArgExpressions(G,M,NewG,CID,Cref,Uargs,Eargs), % failures in the expression (which would be weird btw...) stay directly under CID
    myClause(G,M,B,Ref,IsProlog,_URL,LocalE),
    (IsProlog==false -> i(B,M,NewID,Ref,U_,Children_) ; (
        catch(B,error(Error,_),U_=[at(Error,M)]),
        (var(U_)->U_=[];true),
        Children_=LocalE
    )),
    inc_counter(Counter), % one more solution found; this is a nonbacktrackable operation
    append(Uargs,U_,U),
    (catch(M:irrelevant_explanation(NewG),_,fail) -> E=Eargs ; (E=[s(G,Ref,Children)], append(Eargs,Children_,Children) )).

:- thread_local last_goal_id/1, failed/4, failed_success/2.

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

evalArgExpressions(G,M,NewG,CID,Cref,U,E) :- 
    G=..[F|Args], 
    maplist(evalExpression(M,CID,Cref),Args,Results,Us,Es),
    NewG=..[F|Results], 
    append(Us,U), append(Es,E).

% evalExpression(+Module,+Expression,-Result,+CallerID,+CallerClauseRef,-Unknowns,-WhyExplanation) expands (only) user functions
% TODO: add arithmetic expressions too...?
evalExpression(_M,_CID,_Cref,X,X,[],[]) :- var(X), !.
evalExpression(M,CID,Cref,Exp,R,U,[s(function(Exp),Ref,Children)]) :- M:clause(function(Exp,R),Body,Ref), !,
    once( i(Body,M,CID,Cref,U,Children) ).
evalExpression(_M,_CID,_,X,X,[],[]).

%wrapTemplateGoal(+Gtemplate,+Module,+CallerID,+CallerClauseRef,+Unknowns,+Explanation,-WrappedGtemplate)
% e.g. X^Y^g --> i(X^Y^i(g,Module,CID,Cref,Unknowns,Explanation))
wrapTemplateGoal(G,M,CID,Cref,U,E,i(G,M,CID,Cref,U,E)) :- var(G), !.
wrapTemplateGoal(V^G,M,CID,Cref,U,E,V^Wrapped) :- !, wrapTemplateGoal(G,M,CID,Cref,U,E,Wrapped).
wrapTemplateGoal(G,M,CID,Cref,U,E,i(G,M,CID,Cref,U,E)).

%squeezeTuples(+Tuples,-ResultsList,-Unknowns,-Explanations)
squeezeTuples(Tuples,L,U,Es) :-
    findall(X, member(X/_/_,Tuples), L), 
    findall(Ui, member(_/Ui/_,Tuples), U_), append(U_,U),
    findall(Ei, member(_/_/Ei,Tuples), Es_), append(Es_,Es).

% myClause(+Head,+Module,-Body,-IsProlog,-LocalExplanation)  IsProlog is true if the body should be called directly, without interpretation
myClause(on(H,Time),M,Body,Ref,IsProlog,URL,E) :- !, myClause2(H,Time,M,Body,Ref,IsProlog,URL,E).
myClause(H,M,Body,Ref,IsProlog,URL,E) :- myClause2(H,_Time,M,Body,Ref,IsProlog,URL,E).

% myClause2(PlainHead,Time,Module,Body,Ref,IsProlog,URL,LocalExplanation)
myClause2(H,Time,M,Body,Ref,IsProlog,URL,E) :- 
    (nonvar(Ref) -> clause_property(Ref,module(M)) ; true),
    M:clause(H,Body_,Ref), 
    ((Body_= taxlogBody(Body,Time,URL,E_), E_\==[] ) -> (
            (IsProlog=true, E=[s(E_,Ref,[])])
        ); 
        Body_=taxlogBody(Body,Time,URL,E) -> (IsProlog=false) ; 
        (Body_=Body,IsProlog=true,E=[],URL='')).

% unknown(+Goal,+Module) whether the knowledge source is currently unable to provide a result 
unknown(G,M) :- var(G), !, throw(variable_unknown_call_at(M)).
unknown(on(G,_Time),M) :- !, functor(G,F,N),functor(GG,F,N), \+ myClause2(GG,_,M,_,_,_,_,_).
unknown(G,M) :- functor(G,F,N),functor(GG,F,N), \+ myClause2(GG,_,M,_,_,_,_,_).

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
prolog:called_by(taxlogBody(G,_,_,_), M, M, [G]). 
%prolog:called_by(aggregate(_,G,_), M, M, [G]). % why is this needed, given meta_goal(on(..))...?

% does NOT fix the "G is not called" bug: prolog:called_by(mainGoal(G,_), M, M, [G]).

system_predicate(G) :- predicate_property(G,built_in). 
system_predicate(G) :- kp_dir(D), predicate_property(G,file(F)), \+ sub_atom(F,_,_,_,D).


%%%% Support for automated tests/examples

run_examples :-
    forall(kp(M),(
        format("Knowledge page ~w~n",M),
        run_examples(M)
    )).

run_examples(Module) :-
    call_at(true,Module),
    forall( catch(Module:example(Desc,Scenarios),_,fail), (
        format(" Running example ~w~n",Desc),
        run_scenarios(Scenarios,Module,1,[],_U,_E)
    )).

%consider sequence of scenario fact sets; for now, a simple concatenation:
run_scenarios([scenario(Facts,G)|Scenarios],M,N,PreviousFacts,U,E) :- !,
    append(PreviousFacts,Facts,Facts_),
    i_once_with_facts(at(G,M),Facts_,U1,E1),
    format("  Scenario ~w unknowns   : ~w~n",[N,U1]),
    format("  Scenario ~w explanation: ~w~n",[N,E1]),
    NewN is N+1,
    run_scenarios(Scenarios,M,NewN,Facts_,Un,En),
    append(U1,Un,U), append([E1],En,E).
run_scenarios([],_,_,_,[],[]).

i_once_with_facts(at(G,M),Facts,U,E) :-
    context_module(Me),
    once_with_facts( Me:i(at(G,M),U,E), M, Facts,true).

% once_with_facts(Goal,Module,AdditionalFacts,+DoUndo)
% asserts the facts and calls Goal, stopping at the first solution, and optionally undoing the fact changes
% if a fact's predicate is undefined or not dynamic, it is declared (forever) as thread_local, 
% to support multiple clients
% BUG: not thread safe, failing to call thread_local(..) before
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
assert_and_remember(M:Fact,Undo) :- functor(Fact,F,N), dynamic(M:F/N), % should be thread_local(M:F/N) !!!
    Undo = retractall(M:Fact), assert(M:Fact).

canonic_fact(M_:F,_,M:F) :- !, atom_string(M,M_).
canonic_fact(at(F,M),_,M_:F) :- !, atom_string(M_,M).
canonic_fact(F,M,M:F).

%%%%% Explanations

% expand_failure_trees(+Why,-ExpandedWhy)
expand_failure_trees([s(X,Ref,Children)|Wn],[s(X,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,NewChildren), expand_failure_trees(Wn,EWn).
expand_failure_trees([u(X,Ref,Children)|Wn],[u(X,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,NewChildren), expand_failure_trees(Wn,EWn).
expand_failure_trees([f(ID,CID,Children)|Wn],[f(G,Cref,Children)|EWn]) :- failed_success(ID,Why), !, 
    must_be(var,Children),
    must(failed(ID,CID,Cref,G),one),
    %Children = Why,
    expand_failure_trees(Why,Children),
    expand_failure_trees(Wn,EWn).
expand_failure_trees([f(ID,CID,Children)|Wn],Expanded) :- 
    must_be(var,Children),
    findall(f(ChildID,ID,_),failed(ChildID,ID,_Cref,_ChildG),Children),
    expand_failure_trees(Children,NewChildren),
    expand_failure_trees(Wn,EWn),
    (failed(ID,CID,Cref,G) -> Expanded=[f(G,Cref,NewChildren)|EWn]; append(NewChildren,EWn,Expanded)).
expand_failure_trees([],[]).

% for HTML rendering, see explanation_renderer.pl

/* Graphviz support, not very promising given the large size of our labels (predicate names)
% experimental; would need unique IDs to avoid large term duplication
explanationChild(s(_,_Ref,Children),C) :- member(C,Children).

explanationRelation(Root,Parent,Child) :- Parent=Root, explanationChild(Parent,Child).
explanationRelation(Root,Parent,Child) :- explanationChild(Root,X), explanationRelation(X,Parent,Child).

explanationGraph(E,dot(digraph([rankdir='TB'|Items]))) :-
    setof(edge(From->To,[label=""]), E_^From^To^(member(E_,E), explanationRelation(E_,From,To)), Edges),
    setof(node(N,NodeAttrs), Attrs^From^To^(member(edge(From->To,Attrs),Edges), (From=N;To=N), nodeAttributes(N,NodeAttrs)), Nodes),
    append(Edges,Nodes,Items).

nodeAttributes(s(G,_Ref,_),[label=S]) :- format(string(S),"~w",G).
nodeAttributes(unknown(at(G,K)), [label=S]) :- format(string(S),"~w",G).
nodeAttributes(failed(at(G,K)), [color=red,label=S]) :- format(string(S),"~w",G).
nodeAttributes(at(G,K), [color=green,label=S]) :- format(string(S),"~w",G).
*/

:- meta_predicate(must(0,+)).
must(G,_) :- G, !.
must(G,M) :- throw(weird_failure_of(G,M)).

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

sandbox:safe_primitive(reasoner:query(_,_,_,_)).

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
