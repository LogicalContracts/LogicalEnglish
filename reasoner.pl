:- module(_ThisFileName,[query/4, query_with_facts/5, query_once_with_facts/5, expand_explanation_refs/4, explanation_node_type/2,
    run_examples/0, myClause2/8, niceModule/2, refToOrigin/2,
    after/2, not_before/2, before/2, immediately_before/2, same_date/2, this_year/1, in/2]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

:- use_module(library(aggregate)).

:- use_module(kp_loader).

% query(AtGoal,Unknowns,ExplanationTerm,Result)
%  Result will be true/false/unknown
query(Goal,Questions,E,Result) :- 
    query_with_facts(Goal,[],false,Questions,E,Result).

%! query_with_facts(+Goal,?FactsListOrExampleName,-Unknowns,-Explanation,-Result)
query_with_facts(Goal,Facts,Questions,E,Outcome) :-
    query_with_facts(Goal,Facts,false,Questions,E,Outcome).


%! query_with_facts(+Goal,?FactsListOrExampleName,+OnceUndo,-Unknowns,-Explanation,-Result)
%  query considering the given facts (or accumulated facts of all scenarios the given example name)
%  if OnceUndo, only one solution, and time execution is limited
%  Result will be true/false/unknown
%  This is NOT reentrant
query_with_facts(Goal,Facts_,OnceUndo,Questions,taxlogExplanation(E),Outcome) :-
    must_be(boolean,OnceUndo),
    (Goal=at(G,M__) -> atom_string(M_,M__) ; 
        myDeclaredModule(M_) -> Goal=G; 
        (print_message(error,"No knowledge module specified"-[]), fail)),
    context_module(Me),
    (shouldMapModule(M_,M)->true;M=M_),
    (is_list(Facts_)-> Facts=Facts_; example_fact_sequence(M,Facts_,Facts)),
    Caller = Me:(
        i(at(G,M),OnceUndo,U,Result_),
        Result_=..[Outcome,E_],
        expand_explanation_refs(E_,Facts,M,E)
        ),
    retractall(hypothetical_fact(_,_,_,_,_)),
    (OnceUndo==true -> once_with_facts(Caller, M, Facts, true) ; call_with_facts(Caller, M, Facts)),
    list_without_variants(U,Questions). % remove duplicates

%! query_once_with_facts(+Goal,?FactsListOrExampleName,-Unknowns,-Explanation,-Result)
%  query considering the given facts (or accumulated facts of all scenarios the given example name), undoes them at the end; limited execution time
query_once_with_facts(Goal,Facts_,Questions,E,Outcome) :-
    query_with_facts(Goal,Facts_,true,Questions,E,Outcome).

% list_without_variants(+L,-NL) 
% Remove duplicates from list L, without binding variables, and keeping last occurrences in their original order
list_without_variants([X|L],[X|NL]) :- !, remove_all_variants(L,X,LL), list_without_variants(LL,NL).
list_without_variants([],[]).
% remove_all_variants(+List,+Term,-NewList)
remove_all_variants(L,T,NL) :- select(X,L,LL), variant(X,T), !, remove_all_variants(LL,T,NL).
remove_all_variants(L,_,L).

niceModule(Goal,NiceGoal) :- nonvar(Goal), Goal=at(G,Ugly), moduleMapping(Nice,Ugly), !, NiceGoal=at(G,Nice).
niceModule(G,G).

% i(+AtGoal,+OnceTimed,-Unknowns,-ExplainedResult) always succeeds, with result true(Explanation) or false(Explanation)
% top level interpreter predicate; true with Unknowns\=[].... means 'unknown'
i( at(G,KP),OnceTimed,U,Result) :- % hack to use the latest module version on the SWISH window
    shouldMapModule(KP,UUID), !,
    i( at(G,UUID),OnceTimed,U,Result).
i( at(G,KP),OnceTimed,Unknowns,Result) :- !,
    reset_errors,
    context_module(M), 
    nextGoalID(ID),
    Limit=0.5, % max seconds
    Caller = i(at(G,KP),M,top_goal,top_clause,U__,E__),
    (OnceTimed==true -> IG = call_with_time_limit( Limit, Caller) ; IG = Caller), 
    ( catch( (IG,E_=E__,U=U__), time_limit_exceeded, (E_=[], U=[time_limit_exceeded], print_message(warning,"Time limit of ~w seconds exceeded by ~w at ~w"-[Limit,G,KP])) ) *-> 
        (expand_failure_trees_and_simp(E_,FailedUnknowns,E), Result=true(E)) ;
        (expand_failure_trees_and_simp([f(ID,_,_)],FailedUnknowns,E), U=[], Result=false(E)) ),
    append(U,FailedUnknowns,U_),
    maplist(niceModule,U_,Unknowns).
i( G,_,_,_) :- print_message(error,"Top goal ~w should be qualified with ' at knowledge_page'"-[G]), fail.

% i(+Goal,+AlreadyLoadedAndMappedModule,+CallerGoalID,+CallerClauseRef,-Unknowns,-Why) 
% failure means false; success with empty Unknowns list means true; 
% Unknowns contains a list of at(GoalOrErrorTerm,Module)
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
%  explanation is a list of proof-like trees: 
%   s(nodeLiteral,ClauseRef,childrenNodes); (s)success) [] denotes.. some self-evident literal; 
%   u(nodeLiteral,CallerClauseRef,[]); u)nknown (or system predicate floundering), basically similar to s
%  if nextGoalID(ID), i(G,...) fails with zero solutions, there will be a failed tree asserted with root failed(ID,...)
%  failures for not(G) goals will also leave asserted a fact failed_success(ID,Unknowns,Why)
%  successes for not(G) goals will have a Why with the underlying failure, f(NegatedGoalID,CallerID,FreeVar); 
%  these can be expanded by expand_failure_trees into f(G,CallerClausRef,Children)
%  there may be orphan failed(ID,...) facts, because we're focusing on solution-less failures only
%  this predicate is NOT thread safe
%TODO: add meta_predicate(i(0,...)) declarations...?
%TODO: ?? _-system is an "unknown" likely irrelevant, a consequence of others
%i(G,M,_,_,_,_) :- nextGoalID(ID), writeln(ID-G/M), fail.
i(G,M,_,_,_,_) :- var(G), !, throw(variable_call_at(M)).
i(true, _, _, _, U, E) :- !, U=[], E=[].
i(false, _, _, _, _U, _E) :- !, fail.
i(and(A,B), M, CID, Cref, U, E) :- !, i((A,B),M,CID,Cref,U,E).
i((A,B), M, CID, Cref, U, E) :- !, i(A,M,CID,Cref,U1,E1), i(B,M,CID,Cref,U2,E2), append(U1,U2,U), append(E1,E2,E).
i(or(A,B), M, CID, Cref, U, E) :- !, i((A;B),M,CID,Cref,U,E).
i((A;B), M, CID, Cref, U, E) :- !, (i(A,M,CID,Cref,U,E) ; i(B,M,CID,Cref,U,E)).
i(must(I,M), Mod, CID, Cref, U, E) :- !, i(then(I,M), Mod, CID, Cref, U, E).
i(\+ G,M,CID, Cref, U,E) :- !, i( not(G),M,CID,Cref,U,E).
i(not(G), M, CID, Cref, NotU, NotE) :- !, 
    newGoalID(NotID),
    % our negation as failure requires no unknowns:
    ( i( G, M, NotID, Cref, U, E) -> (
        assert( failed(NotID,CID,Cref,not(G))),
        assert( failed_success(NotID,U,E)),
        fail
    ) ; (
        NotE = [f(NotID,CID,_NotHere_TheyAreAsserted)], NotU=[]
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
% sometimes this is not used... SWI seems to expands forall(X,C) into \+ (X, \+C)
i(forall(A,B),M,CID,Cref,U,E) :- !, 
    E=[s(forall(A,B),meta,Children)],
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
    (member(failed(UB/Ei),Tuples) -> (
        assert( failed(ForID,CID,Cref,forall(A,B))),
        assert( failed_success(ForID,UB,Ei)), 
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
    aggregate:template_to_pattern(bag, Template, Pattern, M:G, Goal, Aggregate),
    i(bagof(Pattern, Goal, List),M,CID,Cref,U_,[s(_Bagof,_ClauseRef,Children_)]),
    catch( ( aggregate:aggregate_list(Aggregate, List, Result), U=U_, Children=Children_ ), 
        error(instantiation_error,_Cx), 
        (append(U_,[at(instantiation_error(G),M)],U), append(Children_,[u(instantiation_error(G),unknown,[])],Children)) 
        ).
i(findall(X,G,L),M,CID,Cref,U,E) :- !, E=[s(findall(X,G,L),meta,Children)],
    findall(X/Ui/Ei, i(G,M,CID,Cref,Ui,Ei), Tuples), 
    squeezeTuples(Tuples,L,U,Children).
i(Q,M,_CID,Cref,U,E) :- functor(Q,question,N), (N=1;N=2), !, 
    Q=..[_,Q_|_],
    (Q_=Format-Args -> format(string(Q__),Format,Args); Q_=Q__),
    U=[at(Q__,M)], E=[u(at(Q__,M),Cref,[])].
i(G,M,CID,Cref,U,E) :- system_predicate(G), !, 
    evalArgExpressions(G,M,NewG,CID,Cref,Uargs,E_),
    % floundering originates unknown:
    catch(( myCall(M:NewG), U=Uargs, E=E_), 
        error(instantiation_error,_Cx), 
        (append(Uargs,[at(instantiation_error(G),M)],U), append(E_,[u(instantiation_error(G),Cref,[])],E) )).
i(M:G,Mod,CID,Cref,U,E) :- !, i(at(G,M),Mod,CID,Cref,U,E).
i(at(G,KP),M,CID,Cref,U,E) :- shouldMapModule(KP,UUID), !, 
    i(at(G,UUID),M,CID,Cref,U,E). % use SWISH's latest editor version
i(At,_Mod,CID,Cref,U,E) :- At=at(G_,M_), !,
    atom_string(M,M_),
    (G_=on(G,Time)->true;G=G_),
    (hypothetical_clause_with_time(M,G,G__,Time_,Body,Ref) *-> ( % hypo facts: use them, ignoring the real module:
            % hypo rules can NOT depend on unknowns
            i(Body,M,-1,Ref,[],_Why), % the hypo body may fail, effectively overriding any other solutions of the next (non hypo) branch
            G=G__, Time=Time_, U=[], E=[s(At,Ref,[])] 
        ) ; (
            % attempt to load and continue, or report it as unknown:
            loaded_kp(M) -> i(G_,M,CID,Cref,U,E) ; (U=[At], E=[u(At,Cref,[])] )
        )).
i(G,M,_CID,Cref,U,E) :- unknown(G,M), !, (U=[at(G,M)],E=[ u(at(G,M),Cref,[]) ]).
%TODO: on(G,2020) means "G true on some instant in 2020"; who matches that with '20210107' ? check for clauses and hypos
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
        catch( myCall(B), error(Error,_), U_=[at(Error,M)]), % should this call be qualified with M? What when M is the SWISH module...?
        (var(U_)->U_=[];true),
        Children_=LocalE
    )),
    inc_counter(Counter), % one more solution found; this is a nonbacktrackable operation
    append(Uargs,U_,U),
    (catch(M:irrelevant_explanation(NewG),_,fail) -> E=Eargs ; (E=[s(G,Ref,Children)], append(Eargs,Children_,Children) )).

% unknown(+Goal,+Module) whether the knowledge source is currently unable to provide a result 
unknown(G,M) :- var(G), !, throw(variable_unknown_call_at(M)).
unknown(on(G,_Time),M) :- !, unknown(G,M).
unknown(G,M) :- functor(G,F,N),functor(GG,F,N), \+ hypothetical_fact(M,GG,_,_,_), \+ myClause2(GG,_,M,_,_,_,_,_).


myCall(G) :- sandbox:safe_call(G).

:- thread_local last_goal_id/1, failed/4, failed_success/3.

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

% Supports the injecting of facts for a query:
:- thread_local hypothetical_fact/5. % Module, FactTemplate, Fact, ClauseLikeBody, FakeClauseRef

hypothetical_clause_with_time(Module,Template,Head,Time,Body,Ref) :-
    hypothetical_fact(Module,Template,Head,taxlogBody(Body,Time,_URL,_E),Ref).


% myClause2(PlainHead,Time,Module,Body,Ref,IsProlog,URL,LocalExplanation)
myClause2(H,Time,M,Body,Ref,IsProlog,URL,E) :- 
    (nonvar(Ref) -> clause_property(Ref,module(M)) ; true),
    % backtrack over hypothetical facts if some is present, hence the 'soft-cut':
    % ...actually too strong, would forget existing facts in same module: (hypothetical_fact(M,H,Fact,Body_,_) *-> H=Fact ; M:clause(H,Body_,Ref)),
    % hypos with rules cause their bodies to become part of our resolvent via Body:
    (hypothetical_fact(M,H,H,Body_,Ref) ; M:clause(H,Body_,Ref)),
    ((Body_= taxlogBody(Body,Time_,URL,E_), E_\==[] ) -> (
            (Time=Time_, IsProlog=true, E=[s(E_,Ref,[])])
        ); 
        Body_=taxlogBody(Body,Time_,URL,E) -> (Time=Time_, IsProlog=false) ; 
        (Body_=Body,IsProlog=true,E=[],URL='')).

refToOrigin(Ref,URL) :-
    blob(Ref,clause), 
    myClause2(_H,_Time,Module_,_Body,Ref,_IsProlog,URL_,_E),
    !,
    (moduleMapping(Module,Module_)-> true ; Module=Module_),
    (is_absolute_url(URL_) -> URL=URL_; (
        sub_atom(Module,_,_,0,'/') -> atomic_list_concat([Module,URL_],URL) ; atomic_list_concat([Module,'/',URL_],URL)
        )).
refToOrigin(Ref_,Ref) :- term_string(Ref_,Ref).

% refToModuleAndSourceAndOrigin(ClauseRef,-Module,-SourceCode,-TextOriginURL)
refToModuleAndSourceAndOrigin(Ref,M,Source,Origin) :-
    refToOrigin(Ref,Origin),
    ((blob(Ref,clause),clause(H,B,Ref)) -> (
        with_output_to(string(Source),portray_clause((H:-B))),
        clause_property(Ref,module(M))
    )
        ; (Source="", M='???')).

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
    loaded_kp(Module),
    forall( catch(Module:example(Desc,Scenarios),error(existence_error(_, _), _),fail), (
        format(" Running example ~w~n",Desc),
        run_scenarios(Scenarios,Module,1,[],_U,_E)
    )).

%consider sequence of scenario fact sets; for now, a simple concatenation:
run_scenarios([scenario(Facts,G)|Scenarios],M,N,PreviousFacts,U,E) :- !,
    append(PreviousFacts,Facts,Facts_),
    query_once_with_facts(at(G,M),Facts_,U1,E1,Result),
    format("  Scenario ~w result   : ~w~n",[N,Result]),
    format("  Scenario ~w unknowns   : ~w~n",[N,U1]),
    format("  Scenario ~w explanation: ~w~n",[N,E1]),
    NewN is N+1,
    run_scenarios(Scenarios,M,NewN,Facts_,Un,En),
    append(U1,Un,U), append([E1],En,E).
run_scenarios([],_,_,_,[],[]).

% example_fact_sequence(+Module,?ExampleName,-Facts)
example_fact_sequence(M_,Name,Facts) :- 
    atom_string(M,M_),
    loaded_kp(M),
    (catch(M:example(Name,Scenarios),error(existence_error(_, _), _),fail) *-> true ; 
        (print_message(error,"Missing scenario for example: ~w"-[Name]), fail)),
    findall(SF,member(scenario(SF,_Assertion),Scenarios),Facts_),
    append(Facts_,Facts).


% once_with_facts(Goal,Module,AdditionalFacts,+DoUndo)
% Facts should be ground...
% asserts the facts (and deletes those with a - ) and calls Goal, stopping at the first solution, and optionally undoing the fact changes
% if a fact's predicate is undefined or not dynamic, it is declared (forever) as thread_local, 
% to support multiple clients
% BUG: not thread safe, failing to call thread_local(..) before
once_with_facts(G,M_,Facts,DoUndo) :-
    must_be(boolean,DoUndo),
    atom_string(M,M_),
    loaded_kp(M), % make sure the module is loaded
    assert_and_remember(Facts,M,from_with_facts,Undo),
    (true; DoUndo==true, once(Undo), fail),
    once(M:G),
    (DoUndo==true -> once(Undo) ; true).

% call_with_facts(+Goal,+Module,+AdditionalFacts) This does NOT undo the fact changes
call_with_facts(G,M_,Facts) :-
    atom_string(M,M_),
    loaded_kp(M), % make sure the module is loaded
    assert_and_remember(Facts,M,from_with_facts,_Undo),
    call(M:G).


% assert a list of timed facts, and returns a goal to undo the asserts
assert_and_remember([-Fact|Facts],M,Why,(Undo,Undos)) :- !,
    must_be(nonvar,Fact),
    assertion( \+ (functor(Fact,':-',_);functor(Fact,if,_)) ),
    canonic_fact_time(Fact,M,CF,Time), assert_and_remember_(delete,CF,_,Time,Why,Undo),
    assert_and_remember(Facts,M,Why,Undos).
assert_and_remember([Fact_|Facts],M,Why,(Undo,Undos)) :- must_be(nonvar,Fact_),
    % Note: the following MUST be kept in sync with taxlog2prolog/3; essencially, this assumes no transform occurs:
    (Fact_ = if(Fact,Body) -> true ; (Fact=Fact_,Body=true)), %TODO: verify that rules are not functions etc
    canonic_fact_time(Fact,M,CF,Time), assert_and_remember_(add,CF,Body,Time,Why,Undo),
    assert_and_remember(Facts,M,Why,Undos).
assert_and_remember([],_,_,true).

assert_and_remember_(Operation,M:Fact,Body,Time,Why,Undo) :- 
   %TODO: Adds could check if there's a matching clause already, to avoid spurious facts at the end of some example runs
    % abolish caused 'No permission to modify thread_local_procedure'; weird interaction with yall.pl ..??
    % ( \+ predicate_property(M:Fact,_) -> (functor(Fact,F,N), thread_local(M:F/N)) ;
    %     predicate_property(M:Fact,(dynamic)) -> true ; 
    %     (functor(Fact,F,N), dynamic(M:F/N) ) % should be thread_local(M:F/N) !!!
    % ),
    % Instead of the above complications, we now use hypothetical_fact:
    % e.g. add a fact F in one scenarion and deleting in the next, which may leave F asserted when undoing the delete
    % this seems to require either using a variant test... or demanding facts to be ground
    % hypothetical_fact(M,H,Fact,Body_,Ref)
    functor(Fact,F,N), functor(Template,F,N), 
    Add = assert( hypothetical_fact(M,Template,Fact,taxlogBody(Body,Time,'',Why),hypothetical) ),
    Delete = retractall( hypothetical_fact(M,Template,Fact,taxlogBody(Body,Time,'',Why),_) ),
    (Operation==add ->( Undo=Delete, Add) ; ( Undo=Add, Delete )).

% canonic_fact_time(+Fact,+DefaultModule,Module:Fact_,Time)
canonic_fact_time(M_:on(F,T),_,M:F,T) :- !, atom_string(M,M_).
canonic_fact_time(M_:F,_,M:F,_) :- !, atom_string(M,M_).
canonic_fact_time(at(on(F,T),M),_,M_:F,T) :- !, atom_string(M_,M).
canonic_fact_time(at(F,M),_,M_:F,_) :- !, atom_string(M_,M).
canonic_fact_time(on(F,T),M,M:F,T) :- !.
canonic_fact_time(F,M,M:F,_).

%%%%% Explanations

expand_failure_trees_and_simp(E,FailedUnknowns,ES) :-
    expand_failure_trees(E,[],FailedUnknowns,Expanded), 
    simplify_explanation(Expanded,ES).

% expand_failure_trees(+Why,Unknowns,NewUnknowns,-ExpandedWhy)  the unknows are only those in failed branches
expand_failure_trees([s(X,Ref,Children)|Wn],U1,Un,[s(X,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,U1,U2,NewChildren), expand_failure_trees(Wn,U2,Un,EWn).
expand_failure_trees([u(X,Ref,Children)|Wn],U1,Un,[u(X,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,U1,U2,NewChildren), expand_failure_trees(Wn,U2,Un,EWn).
expand_failure_trees([f(ID,CID,Children)|Wn],U1,Un,[f(G,Cref,Children)|EWn]) :- failed_success(ID,U,Why), !, 
    must_be(var,Children),
    must(failed(ID,CID,Cref,G),one),
    %Children = Why,
    expand_failure_trees(Why,U1,U2,Children),
    expand_failure_trees(Wn,U2,Un_,EWn), append(Un_,U,Un).
expand_failure_trees([f(ID,CID,Children)|Wn],U1,Un,Expanded) :- 
    must_be(var,Children),
    findall(f(ChildID,ID,_),failed(ChildID,ID,_Cref,_ChildG),Children),
    expand_failure_trees(Children,U1,U2,NewChildren),
    expand_failure_trees(Wn,U2,Un,EWn),
    (failed(ID,CID,Cref,G) -> Expanded=[f(G,Cref,NewChildren)|EWn]; append(NewChildren,EWn,Expanded)).
expand_failure_trees([],U,U,[]).

% simplify_explanation(+ExpandedWhy,-LeanerWhy)
simplify_explanation(Why,Simp) :- simplify_explanation(Why,[],_,Simp).

% simplify_explanation(+Why,+VisitedNodes,-NewVisitedNodes,-Simplified) ...Nodes are lists of (s/f/u)(Literal)
simplify_explanation([E1|En],Visited,NewVisited,Simplified) :- E1=..[Type,X,Ref,Children], Node=..[Type,X],
    ((member(Node_,Visited), variant(Node_,Node))->
        simplify_explanation(En,Visited,NewVisited,Simplified) ;
        ( 
            simplify_explanation(Children,[Node|Visited],V2,SimpChildren), simplify_explanation(En,V2,NewVisited,SimpN), 
            E1Simp=..[Type,X,Ref,SimpChildren],
            Simplified=[E1Simp|SimpN]
            )
        ).
simplify_explanation([],V,V,[]).

% expand_explanation_refs(+ExpandedWhy,+ExtraFacts,+TheirModule,-ExpandedRefLessWhy)
% TODO: recover original variable names? seems to require either some hacking with clause_info or reparsing
% transforms explanation: each nodetype(Literal,ClauseRef,Children) --> nodetype(Literal,ClauseRef,Module,SourceString,OriginURL,Children)
expand_explanation_refs([Node|Nodes],Facts,M,[NewNode|NewNodes]) :- !,
    Node=..[Type,X,Ref,Children], 
    refToModuleAndSourceAndOrigin(Ref,Module,Source,Origin),
    ((M=Module, member(XX,Facts), variant(XX,X)) -> NewOrigin=userFact ; NewOrigin=Origin),
    NewNode=..[Type,X,Ref,Module,Source,NewOrigin,NewChildren],
    expand_explanation_refs(Children,Facts,M,NewChildren),
    expand_explanation_refs(Nodes,Facts,M,NewNodes).
expand_explanation_refs([],_,_,[]).


% [s(a(1,a),<clause>(0x7f95c763bc30),[s(c(1),<clause>(0x7f95c763bd90),[]),s(t(a),<clause>(0x7f95c763c000),[])])]
explanation_node_type(s,success).
explanation_node_type(f,failure).
explanation_node_type(u,unknown). % a success depending on unknown subgoals

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
%   Arguments must be dates in iso_8601 format, e.g. '20210206' or '2021-02-06T08:25:34'
after(Later,Earlier) :- 
    parse_time(Later,L), parse_time(Earlier,E), L>E.
not_before(Later,Earlier) :-
    parse_time(Later,L), parse_time(Earlier,E), L>=E.
before(Earlier,Later) :-
    parse_time(Later,L), parse_time(Earlier,E), E<L.

%! immediately_before(?Earlier,?Later) is det.
%  Later is 24h after Earlier; at least one must be known
immediately_before(Earlier,Later) :- 
    ((nonvar(Earlier);nonvar(Later)) -> true ; throw("Unbound arguments in immediately_before"-[])),
    (nonvar(Earlier) -> (parse_time(Earlier,E), L is E+24*3600 ) ; true),
    (nonvar(Later) -> (parse_time(Later,L), E is L-24*3600) ; true),
    (var(Earlier) -> format_time(string(Earlier),"%FT%T%z",E) ; true),
    (var(Later) ->  format_time(string(Later),"%FT%T%z",L) ; true).

same_date(T1,T2) :- 
    format_time(string(S),"%F",T1), format_time(string(S),"%F",T2).

%! this_year(?Year) is det.
%  The current year
this_year(Y) :- get_time(Now), stamp_date_time(Now,date(Y,_M,_D,_,_,_,_,_,_),local).

%! in(X,List) is nondet.
%  X is in List
in(X,List) :- must_be(list,List), member(X,List).

:- if(current_module(swish)). %%%%% On SWISH:

sandbox:safe_primitive(reasoner:query(_,_,_,_)).
sandbox:safe_primitive(reasoner:query_once_with_facts(_,_,_,_,_)).
sandbox:safe_primitive(reasoner:query_with_facts(_,_,_,_,_)).



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
