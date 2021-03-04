:- module(_,[]).

:- use_module(reasoner).
:- use_module(drafter).

% sketch of DCG; inadequate, because we need dynamic (multiple length) terminals
/*
rule --> conclusion, [if], conditions.

conclusion --> atomicSentence.

conditions --> condition, moreConditions.

moreConditions --> connective, condition, moreConditions.
moreConditions --> [].

connective --> {member(C,[and,or])}, [C].

condition --> [not], atomicSentence.
condition --> [it,is,not,the,case,that], atomicSentence.
condition --> atomicSentence.

% or just hack up to ternary and quaternary and quinary...
atomicSentence(Name,[A1,A2]) --> argument(A1), {predicate(Name,[_,_])}, [Name], argument(A2).
atomicSentence(Name,[A1,A2,A3]) --> argument(A1), {predicate(Name,[_,_,Arg3])}, [Name], argument(A2), [Arg3], argument(A3).

% predicate(Name(ArgNames))  e.g.:
%  predicate(owns,[_Entity,_Thing[])    predicate('has liability'[_Asset,'of type','with value'])
%  predicate('is before',[_,_])
% ...

argument(X) --> value(X).
argument(X) --> expression(X).
argument(a(Var,Type,Letter)) --> [a,Type,Letter].
argument(a(Var,Type,Letter)) --> [an,Type,Letter].
argument(a(Var,Type,_)) --> [a,Type].
argument(a(Var,Type,_)) --> [an,Type].
argument(the(Var,Type)) --> [the,Type]. % optional Letter too
argument(the(Var,Type)+(Var\=_Other)) --> [another,Type]. % ...
argument(the(Name)) --> shortVarName(Name).
*/

%%%% And now for something completely different: LE generation from Prolog

%le_clause(?H,+Module,-Ref,-LEterm)
% LEterms:
%  if(..),or/and/not/must(..),
%   predicate(FunctorWords,Args); each Arg is a a/the(Words) or some_thing (anonymous var) or value(..)
%   predicate(the(Words),unknown_arguments) for meta variables :-)
% TODO: verify that no Weird and Weird_ are handled together!
% BUG: can't bind variables when there are anonymous vars
le_clause(H,M,Ref,LE) :-
    myClause(H,M,B,Ref), clause_info(Ref,_,_,_,[variable_names(VarNames)]),
    length(VarNames,N),
    term_variables(H:-B,AllVars),
    (length(AllVars,N) -> bind_clause_vars(VarNames,AllVars) ; throw("Don't know how to bind vars for ~w"-[(H:-B)])),
    atomicSentence(H,VarNames,[],Vars1,Head),
    (B==true -> LE=Head ; (
        LE=if(Head,Body),
        conditions(B,VarNames,Vars1,_,Body)
    )).

bind_clause_vars([VN|VarNames],[Var|Vars]) :- !, arg(2,VN,Var), bind_clause_vars(VarNames,Vars).
bind_clause_vars([],[]).

% atomicSentence(Literal,ClauseVarNames,Vars,NewVars,LEterm)
% Vars is a list of the variables encountered so far, each a v(Words,Var)
% e.g. cgt_assets_net_value(Entity,Value) --> predicate([cgt,assets,net,value],[a([Entity]),a([Value])])

atomicSentence(Literal,VarNames,V1,Vn,predicate(Functor,Arguments)) :- Literal=..[F|Args],
    nameToWords(F,Functor), arguments(Args,VarNames,V1,Vn,Arguments).

arguments([],_,V,V,[]) :- !.
arguments([A1|An],VarNames,V1,Vn,[Argument|Arguments]) :- var(A1), !,
    (find_var_name(A1,V1,Words) -> (
        Argument=the(Words),
        arguments(An,VarNames,V1,Vn,Arguments)
        ) ; find_var_name(A1,VarNames,Name) -> (
            nameToWords(Name,Words),
            Argument=a(Words),
            arguments(An,VarNames,[v(Words,A1)|V1],Vn,Arguments)
        ) ; (
            Argument=some_thing,
            arguments(An,VarNames,V1,Vn,Arguments)
            )
    ).
arguments([A1|An],VarNames,V1,Vn,[value(A1)|Arguments]) :- 
    arguments(An,VarNames,V1,Vn,Arguments).

conditions(V,_VarNames,V1,V1,Predicate) :- var(V), !,
    (find_var_name(V,V1,Words) -> Predicate=predicate(the(Words,unknown_arguments)) ; throw("Anonymous variable as body goal"-[])).
conditions(Cond,VarNames,V1,Vn,Condition) :- Cond=..[Connective,A,B], member(Connective,[and,or,must]), !,
    conditions(A,VarNames,V1,V2,NiceA), conditions(B,VarNames,V2,Vn,NiceB), 
    Condition=..[Connective,NiceA,NiceB].
%TODO: aggregates, ;/->, other cases in i(...)
conditions(not(Cond),VarNames,V1,Vn,not(Condition)) :- !,
    conditions(Cond,VarNames,V1,Vn,Condition).
conditions(P,VarNames,V1,Vn,Predicate) :- 
    atomicSentence(P,VarNames,V1,Vn,Predicate).



% find_var_name(Var,VarNames,Name) VarNames is a list of Name=Var or v(Words,Var)
find_var_name(V,[VN|_VarNames],Name) :- VN=..[_,Name,Var], V==Var, !.
find_var_name(V,[_|VarNames],Name) :- find_var_name(V,VarNames,Name).