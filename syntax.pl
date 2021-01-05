:- module(_,[
    op(1190,xfx,user:(if)),
    op(1187,xfx,user:(then)),
    op(1187,xfx,user:(must)),
    op(1185,fx,user:(if)),
    op(1185,xfy,user:else),
    op(1000,xfy,user:and), % same as ,
    op(1050,xfy,user:or), % same as ;
    op(900,fx,user:not), % same as \+
    op(700,xfx,user:in),
    op(600,xfx,user:on),
    op(1150,xfx,user:because), % to support because(on(p,t),why) if ...
    op(700,xfx,user:at), % note vs. negation...incompatible with LPS fluents
    taxlog2prolog/3
    ]).

:- use_module(library(prolog_xref)).

% user:question(_,_) :- throw(use_the_interpreter). % to avoid "undefined predicate" messages in editor
% user:question(_) :- throw(use_the_interpreter).

taxlog2prolog(if(function(Call,Result),Body), neck(if)-[delimiter-[head(meta,Call),classify],SpecB], if(function(Call,Result),Body)) :- !,
    taxlogBodySpec(Body,SpecB).
taxlog2prolog(if(on(H,T),B), neck(if)-[delimiter-[SpecH,classify],SpecB], (H:-on(B,T))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(if(H,B),neck(if)-[SpecH,SpecB],(H:-B)) :- 
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog((because(on(H,T),Why):-B), neck(clause)-[ delimiter-[delimiter-[SpecH,classify],classify], SpecB ], (H:-because(on(B,T),Why))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(mainGoal(G,Description),delimiter-[Spec,classify],(mainGoal(G,Description):-(_=1->true;G))) :- % hack to avoid 'unreferenced' highlight in SWISH
    taxlogBodySpec(G,Spec).
taxlog2prolog(example(T,Sequence),delimiter-[classify,classify],example(T,Sequence)).
taxlog2prolog(irrelevant_explanation(G),delimiter-[Spec],irrelevant_explanation(G)) :- taxlogBodySpec(G,Spec).

taxlogHeadSpec(H,head(Class, H)) :- current_editor(UUID),
    !,
    xref_module(UUID,Me),
    %mylog(H/UUID/Me),
    (xref_called(_Other,Me:H, _) -> (Class=exported) ;
        xref_called(UUID, H, _By) -> (Class=head) ;
        Class=unreferenced).
    %mylog(class/Class).
taxlogHeadSpec(H,head(head, H)).

:- multifile swish_highlight:style/3.
swish_highlight:style(neck(if),     neck, [ text(if) ]).

% :- thread_local current_module/1.
% :- multifile prolog_colour:directive_colours/2.
% prolog_colour:directive_colours((:- module(M,_)),null) :-
%     mylog(detected_module/M), % NOT CALLED AT ALL???
%     retractall(current_module(_)), assert(current_module(M)), fail.


% this must be in sync with the interpreter i(...) and prolog:meta_goal(...) hooks
% assumes a SWISH current_editor(...) exists
taxlogBodySpec(V,classify) :- var(V), !.
taxlogBodySpec(and(A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec((A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec(or(A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec((A;B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec(must(if(I),M),delimiter-[delimiter-SpecI,SpecM]) :- !, 
    taxlogBodySpec(I,SpecI), taxlogBodySpec(M,SpecM).
taxlogBodySpec(not(G),delimiter-[Spec]) :- !, 
    taxlogBodySpec(G,Spec).
taxlogBodySpec(then(if(C),else(T,Else)),delimiter-[delimiter-[SpecC],delimiter-[SpecT,SpecE]]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(T,SpecT), taxlogBodySpec(Else,SpecE).
taxlogBodySpec(then(if(C),Then),delimiter-[delimiter-[SpecC],SpecT]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(Then,SpecT).
taxlogBodySpec(forall(C,Must),control-[SpecC,SpecMust]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(Must,SpecMust).
taxlogBodySpec(setof(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). %TODO: handle vars^G
taxlogBodySpec(aggregate(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). %TODO: handle vars^G
taxlogBodySpec(on(G,_T),delimiter-[SpecG,classify] ) :- !,
    taxlogBodySpec(G,SpecG).
taxlogBodySpec(at(G,M_),delimiter-[SpecG,classify]) :- nonvar(M_), nonvar(G), !, % assuming atomic goals
    atom_string(M,M_),
    % check that the source has already been xrefed, otherwise xref will try to load it and cause a "iri_scheme" error:
    ((xref_current_source(M), xref_defined(M,G,_))-> SpecG=goal(imported(M),G) ; SpecG=goal(undefined,G)).
taxlogBodySpec(G,goal(Class,G)) :-  current_editor(UUID), taxlogGoalSpec(G, UUID, Class), !,
     mylog(goal(Class,G)).
taxlogBodySpec(_G,classify).

%TODO: meta predicates - forall, setof etc
taxlogGoalSpec(question(_), _UUID, meta) :- !.
taxlogGoalSpec(question(_,_), _UUID, meta) :- !.
taxlogGoalSpec(G, UUID, Class) :-
    (xref_defined(UUID, G, Class) -> true ; 
        %prolog_colour:built_in_predicate(G)->Class=built_in ;
        prolog_colour:goal_classification(G, Class) -> true;
        Class=undefined).

% hack to find the editor that triggered the present highlighting
current_editor(UUID) :- 
    swish_highlight:current_editor(UUID, _TB, source, Lock, _), mutex_property(Lock,status(locked(_Owner, _Count))), !.
current_editor(UUID) :- 
    %mylog('Could not find locked editor, going with the first one'), 
    swish_highlight:current_editor(UUID, _TB, source, _Lock, _), !.

user:term_expansion(T,NT) :- taxlog2prolog(T,_,NT).
