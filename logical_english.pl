:- module(_,[print_kp_le/1, le/2, le/1]).

:- use_module(reasoner).
:- use_module(kp_loader).
:- use_module(drafter).
:- use_module('spacy/spacy.pl').

/*
sketch of a DCG; inadequate for an actual implementation, because we need dynamic (multiple length) terminals

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

%%%% And now for something completely different: LE generation from Taxlog

% Ex: logical_english:test_kp_le('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/').
test_kp_le(KP) :-
    tmp_file(le,Tmp), atom_concat(Tmp, '.html', F),
    tell(F), print_kp_le(KP), told, www_open_url(F).

% "invokes" our SWISH renderer
le(KP,logicalEnglish([HTML])) :-
    loaded_kp(KP), le_kp_html(KP,HTML).

% Obtain navigation link to Logical English for the current SWISH editor
le(logicalEnglish([HTML])) :- 
    myDeclaredModule(KP), % no need to load, it's loaded already
    le_kp_html(KP,HTML).

print_kp_le(KP) :- 
    loaded_kp(KP), le_kp_html(KP,HTML), myhtml(HTML).

%%%%% LE proper

le_kp_html(KP, div(p(i([
        "Knowledge page carefully crafted in ", 
        a([href=EditURL,target='_blank'],"Taxlog/Prolog"), 
        " from the legislation at",br([]),
        a([href=KP,target='_blank'],KP)|PredsHTML
    ])))) :-
    (shouldMapModule(KP,KP_) -> true ; KP=KP_), %KP_ is the latest SWISH window module
    swish_editor_path(KP,EditURL),
    findall([\["&nbsp;"],div(style='border-style: inset',PredHTML)], (
        kp_predicate_mention(KP,Pred,defined), 
        findall(div(ClauseHTML), (le_clause(Pred,KP_,_Ref,LE), le_html(LE,_,ClauseHTML)), PredHTML)
        ),PredsHTML_),
    append(PredsHTML_,PredsHTML).
    
%le_html(+TermerisedLE,-PlainWordsList,-TermerisedHTMLlist)
%le_html(LE,_,_) :- mylog(LE),fail.
le_html(if(Conclusion,Conditions), RuleWords, [div(Top), div(style='padding-left:30px;',ConditionsHTML)]) :- !,
    le_html(Conclusion,ConclusionWords,ConclusionHTML), le_html(Conditions,ConditionsWords,ConditionsHTML),
    append(ConclusionHTML,[span(b(' if'))], Top),
    append([ConclusionWords,[if],ConditionsWords],RuleWords).
le_html(Binary, Words, HTML) :- \+ compound(Binary), Binary=..[Op,A,B], member(Op,[and,or]), !, 
    le_html(A,AW,Ahtml), le_html(B,BW,Bhtml), append([Ahtml,[b(" ~w "-[Op])],Bhtml],HTML),
    append([AW,[Op],BW],Words).
%TOOO: to avoid this verbose form, generate negated predicates with Spacy help, or simply declare the negated forms
le_html(not(A),[it, is, not, the, case, that|AW],HTML) :- !, le_html(A,AW,Ahtml), HTML=["it is not the case that "|Ahtml]. 
le_html(if_then_else(Condition,true,Else),Words,HTML) :- !,
    le_html(Condition,CW,CH), le_html(Else,EW,EH), append([["{ "],CH,[" or otherwise "],EH, ["}"]],HTML),
    append([["{"],CW,[or,otherwise],EW, ["}"]],Words).
le_html(if_then_else(Condition,Then,Else),Words,HTML) :- !,
    le_html(Condition,CW,CH), le_html(Then,TW,TH), le_html(Else,EW,EH), 
    append([["{ if "],CH,[" then it must be the case that "],TH,[" or otherwise "],EH,[" }"]],HTML),
    append([["{", if],CW,[then, it, must, be, the, case, that],TW,[or, otherwise],EW,["}"]],Words).
le_html(at(Conditions,KP),Words,HTML) :- !,
    le_html(Conditions,CW,CH), le_html(KP,KPW,KPH), 
    (KPH=[HREF_]->true;HREF_='??'),
    % if we have a page, navigate to it:
    ((KP=le_value(Word), kp(Word)) -> format(string(HREF),"/logicalEnglish?kp=~a",[Word]); HREF=HREF_),
    Label="other legislation",
    %TODO: distinguish external data, broken links...
    %(sub_atom(HREF,0,_,_,'http') -> Label="other legislation" ; Label="existing data"),
    append([CH,[" according to ",a([href=HREF,target('_blank')],Label)]],HTML),
    append([CW,[according,to|KPW]],Words).
le_html(on(Conditions,Time),Words,HTML) :- !,
    (Time=a(T) -> (TimeQualifier = [" at a time "|T], TQW = [at,a,time|T]); 
        (le_html(Time,TW,TH), TimeQualifier = [" at "|TH], TQW=[at|TW]) ),
    le_html(Conditions,CW,CH), %TODO: should we swap the time qualifier for big conditions, e.g. ..., at Time, blabla
    append([CH,TimeQualifier],HTML),
    append([CW,TQW],Words).
le_html(aggregate_all(Op,Each,SuchThat,Result),Words,HTML) :- !,
    le_html(Result,RW,RH), le_html(SuchThat,STW,STH), 
    (Each=a(Words) -> (EachH = ["each "|Words], EachW=[each|Words]) ; le_html(Each,EachW,EachH)),
    append([RH, [" is the ~w of "-[Op]],EachH,[" such that "],STH],HTML),
    append([RW, [is, the, Op, of],EachW,[such, that],STW],Words).
le_html(predicate(Functor,[]), Words, [span([title=Tip,style=S],HTML)]) :- !, 
    predicate_html(Functor,HTML), Words=Functor,
    atomicSentenceStyle(predicate(Functor,[]),Words,S,Tip).
le_html(predicate(Functor,[Arg]), Words, [span([title=Tip,style=S],HTML)]) :- !, 
    predicate_html(Functor,PH), le_html(Arg,AW,AH), 
    append([AH,[" "],PH],HTML),
    append([AW,Functor],Words),
    atomicSentenceStyle(predicate(Functor,[Arg]),Words,S,Tip).
le_html(predicate(Functor,[A,B]), Words, [span([title=Tip,style=S],HTML)]) :- !, 
    predicate_html(Functor,PH), le_html(A,AW,AH), le_html(B,BW,BH), 
    append([AH,[" "],PH,[" "],BH],HTML),
    append([AW,Functor,BW],Words),
    atomicSentenceStyle(predicate(Functor,[A,B]),Words,S,Tip).
le_html(predicate(Functor,[A1|Args]), Words, [span([title=Tip,style=S],HTML)]) :- !, 
    predicate_html(Functor,PH), le_html(A1,A1W,A1H),
    findall([", "|AH]/AW, (member(A,Args),le_html(A,AW,AH)), Pairs),
    findall(AH,member(AH/_,Pairs),ArgsH_),
    findall(AW,member(_/AW,Pairs),ArgsW_),
    append(ArgsH_,ArgsH), append(ArgsW_,ArgsW),
    append([PH,[" ( "],A1H,ArgsH,[")"]],HTML),
    append([Functor,["("],A1W,ArgsW,[")"]],Words),
    atomicSentenceStyle(predicate(Functor,[A1|Args]),Words,S,Tip).
le_html(a(Words), TheWords, [span(VS,[Det," ",Name])]) :- !, 
    var_style(VS), atomics_to_string(Words," ",Name), atom_chars(Name,[First|_]),
    (member(First,[a,e,i,o,u,'A','E','I','O','U']) -> Det=an ; Det=a),
    TheWords=[Det|Words].
le_html(the(Words), TheWords, [span(VS,["the ",Name])]) :- !, 
    var_style(VS), atomics_to_string(Words," ",Name),
    TheWords = [the|Words].
le_html(le_value(X), [Word], ["~w"-[X]]) :- !, format(string(Word),"~w",[X]).
le_html(L,Words,["[",span(LH),"]"]) :- is_list(L), !, 
    findall([","|XH]/XW, (member(X,L), le_html(X,XW,XH)), Pairs), 
    findall(XH,member(XH/_,Pairs),LHS),
    append(LHS,LH_), 
    findall(XW,member(_/XW,Pairs),Xwords_),
    append(Xwords_,Xwords),
    (LH_=[] -> (LH=[""], Words=["[]"]); 
        ( LH_=[_Comma|LH], append(['['|Xwords],[']'],Words))
    ).
le_html(LE,Words,["??~w??"-[LE]]) :- atomic(LE), !, Words=[LE].
le_html(Term,Words,HTML) :- compound(Term), compound_name_arguments(Term,F,Args), !, 
    le_html(predicate([F],Args),Words,HTML). %TODO: represent functions explicitly?
le_html(Term,Words,HTML) :- Term=..[F|Args], !, 
    le_html(predicate([F],Args),Words,HTML).
le_html(LE,[Word],["?~w?"-[LE]]) :- format(string(Word),"~w",[LE]). 

predicate_html(Words,[b(Name)]) :- atomics_to_string(Words, " ", Name).

var_style(style="color:#880000").

% from https://www.phpied.com/curly-underline/
bad_style('background: url(taxkb/underline.gif) bottom repeat-x;').

%! atomicSentenceStyle(+LEPredicate,+Words,-StyleString,-Tip)
%  Uses Spacy to form an opinion on the writing style, reporting it as either empty ('') or curly underline plus a text tip
% spaCyParseTokens(Text,SentenceI,Tokens)
atomicSentenceStyle(predicate([F],Args),_Words,S,Tip) :- length(Args,Arity), functor(Pred,F,Arity), system_predicate(Pred), !,
    S='', Tip="system predicate".
atomicSentenceStyle(_,Words,S,Tip) :- 
    atomic_list_concat(Words," ",Text), findall(SI/Tokens,spaCyParseTokens(Text,SI,Tokens),Sentences),
    bad_style(BS),
    (Sentences=[_/Tokens] -> (
        member_with([dep=root,pos=POS,lemma=Lemma],Tokens),
        (POS==verb -> (S='', Tip="verb 'to ~a'"-Lemma) ; 
            (S=BS, Tip="predicate sentence should have a verb, has a ~a instead"-[POS] )
        )
        );
        (S = BS,Tip="predicate sentence too complicated")
    ).


%le_clause(?H,+Module,-Ref,-LEterm)
% H is a predicate head, with or without time; if the ruel has explicit time in head, time will be explicit in the LE too
% LEterms:
%  if(Conclusion,Conditions), or/and/not/must(Condition,Obligation), if_then_else(Condition,Then,Else)
%   predicate(FunctorWords,Args); each Arg is a a/the(Words) or some_thing (anonymous var) or le_value(Term)
%   predicate(the(Words),unknown_arguments) for meta variables :-)
%   triu/false
%   aggregate_all(Operator,Each,Condition,Result)
%   at(Predicate,KP)  ...Predicate (cf. RefLink)...
%   on(Predicate,TimeExpression) ...Predicate at time ...
%TODO: return explicit time on the head
le_clause(H,M,Ref,LE) :-
    must_be(nonvar,H),
    myClause(H,M,_,Ref), 
    % hacky code extracted from clause_info/2, to make sure we do not get confused with anonymous variables 
    % (which are ommited from the variable names list)
    clause_property(Ref,file(File)), File\==user, clause_property(Ref,line_count(LineNo)),
    prolog_clause:read_term_at_line(File, LineNo, M, Clause_, _TermPos0, VarNames),
    expand_term(Clause_,RawClause),
    (RawClause = (RealH:-RawBody) -> true ; (RawClause=RealH, RawBody=true)),
    taxlogWrapper(RawBody,Explicit,Time,M,B,Ref,_IsProlog,_URL,_E),
    ((H=on(_,Time);Explicit==true)->TheH=on(RealH,Time);TheH=RealH),
    % ...now for the normal stuff:
    atomicSentence(TheH,VarNames,[],Vars1,Head),
    (B==true -> (LE=Head, VarsN=Vars1) ; (
        conditions(B,VarNames,Vars1,VarsN,Body),
        LE=if(Head,Body)
    )),
    length(VarsN,N),
    ( setof(Words,Var^member(v(Words,Var),VarsN),DistinctWords) -> true ; DistinctWords=[]),
    length(DistinctWords,Found),
    (Found==N->true ; throw("You need to use unambiguous variable names; avoid a trailing _ ?")).

bind_clause_vars([VN|VarNames],[Var|Vars]) :- !, arg(2,VN,Var), bind_clause_vars(VarNames,Vars).
bind_clause_vars([],[]).

% atomicSentence(Literal,ClauseVarNames,Vars,NewVars,LEterm)
% Vars is a list of the variables encountered so far, each a v(Words,Var)
% e.g. cgt_assets_net_value(Entity,Value) --> predicate([cgt,assets,net,value],[a([Entity]),a([Value])])

atomicSentence(at(Cond,KP),VarNames,V1,Vn,at(Conditions,Reference)) :- !,
    conditions(Cond,VarNames,V1,V2,Conditions),
    arguments([KP],VarNames,V2,Vn,[Reference]).
atomicSentence(on(Cond,Time),VarNames,V1,Vn,on(Conditions,Moment)) :- !,
    conditions(Cond,VarNames,V1,V2,Conditions),
    arguments([Time],VarNames,V2,Vn,[Moment]).
atomicSentence(true,_,V,V,true) :- !.
atomicSentence(false,_,V,V,false) :- !.
atomicSentence(Literal,VarNames,V1,Vn,predicate(Functor,Arguments)) :- Literal=..[F|Args],
    nameToWords(F,Functor), arguments(Args,VarNames,V1,Vn,Arguments).

arguments([],_,V,V,[]) :- !.
arguments([A1|An],VarNames,V1,Vn,[Argument|Arguments]) :- 
    argument(A1,VarNames,V1,V2,Argument), arguments(An,VarNames,V2,Vn,Arguments).

% argument(+A1,+VarNames,+V1,-Vn,-Argument)
argument(A1,VarNames,V1,Vn,Argument) :- var(A1), !,
    (find_var_name(A1,V1,Words) -> (
        Argument=the(Words), Vn=V1
        ) ; find_var_name(A1,VarNames,Name) -> (
            nameToWords(Name,Words),
            Argument=a(Words),
            Vn = [v(Words,A1)|V1]
        ) ; (
            Argument=some_thing, Vn=V1
    )).
argument(A1,_VarNames,V,V,le_value(A1)) :-  atomic(A1),!.
argument(A1,VarNames,V1,Vn,NewA1) :- is_list(A1),!, arguments(A1,VarNames,V1,Vn,NewA1).
argument(A1,VarNames,V1,Vn,NewA1) :- compound(A1), !, compound_name_arguments(A1,F,Args),
    arguments(Args,VarNames,V1,Vn,NewArgs), compound_name_arguments(NewA1,F,NewArgs).
argument(A1,VarNames,V1,Vn,NewA1) :-  A1=..[F|Args], !, % terms in general; add case for isExpressionFunctor(F)?
    arguments(Args,VarNames,V1,Vn,NewArgs), NewA1=..[F|NewArgs].

conditions(V,_VarNames,V1,V1,Predicate) :- var(V), !,
    (find_var_name(V,V1,Words) -> Predicate=predicate(the(Words,unknown_arguments)) ; throw("Anonymous variable as body goal"-[])).
conditions(Cond,VarNames,V1,Vn,Condition) :- Cond=..[Connective,A,B], member(Connective,[and,or,must]), !,
    conditions(A,VarNames,V1,V2,NiceA), conditions(B,VarNames,V2,Vn,NiceB), 
    Condition=..[Connective,NiceA,NiceB].
conditions(then(if(C),else(T,E)),VarNames,V1,Vn,if_then_else(Condition,Then,Else)) :- !,
    conditions(C,VarNames,V1,V2,Condition), conditions(T,VarNames,V2,V3,Then), conditions(E,VarNames,V3,Vn,Else).
conditions(then(if(C),T),VarNames,V1,Vn,must(Condition,Then)) :- !,
    conditions(C,VarNames,V1,V2,Condition), conditions(T,VarNames,V2,Vn,Then).
%TODO: forall, setof, ->, other cases in i(...)
conditions(not(Cond),VarNames,V1,Vn,not(Condition)) :- !, conditions(Cond,VarNames,V1,Vn,Condition).
conditions((A,B),VarNames,V1,Vn,Condition) :- !, conditions(and(A,B),VarNames,V1,Vn,Condition).
conditions(';'(C->T,E),VarNames,V1,Vn,LE) :- !, conditions(then(if(C),else(T,E)),VarNames,V1,Vn,LE). % not quite the same meaning!
conditions((A;B),VarNames,V1,Vn,Condition) :- !, conditions(or(A,B),VarNames,V1,Vn,Condition).
conditions(\+ A,VarNames,V1,Vn,Condition) :- !, conditions(not(A),VarNames,V1,Vn,Condition).
conditions(call(G),VarNames,V1,Vn,Condition) :- !, conditions(G,VarNames,V1,Vn,Condition).
conditions(aggregate_all(Expr,Cond,Aggregate),VarNames,V1,Vn,aggregate_all(Op,Each,SuchThat,Result)) :- Expr=..[Op,Arg], !,
    arguments([Arg],VarNames,V1,V2,[Each]),
    conditions(Cond,VarNames,V2,V3,SuchThat),
    arguments([Aggregate],VarNames,V3,Vn,[Result]).
conditions(P,VarNames,V1,Vn,Predicate) :- 
    atomicSentence(P,VarNames,V1,Vn,Predicate).

% find_var_name(Var,VarNames,Name) VarNames is a list of Name=Var or v(Words,Var)
% finds the name for a (free, unbound) variable
find_var_name(V,[VN|_VarNames],Name) :- VN=..[_,Name,Var], V==Var, !.
find_var_name(V,[_|VarNames],Name) :- find_var_name(V,VarNames,Name).

%%%% Serving Logical English versions of the KB

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- http_handler('/logicalEnglish', handle_le, []).  % parameters: either of kp or html (this uri encoded)
handle_le(Request) :-
    http_parameters(Request, [kp(Module_,[default('')]), html(ReadyHTML,[default('')])]),
    (ReadyHTML\='' -> ( 
        uri_encoded(path,ReadyHTML_,ReadyHTML),
        \[ReadyHTML_]=TheHTML, 
        assertion(Module_=='') 
        ) ; (
        atom_string(Module,Module_),
        loaded_kp(Module), le_kp_html(Module,TheHTML)
    )),
    reply_html_page([title("Logical English version")],[
        h2("Logical English"),
        \["<!-- "], % HTML comment
        "Your request: ~w"-[Request],
        \["-->"]
        |TheHTML
    ]).


:- if(current_module(swish)). %%%%% On SWISH:
:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(logical_english:print_kp_le(_)).
sandbox:safe_primitive(logical_english:le(_)).
sandbox:safe_primitive(logical_english:le(_,_)).
:- endif.