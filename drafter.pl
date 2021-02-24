:- module(_,[draft_string/2, test_draft/2]).

:- use_module('spacy/spacy.pl').
:- use_module(kp_loader).

% Knowledge page drafting aids, assuming loaded content and using Spacy parses and known knowledge pages (modules)

%! test_draft(+Text,-DraftedCode)
%  Draft some source code from a given text string
test_draft(Text,DraftedCode) :-
    load_content( [_{url:testURL,text:Text}] ),
    draft_string(testURL,DraftedCode).

%  Args is (for now..) a list of role names
%  Why includes relevant sentences and tokens within the TextURL's text, Spacy extraction
:- thread_local predicate_draft/4. % TextURL,Functor,Args,Why 

draft_string(URL,S) :- 
    draft(URL,Tmp), read_file_to_string(Tmp,S,[]).

% draft(+URL,-TmpPrologFile)
draft(URL,TmpFile):-
    must_be(atomic,URL), must_be(var,TmpFile),
    retractall(predicate_draft(URL,_,_,_)),
    refreshTokens(URL),
    forall((
        content_tokens_in(URL,SpecificURL,SI,Tokens,Extraction),
        detected_predicate(Tokens,Functor,Args,Reason)
        ),
        %TODO: detect duplicates/variants
        assert(predicate_draft(URL,Functor,Args,Extraction/SpecificURL/SI/Reason)
    )),
    tmp_file_stream(TmpFile, S, [encoding(text),extension(pl)]),
    format(S,":- module('~a',[]).~n~n",[URL]),
    forall(predicate_draft(URL,Functor,Args_,Why),(
        Why=_/SpecificURL/SI/_, 
        content_tokens(SpecificURL, SI, Tokens,_), sentence(Tokens,Sentence),
        maplist(capitalize,Args_,Args),
        Pred=..[Functor|Args],
        format(S,"% ~w.~n%  Why: ~w~n%  To parse sentence:~n% parseAndSee('~a',SentenceI,Tokens,Tree).~n~n",[Pred,Why,Sentence])
        )),
    close(S).

% detected_predicate(+Tokens,-Functor,-Args,-Reason)
% See predicates and notes on tags etc. in spacy.pl 
detected_predicate(Tokens,F,Args,VerbToken) :- 
    member_with([lemma=L_,tag=VerbTag,pos=verb,i=Vi_], VerbToken, Tokens), 
    VerbTag\=md, % must not be a modal auxiliary
    (L_=="be" -> (
        member_with([head=Vi_,dep=acomp,lemma=RealL,i=Vi],Tokens), 
        atomic_list_concat([L_,'_',RealL],LL), atom_string(L,LL)
        ) ; (
            L=L_, Vi=Vi_
        )
    ), 
    atom_string(F,L),
    findall(Arg,(
        (
            member_with([head=Vi,dep=nsubj],Tokens), Arg_="Subject" 
            ; member_with([head=Vi,dep=dobj],Tokens), Arg_="Object"
            ; member_with([head=Vi,dep=prep,lemma=Arg_],Tokens)),
        atom_string(Arg,Arg_)
        ),Args).

capitalize(X,NewX) :- 
    name(X,[First|Codes]), to_upper(First,U), name(NewX,[U|Codes]).

%! nameToWords(PrologAtom,Words) is det
%  Breaks a predicate or variable name into words, if detected via underscores 
nameToWords(X,[Word]) :- \+ atomic(X), !, term_string(X,Word).
nameToWords(X,Words) :- atomics_to_string(Words,'_',X), Words=[_,_|_], !.
nameToWords(X,Words) :- X\='', camelsToList(X,Words), Words=[_,_|_], !.
nameToWords(X,[X]).

camelsToList(X,L) :- 
    must_be(atomic,X), assertion(X\==''), atom_codes(X,Codes), 
    %(code_type(C,upper)->Type=upper;code_type(C,lower)->Type=lower;Type=other),
    camelsToList(Codes,xpto,[],L).

% camelsToList(CharCodes,LastType,NextWordCharsSoFar,Words)
camelsToList([C|Codes],Type,NextCodes,NewWords) :- code_type(C,upper), Type\=upper, !,
    (NextCodes=[]->NewWords=Words ; NewWords=[W|Words]),
    atom_codes(W,NextCodes), camelsToList([C|Codes],upper,[],Words).
camelsToList([C|Codes],_,NextCodes,Words) :- !,
    (code_type(C,upper)->Type=upper;code_type(C,lower)->Type=lower;Type=other),
    append(NextCodes,[C],NewNextCodes),
    camelsToList(Codes,Type,NewNextCodes,Words).
camelsToList([],_,NextCodes,Words) :- 
    (NextCodes=[] -> Words=[] ; (atom_codes(W,NextCodes), Words=[W])).

%! predicateWords(?KP,?Pred,-FunctorWords,-WordArgsList)
%  Pred is a predicate literal template
% E.g. predicateWords(KP,Pred,PredsWords), member(F/N/Fwords/Awords,PredsWords), atomics_to_string(Fwords,' ',Fstring),  format("~w:  ~a~n",[F/N,Fstring]), forall(member(A,Awords),(atomics_to_string(A,' ',Astring),format("  ~a~n",[Astring]))), fail.
predicateWords(KP,Pred,PredsWords) :-
    setof(F/Arity/Fwords/ArgsWords, How^Args^( 
        kp_predicate_mention(KP,Pred,How), 
        functor(Pred,F,Arity), nameToWords(F,Fwords),
        predicate_literal(KP,Pred), Pred=..[F|Args],
        findall(ArgWords, (member(Arg,Args), nameToWords(Arg,ArgWords)), ArgsWords)
    ),PredsWords).


%TODO: handle more verb patterns, e.g. have+dobj, etc.
%TODO: generate rules, extract nouns/concepts/class hierarchies, knowledge page/reference extractor

