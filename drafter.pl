:- module(_,[draft_string/2]).

:- use_module('spacy/spacy.pl').

% Knowledge page drafting aids, assuming loaded content and using Spacy parses and known knowledge pages (modules)

% TextURL,Functor,Args,Why   
%  Args is (for now..) a list of role names
%  Why includes relevant sentences and tokens within the TextURL's text, Spacy extraction
:- thread_local predicate_draft/4. 

% draft(+URL,-TmpPrologFile)
draft(URL,TmpFile):-
    must_be(atomic,URL), must_be(var,TmpFile),
    retractall(predicate_draft(URL,_,_,_)),
    refreshTokens(URL),
    forall((
        content_tokens_in(URL,SpecificURL,SI,Tokens,Extraction),
        detected_predicate(Tokens,Functor,Args,Reason)
        ),
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

draft_string(URL,S) :- 
    draft(URL,Tmp), read_file_to_string(Tmp,S,[]).

% detected_predicate(+Tokens,-Functor,-Args,-Reason)
% See predicates and notes on tags etc. in spacy.pl 
detected_predicate(Tokens,F,Args,VerbToken) :- 
    member_with([lemma=L_,tag=VerbTag,pos=verb,i=Vi_], VerbToken, Tokens), 
    VerbTag\=md, % must not be a modal auxiliary
    (L_=="be" -> (
        member_with([head=Vi_,dep=acomp,lemma=RealL,i=Vi],_,Tokens), 
        atomic_list_concat([L_,'_',RealL],LL), atom_string(L,LL)
        ) ; (
            L=L_, Vi=Vi_
        )
    ), 
    atom_string(F,L),
    findall(Arg,(
        member_with([head=Vi,dep=prep,lemma=Arg_],_ArgToken,Tokens),
        atom_string(Arg,Arg_)
        ),Args).

capitalize(X,NewX) :- 
    name(X,[First|Codes]), to_upper(First,U), name(NewX,[U|Codes]).

%TODO: handle more verb patterns, e.g. have+dobj, etc.
%TODO: generate rules, extract nouns/concepts/class hierarchies, knowledge page/reference extractor

