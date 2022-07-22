/* Copyright [2021] Initial copyright holders by country: 
LodgeIT (AU), AORA Law (UK), Bob Kowalski (UK), Miguel Calejo (PT), Jacinto Dávila (VE)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

:- module(_ThisFileName,[start_api_server/0]).

% API for client apps to use the reasoner and drafter

% Adapted from https://github.com/SWI-Prolog/packages-pengines/blob/master/examples/server.pl :
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).

:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(term_to_json)).
:- use_module(library(http/http_parameters)).

:- use_module(reasoner).
:- use_module('spacy/spacy.pl').
:- use_module(drafter).
:- use_module(kp_loader).
:- use_module(syntax).
:- use_module(reasoner,[taxlogWrapper/10]).
:- use_module(le_input,[text_to_logic/2]).

:- if(current_module(swish)). %%%%% On SWISH:

start_api_server :- print_message(informational,"No need to start API server, SWISH already running"-[]).

:- else. % On command-line SWI-Prolog, no user restrictions:

% Need to call thi to respond to REST API requests:
start_api_server :- start_api_server(3050).
start_api_server(Port) :- http_server(http_dispatch, [port(Port)]).
:- endif.

% Session module management
:- thread_local le_program_module/1. % the default, "user" module, where module-less KRT files get loaded into
% May generate new module name:
set_le_program_module(M) :- var(M), !, gensym(leSessionModule, M), set_le_program_module(M).
set_le_program_module(M) :- 
    retractall(le_program_module(_)), assert(le_program_module(M)).

safe_file(F) :- sub_atom(F,_,_,_,'/moreExamples/').


:- http_handler('/taxkbapi', handle_api, []).  % this defines a web server endpoint
handle_api(Request) :-
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    %asserta(my_request(Request)), % for debugging
    %print_message(informational,"Request Payload: ~w"-[Payload]),
    assertion(Payload.token=='myToken123'),
    (entry_point(Payload,Result)->true;Result=_{error:"Goal failed"}),
    %print_message(informational,"returning: ~w"-[Result]),
    reply_json_dict(Result).

:- discontiguous api:entry_point/2.

% Define our adhoc REST API; more general Prolog querying at
%  https://pengines.swi-prolog.org/docs/documentation.html (Javascript) or
%  https://www.swi-prolog.org/pengines/PenginesFromPython.md (Python)
% Examples (resp: true, unknown, false):
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"a(1,Y)", "module":"http://tests.com"}' http://localhost:3050/taxkbapi
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"testForall([1,2])", "module":"http://tests.com"}' http://localhost:3050/taxkbapi
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"testForall([1,2,9])", "module":"http://tests.com"}' http://localhost:3050/taxkbapi
% Example with hypothetical facts:
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"a(13,Y)", "facts":["d(13)"], "module":"http://tests.com"}' http://localhost:3050/taxkbapi
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"a(13,Y)",  "module":"http://tests.com"}' http://localhost:3050/taxkbapi

% {operation: query, theQuery: "a(1,Y)", module:"https://tests.com"} --> {results:ResultsArray}
%   each result is a {result: true/false/unknown, bindings:VarsValuesArray, unknowns: ArrayOfTerm, why: ExplanationTerm}
entry_point(R, _{results:Results}) :- get_dict(operation,R,query), !, 
    term_string(Query,R.theQuery,[variable_names(VarPairs_)]),
    (get_dict(facts,R,Facts_) -> (is_list(Facts_) -> maplist(term_string,Facts,Facts_) ; Facts=Facts_) ; Facts=[]),
    findall( _{bindings:VarPairs, unknowns:U, result:Result, why:E}, (
        query_with_facts(at(Query,R.module),Facts,unknowns(U_),taxlogExplanation(E_),Result),
        makeBindingsDict(VarPairs_,VarPairs),
        makeUnknownsArray(U_,U),
        makeExplanationTree(E_,E)
        ), Results).

% Example:
%  curl --header "Content-Type: application/json" --request POST --data '{"operation":"draft", "pageURL":"http://mysite/page1#section2",  "content":[{"url":"http://mysite/page1#section2!chunk1", "text":"john flies by instruments"}, {"url":"http://mysite/page1#section2!chunk2", "text":"miguel drives with gusto"}]}' http://localhost:3050/taxkbapi
% {operation:draft, pageURL:U, content:Items} --> {pageURL:U, draft:PrologText}
%   each item is a {url:..,text:...} 
entry_point(R, _{pageURL:ThePage, draft:Draft}) :- get_dict(operation,R,draft), !, 
    load_content(R.content),
    ThePage = R.pageURL,
    draft_string(R.pageURL,Draft).

% Example: see Javascript example in clientExample/
% Translates a LE program to a Prolog program
entry_point(R, _{prolog:Program, kb:KB, predicates:Predicates, examples:Examples}) :- get_dict(operation,R,le2prolog), !, 
    le2prologTerms(R.le,KB,Terms,Preds,Examples),
    with_output_to(string(Program),forall(member(Term,Terms), portray_clause(Term) ) ),
    findall(Pred,(member(Pred_,Preds), term_string(Pred_,Pred)),Predicates).

%TODO: verify if JD initializes parsed etc correctly; they may be prone to threading bugs under the web server thread pool
le2prologTerms(LE,KB,Clauses,Preds,Examples) :-
    text_to_logic(LE,X), 
    findall(Prolog, (
        member(T,X), semantics2prolog(T,_,Prolog_), 
        ((Prolog_=(Head:-RawBody), taxlogWrapper(RawBody,_,_,_,Body,_,_,_,_,_)) -> Prolog=(Head:-Body) 
            ; Prolog=Prolog_)
        ),Clauses),
    (member(kbname(KB),X)->true;KB=null),
    (member(predicates(Preds),X) -> true; Preds=[]),

    findall(_{name:Name, scenarios:Scenarios}, (
        member(example(Name,Scenarios_),X), Name\==null,
        findall( _{assertion:Assertion,clauses:ScenarioProgram},(
            member(scenario(Clauses_,Assertion_),Scenarios_),
            term_string(Assertion_,Assertion),
            with_output_to(string(ScenarioProgram), forall(member(Clause_,Clauses_), portray_clause(Clause_)))
            ), Scenarios)
        ),Examples).


entry_point(R, _{sessionModule:M, kb:KB, predicates:Predicates, examples:Examples, language:Lang}) :- get_dict(operation,R,load), !, 
    set_le_program_module(M),
    print_message(informational,"Created module ~w"-[M]),

    (get_dict(le,R,LE) -> (
            Lang=le,
            le2prologTerms(LE,KB,Clauses,Preds,Examples),
            findall(Pred,(member(Pred_,Preds), term_string(Pred_,Pred)),Predicates),
            forall(member(Clause,Clauses),M:assert(Clause))
        ) ; (
            assertion(safe_file(R.file)),
            (sub_atom(R.file,_,_,0,'le') -> (
                    Lang=le,
                    read_file_to_string(R.file,LE,[]),
                    le2prologTerms(LE,KB,Clauses,Preds,Examples),
                    findall(Pred,(member(Pred_,Preds), term_string(Pred_,Pred)),Predicates),
                    forall(member(Clause,Clauses),M:assert(Clause))
                ) ; (
                    Lang=prolog,
                    load_files(R.file,[module(M)])
                ) 
            )    
        )
    ).




entry_point(R,Result) :- get_dict(operation,R,load), !,
        Result_ = _{session:UUID, friendlyName:KBname, language:Language, languages:Languages},
        (get_dict(previousSession,R,UUID) -> (getSessionModule(UUID,Module),Errors_=[]) ; 
         get_dict(sessionState,R,SessionState) -> (restoreSession(SessionState,Module,UUID),Errors_=[]) ; (
                get_dict(kb,R,KB__),
                (is_list(KB__)->KB_=KB__;KB_=[KB__]),
                krt_examples_dir(ED), 
                findall(KB,( member(A_KB,KB_), format(string(KB),"~a/~a",[ED,A_KB]), assertion(safe_file(KB)) ), KBs),
                initAndLoad(KBs,Errors_,Module),
                createSession(Module,UUID)
                )),
        friendlyKBname(Module,KBname),
        print_message(informational,"Module: ~w"-[Module]),
        language(Language), languages(Languages),
        findall(ES, (member(Error,Errors_), term_string(Error,ES)),Errors),
        (Errors=[] -> Result=Result_ ; put_dict(error,Result_,Errors,Result)).



%makeBindingsDict(+NameTermPairs,-NameTermDict)
makeBindingsDict(Pairs,Dict) :-
    makeBindingsDict_(Pairs,NewPairs), dict_create(Dict,_,NewPairs).

makeBindingsDict_([Name=T|Pairs],[Name=J|NewPairs]) :- !,
    term_to_json(T,J), makeBindingsDict_(Pairs,NewPairs).
makeBindingsDict_([],[]).

makeUnknownsArray([at(X,M)/_Clause|U],[_{goal:J, module:M}|NewU]) :- !,
    term_to_json(X,J), makeUnknownsArray(U,NewU).
makeUnknownsArray([],[]).

% keep in sync with reasoner.pl, namely expand_failure_trees and expand_explanation_refs
makeExplanationTree([Node|Nodes],[_{type:Type, literal:Gstring, module:M, source:Source, textOrigin:Origin, children:NewChildren}|NewNodes]) :- !,
    Node=..[Type_,G,_Ref,M,Source,Origin,Children],
    explanation_node_type(Type_,Type),
    term_string(G,Gstring),
    makeExplanationTree(Children,NewChildren),
    makeExplanationTree(Nodes,NewNodes).
makeExplanationTree([],[]).

:- http_handler('/taxkbapi/draft', handle_api_draft, []).  % this defines a web server endpoint for https://github.com/mcalejo/my-highlighter
% receive content from our highlighter Chrome extension, digest it and open a new Prolog file with its "draft":
handle_api_draft(Request) :-
    http_parameters(Request, [pageURL(PageURL,[]),content(Content_,[])]),
    uri_encoded(query_value,Content,Content_),
    open_string(Content,S), json_read_dict(S, ContentArray), close(S),
    load_content(ContentArray),
    draft_string(PageURL,Draft),
    url_simple(PageURL,Filename_), atomic_list_concat([Filename_,".pl"],Filename),
    update_gitty_file(Filename,PageURL,Draft),
    format(string(NewEditor),"/p/~a",[Filename]),
    http_redirect(see_other,NewEditor,Request).    

