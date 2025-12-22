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

:- module(_ThisFileName,[start_api_server/0, set_le_program_module/1, le_program_module/1, hack_module_for_taxlog/1, handle_api/1, makeExplanationTree/2]).

% API for client apps to use the reasoner and drafter

% Adapted from https://github.com/SWI-Prolog/packages-pengines/blob/master/examples/server.pl :
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(sandbox)).
:- use_module(library(http/http_cors)).
:- use_module(library(settings)).
:- set_setting(http:cors, [*]).
%:- use_module(library(http/http_digest)).  % to activate digest authorization options

% :- multifile pengines:authentication_hook/3.

% pengines:authentication_hook(_Request, _, User).

% :- multifile http:authenticate_client/2.

% http:authenticate_client("http://localhost:3050/pengine", Action) :-
%     print_message(informational, " Authenticate client ~w "-[Action]). 

% previous modules

:- multifile sandbox:safe_primitive/1.

:- use_module(library(http/http_json)).
% :- use_module(library(json)).
:- use_module(library(term_to_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).

:- use_module(reasoner).
:- use_module('spacy/spacy.pl').
:- use_module(drafter).
:- use_module(kp_loader).
:- use_module(syntax).
:- use_module(reasoner,[taxlogWrapper/10]).
:- use_module(le_answer, [parse_and_query/5, prepare_query/6, targetBody/6]). 
:- use_module(le_input,[text_to_logic/2, source_lang/1]).
:- use_module(library(prolog_stack)).
%:- use_module('./clientExample/my-app/build/simple_server.pl').  
:- use_module('le_en.pl', [le_en/3]).  % Load Logical English Examples

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

safe_module(M) :- sub_atom(M,0,_,_,leSessionModule), !.

safe_file(F) :- sub_atom(F,_,_,_,'/moreExamples/').

% handler for the original api
%:- http_handler('/taxkbapi', handle_api, []).  % this defines a web server endpoint
:- http_handler('/leapi', handle_api, []).  % this defines a web server endpoint for LE API

handle_api(Request) :-
    cors_enable, 
    % option(method(options), Request), !,
    % cors_enable(Request,
    %               [ methods([get,post,delete]), headers([*])
    %               ]),
    % format('~n'),
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    %asserta(my_request(Request)), % for debugging
    %print_message(informational,"Request Payload: ~w"-[Payload]),
    assertion(Payload.token=='myToken123'),
    (entry_point(Payload,Result)->true;Result=_{error:"No answer"}),
    %print_message(informational,"returning Result: ~w"-[Result]),
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
    %print_message(informational,"entry point query asking: ~w"-[Query]),
    (get_dict(facts,R,Facts_) -> (is_list(Facts_) -> maplist(term_string,Facts,Facts_) ; Facts=Facts_) ; Facts=[]),
    findall( _{bindings:VarPairs, unknowns:U, result:Result, why:E}, (
        query_with_facts(at(Query,R.module),Facts,unknowns(U_),taxlogExplanation(E_),Result),
        makeBindingsDict(VarPairs_,VarPairs),
        makeUnknownsArray(U_,U),
        makeExplanationTree(E_,E)
        ), Results).

% Added for API LE LLM App
% curl --header "Content-Type: application/json" --request POST --data '{"token":"myToken123","operation":"examples", "file":"1_net_asset_value_test_3"}' http://localhost:3050/leapi
entry_point(R, _{results:AnswerWithDocument}) :- 
    get_dict(operation,R,examples), !, 
    %print_message(informational,"LE API: examples request received and extracted: ~w"-[R]),
    Filename = R.file,
    %print_message(informational,"Requested File Example ~w \n"-[R.file]),
    (catch(le_en:en(Filename, _, Document), Error, (
            print_message(error, Error),
            AnswerWithDocument = _{answer:'LLM request failed', details:Error, document:""} )
        ) ->  
        AnswerWithDocument = _{document: Document}
    ;   AnswerWithDocument = _{answer:'Example request failed', details:'Unknown error', document:"Document no available"}). 
    %print_message(informational,"LE API: examples request returning: ~w"-[AnswerWithDocument]).   

% Added for API LE
% curl --header "Content-Type: application/json" --request POST --data '{"token":"myToken123","operation":"answer", "file": "testingle", "document":" ... ", "theQuery":"one", "scenario":"alice"}' http://localhost:3050/leapi
entry_point(R, _{answer:AnswerExplanation}) :- get_dict(operation,R,answer), !, 
   %term_string(Query,R.theQuery,[variable_names(_VarPairs_)]),
   %thread_create(    
        %print_message(informational,"entry point answer asking: ~w"-[Query]),
        le_answer:parse_and_query(R.file, en(R.document), R.theQuery, with(R.scenario), AnswerExplanation).
        %print_message(informational,"entry point query returning: ~w"-[AnswerExplanation]).
    %    ThreadId,
    %    [detached(true)]
    %),
    %thread_join(ThreadId, status(AnswerExplanation)).

% Added for API LE
entry_point(R, _{results:AnswerExplanation}) :- get_dict(operation,R,explain), !, 
    % print_message(informational,"api.pl: Query ~w  Scenario ~w\n"-[R.theQuery, R.scenario]),
    le_answer:parse_and_query_all_answers(R.file, en(R.document), R.theQuery, with(R.scenario), AnswerExplanation). 
    %print_message(informational,"entry point explain returning: ~w"-[AnswerExplanation]).

% NEW: Entry point for Gemini
% curl --header "Content-Type: application/json" --request POST --data '{"token":"myToken123", "gemini_api_key": "..", "operation":"answer_via_llm", "file": "testllm", "document":" ... ", "userinput":"some input text"}' http://localhost:3050/leapi
entry_point(R, _{results:AnswerExplanation, translation: LLMAnswer}) :- get_dict(operation,R,answer_via_llm), !, 
    %print_message(informational,"API: answer_via_llm request received: ~w"-[R]),
    (   catch(process_llm_request(R, Result), Error, (
            print_message(error, Error),
            AnswerExplanation = _{answer:'LLM request failed', details:Error}, LLMAnswer = 'I did not understand'
        ))
    ->  (
        % Successfully processed the LLM request.
        %print_message(informational, "API: LLM request processed successfully: ~w"-[Result]),
        (   Result = _{llm_answer: LLMAnswer, status: 'success'} -> (
            print_message(informational, "API: LLM Answer: ~w"-[LLMAnswer]),
            string_concat("\n   \n", LLMAnswer, LLMAnswer2), 
            string_concat(R.document, LLMAnswer2, NewDocument),
            le_answer:parse_and_query_all_answers(R.file, en(NewDocument), new, with(new), AnswerExplanation)) 
        ;   AnswerExplanation = Result, LLMAnswer = 'I did not understand' )
    )
    ;   AnswerExplanation = _{answer:'LLM request failed', details:'Unknown error'}, LLMAnswer = 'I did not understand'
    ).
    %print_message(informational,"API: answer_via_llm request returning: ~w with LLM answer: ~w"-[AnswerExplanation, LLMAnswer]).
    % AnswerExplanation = _{error:'LLM request passed', details:'LLM integration disabled in this build'}.

% NEW: Helper predicate to process the request, call Gemini, and process the response
process_llm_request(Request, Result)  :- %trace, 
    %Result = _{error:'LLM request passed', details:'LLM integration disabled in this build'}.
    % 1. Adapt inputs from the request
    get_dict(userinput, Request, UserInput),
    %print_message(informational,"API: Got the User Input: ~w"-[UserInput]),
    (get_dict(document, Request, Document) -> DocText = Document; DocText = ''),
    %print_message(informational,"API: Got the Document Text: ~w"-[DocText]),
    %(get_dict(gemini_api_key, Request, APIKey) -> true; throw(error(missing_gemini_api_key, _))),
    (getenv('LE_LLM_K', Value) -> APIKey = Value ; throw(error(missing_gemini_api_key, _))),
    %print_message(informational,"API: Using Gemini API Key: ~w"-[APIKey]),
    % 2. Create a suitable prompt for Gemini
    format(string(Prompt), 'You are an expert on legal and logical reasoning. I will provide a description of a specific situation, possibly containing assumptions and questions: Description: ~w
 
Turn this into a query and scenario pair using these templates shown below.

Name both the new query and the new scenario (say query new, scenario new) and 
return them with this form (no bullet points, please):

scenario new is:

query new is:

(if there is nothing to put on each section, do not write the header)

as illustrated in the examples in the Document bellow:

Document:
~w

', [UserInput, DocText]), %Result = _{passed:'Here', details:Prompt}.
    %print_message(informational,"API: Prompt ready ~w"-[Prompt]),    
    % 3. Call Gemini (via HTTPS)
    %GeminiURL = 'https://generativelanguage.googleapis.com/v1/models/gemini-pro:generateContent?key=',
    %GeminiURL = 'https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=', 
    %GeminiURL = 'https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-05-20:generateContent?key=',
    %GeminiURL = 'https://generativelanguage.googleapis.com/v1beta/models/gemini-3-pro-preview:generateContent?key=',
    % USED_LLM environment variable defines the model to use like     gemini-2.5-flash
    (getenv('USED_LLM', Model) -> LLM = Model ; throw(error(missing_llm, _))),
    GeminiURL = 'https://generativelanguage.googleapis.com/v1beta/models/',
    URLPost = ':generateContent?key=',
    concat_atom([GeminiURL, LLM, URLPost, APIKey], GeminiURLFull), 
    PayloadDict = _{contents: [_{parts: [_{text: Prompt}]}]},
    % print_message(informational,"API: About to send the PayloadDict ~w to ~w"-[PayloadDict, GeminiURLFull]), 
    time(http_post(GeminiURLFull, 
              json(PayloadDict, [json_object(dict)]), 
              ReplyDict, 
              [ cert_verify_hook(cert_accept_any),
                request_header('Content-Type'='application/json')
              ])),
    
    % 4. Use the new predicate to process the response
    gemini_process_response(ReplyDict, Result),
    % 5. Print a message based on the result
    ( Result.status = 'success' ->
        true %print_message(informational, "API: Ready to return ~w\n"-[Result.llm_answer])
    ;
        print_message(error, "API error: ~w"-['Reason:', Result.reason])    ).
% ends process_llm_request/2

% gemini_process_response(ReplyDict, Result) :-
%     % Process the reply from Gemini
%     ( ReplyDict = json(Dict) -> true
%     ; Result = _{status: 'error', reason: 'Invalid JSON reply'}
%     ),
    
%     ( var(Result) ->
%         ( Dict = [candidates=Candidates|_] ->
%             ( Candidates = [CandidatesDict|_] ->
%                 ( CandidatesDict = json([content=Content|_]) ->
%                     ( Content = json([parts=Parts|_]) ->
%                         ( Parts = [json([text=LLMAnswer])|_] ->
%                             % Success: All parts found
%                             Result = _{llm_answer: LLMAnswer, status: 'success'}
%                         ; Result = _{status: 'error', reason: 'Missing text part'}
%                         )
%                     ; Result = _{status: 'error', reason: 'Missing part'}
%                     )
%                 ; Result = _{status: 'error', reason: 'Missing candidates list'}
%                 )
%             ; Result = _{status: 'error', reason: 'Wrong Candidates format'}
%             )
%         ; Result = _{status: 'error', reason: 'Wrong Dict format'}
%         )
%     ), !.

% For a single json/1 term, it unifies Value with the value associated with Key.
% For a list of json/1 terms, it unifies Value with the value from the first
% dictionary in the list.
get_json_value(json(Dict), Key, Value) :-
    !,
    member(Key=Value, Dict).
get_json_value([json(Dict)|_], Key, Value) :-
    !,
    member(Key=Value, Dict).

% Predicate to process the full Gemini API JSON response.
% It takes the raw ReplyDict and returns a structured Result.
gemini_process_response(ReplyDict, Result) :-
    ( get_json_value(ReplyDict, candidates, Candidates) ->
        % Successfully extracted candidates list.
        ( get_json_value(Candidates, content, Content) ->
            % Successfully extracted content.
            ( get_json_value(Content, parts, Parts) ->
                % Successfully extracted parts.
                ( get_json_value(Parts, text, LLMAnswer) ->
                    % Success: All parts found.
                    Result = _{llm_answer: LLMAnswer, status: 'success'}
                ; Result = _{answer: 'error', reason: 'Missing text part'}
                )
            ; Result = _{answer: 'error', reason: 'Missing parts list'}
            )
        ; Result = _{answer: 'error', reason: 'Missing candidate content'}
        )
    ; Result = _{answer: 'error', reason: 'Wrong response format'}
    ).

% Example of how to use it
handle_api_call(FullURL, PayloadDict, Result) :-
    % 3. Call Gemini's API
    time(http_post(FullURL, 
              json(PayloadDict, [json_object(dict)]), 
              ReplyDict, 
              [ cert_verify_hook(cert_accept_any),
                request_header('Content-Type'='application/json')
              ])),
    
    % 4. Use the new predicate to process the response
    gemini_process_response(ReplyDict, Result),
    
    % 5. Print a message based on the result
    ( Result.status = 'success' ->
        print_message(informational, "API: Ready to return ~w\n"-[Result.llm_answer])
    ;
        print_message(error, "API error: ~w\n"-[Result.reason])
    ).


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
entry_point(R, _{prolog:Program, kb:KB, 
    predicates:Predicates, examples:Examples, queries:Qs, target:Target}) :- get_dict(operation,R,le2prolog), !, 
    le2prologTerms(R.le,KB,Terms,Preds,Examples, _ExamplesProlog ,Queries,Target),
    with_output_to(string(Program),forall(member(Term,Terms), portray_clause(Term) ) ),
    with_output_to(string(Qs),forall(member(Qx,Queries), portray_clause(Qx) ) ),
    findall(Pred,(member(Pred_,Preds), term_string(Pred_,Pred)),Predicates).
    %print_message(informational,"le2prolog done\n"-[]).

%TODO: verify if JD initializes parsed etc correctly; they may be prone to threading bugs under the web server thread pool
le2prologTerms(LE,KB,Clauses,Preds,ExamplesInJson, Examples, Queries,Target) :-
    %print_message(informational,"le2prologTerms ~w \n"-[LE]),
    text_to_logic(LE,X), 
    %print_message(informational,"text to logic  ~w \n"-[X]),
    (member(target(prolog),X) -> Target=prolog ; Target=taxlog),
    findall(Prolog, (
        member(T,X), 
        (Target==prolog -> (
            semantics2prolog(T,_,Prolog_),
            ( ( Prolog_=(Head:-RawBody), taxlogWrapper(RawBody,_,_,_,Body,_,_,_,_,_) ) -> 
                Prolog=(Head:-Body) ; 
                Prolog=Prolog_ ) 
            ) ; 
            taxlog2prolog(T,_,Prolog)
            )
        ),Clauses),
    (member(kbname(KB),X)->true;KB=null),
    (member(predicates(Preds),X) -> true; Preds=[]),
    %print_message(informational,"Clauses  ~w \n"-[Clauses]),
    % findall(_{name:QueryName, query: Query}, (
    %     member(query(QueryName, Query_),X), QueryName\==null, term_string(Query_, Query)
    %     ),Queries), 
    findall(query(QueryName, Query), member(query(QueryName, Query),X), Queries), 
    findall(example(ExampleName, Scs), member(example(ExampleName,Scs),X), Examples), 
    %print_message(informational,"Queries  ~w \n"-[Queries]),
    %print_message(informational,"Examples  ~w \n"-[Examples]),
    findall(_{name:Name, scenarios:Scenarios}, (
        member(example(Name,Scenarios_),X), Name\==null,
        findall( _{assertion:Assertion,clauses:ScenarioProgram},(
            member(scenario(Clauses_,Assertion_),Scenarios_),
            term_string(Assertion_,Assertion),
            with_output_to(string(ScenarioProgram), forall(member(Clause_,Clauses_), portray_clause(Clause_)))
            ), Scenarios)
        ),ExamplesInJson).
    %print_message(informational,"Scenarios ~w \n End of le2prologTerms\n"-[Examples]).

entry_point(R, _{sessionModule:M, kb:KB, 
    predicates:Predicates, examples:ExamplesInJSON, 
    queries:QueriesInJSON, language:Lang, target:Target}) :- get_dict(operation,R,load), !, 
    set_le_program_module(M),
    print_message(informational,"Created module ~w\n"-[M]),

    (get_dict(le,R,LE) -> (
            Lang=le,
            le2prologTerms(LE,KB,Clauses,Preds,ExamplesInJSON, Examples, Queries,Target),
            findall(Pred,(member(Pred_,Preds), toJSON(Pred_,Pred)),Predicates),
            findall(QueryJSON,(member(Q_,Queries), toJSON(Q_,QueryJSON)),QueriesInJSON),
            forall(member(Example,Examples),M:assert(Example)), 
            forall(member(Query,Queries),M:assert(Query)), 
            forall(member(Clause,Clauses),M:assert(Clause))
            %print_message(informational,"Asserted  ~w and ~w "-[Queries, Clauses])
        ) ; (
            assertion(safe_file(R.file)),
            (sub_atom(R.file,_,_,0,'le') -> (
                    Lang=le,
                    read_file_to_string(R.file,LE,[]),
                    le2prologTerms(LE,KB,Clauses,Preds,ExamplesInJSON, Examples, Queries, Target),
                    findall(Pred,(member(Pred_,Preds), toJSON(Pred_,Pred)),Predicates),
                    findall(QueryJSON,(member(Q_,Queries), toJSON(Q_,QueryJSON)),QueriesInJSON),
                    forall(member(Example,Examples),M:assert(Example)), 
                    forall(member(Query,Queries),M:assert(Query)), 
                    forall(member(Clause,Clauses),M:assert(Clause))
                    %print_message(informational,"Asserted from file ~w and ~w "-[Queries, Clauses])
                ) ; (
                    Lang=prolog, Target=prolog,
                    load_files(R.file,[module(M)])
                ) 
            )    
        )
    ),
    M:assert(target_executor(Target)),
    (Target==taxlog -> M:assert(myDeclaredModule_(M)) ; true),
    % For LE, make predicates dynamic so we can query them all:
    (nonvar(Preds) -> forall(member(Pred,Preds), (functor(Pred,F,N), M:dynamic(F/N))) ; true),
    %with_output_to(string(Report), (listing(M:_), listing(le_input:_))), 
    print_message(informational,"load finished\n\n"-[]).

hack_module_for_taxlog(M) :-  
    retractall(kp_loader:module_api_hack(_)),
    assert(kp_loader:module_api_hack(M)).

% adding an direct entry point
entry_point(R, _{answer:AnswerJSON, result:ok}) :- get_dict(operation,R,answeringQuery), !, %trace
    print_message(informational,"answering Query: ~w with ~w\n"-[R.query, R.scenario]), 
    assertion(safe_module(R.sessionModule)),
    term_string(Scenario, R.scenario), 
    call_answer(R.query, with(Scenario), R.sessionModule, Answer), !, 
    term_string(Answer, AnswerJSON).  
    % term_string(Requests, R.goal).
    % print_message(informational,"Attending ~w"-[Request]), 
    % % assertion(safe_module(R.sessionModule)) -> assert(parsed),
    % assert(le_input:parsed), 
    % le_input:answer(happy, with(one), Response), retractall(le_input:parsed). 

entry_point(R, _{facts:R.facts, goal: QVS, answers:Solutions, result:Result}) :- get_dict(operation,R,loadFactsAndQuery), !, 
    print_message(informational,"loadFactsAndQuery: ~w\n"-[R]), 
    assertion(safe_module(R.sessionModule)),
    forall(member(Fact_,R.facts),(
        term_string(Fact,Fact_),
        assertion( \+ functor(Fact,':-',_) ),
        R.sessionModule:assert(Fact)
        )),
    (get_dict(goal,R,Goal_) -> (
        assertion(is_list(R.vars)),
        format(string(QVS),"(~a)-(~w)",[Goal_,R.vars]), term_string(Goal-Vars_,QVS),
        (R.sessionModule:target_executor(prolog) -> (
            findall(_{bindings:Vars}, (R.sessionModule:Goal, toJSON(Vars_,Vars)), Solutions),
            (Solutions=[] -> Result=false ; Result=true)
            ) ;(
            % taxlog:
            hack_module_for_taxlog(R.sessionModule),
            findall(Vars+Result+E, (
                query_with_facts(at(Goal,R.sessionModule),[/*??*/],unknowns(_),taxlog(taxlogExplanation(E_)),Result), 
                makeExplanationTree(E_,E),
                toJSON(Vars_,Vars)
                ),Pairs
            ),
            (member(_+unknown+_,Pairs) -> Result=unknown; Pairs=[] -> Result=false; Result=true),
            findall(_{bindings:Vars,explanation:E},member(Vars+_+E,Pairs),Solutions)
            )
        )
    ) ; true).

call_answer(English, Arg, SwishModule, Command) :- %trace, 
    prepare_query(English, Arg, SwishModule,_Goal, Facts, Command), !, 
    print_message(informational, "call_answer: about to call ~w\n"-[Command]), 
    setup_call_catcher_cleanup(le_input:assert_facts(SwishModule, Facts), 
            % with_output_to(string(Out), listing(SwishModule:_)), 
            % catch_with_backtrace(Command, Error, print_message(error, Error)), 
            catch(Command, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            le_input:retract_facts(SwishModule, Facts)). 
    %le_input:translate_goal_into_LE(Goal, RawAnswer), le_input:name_as_atom(RawAnswer, EnglishAnswer). 

toJSON([T1|Tn],[J1|Jn]) :- !, toJSON(T1,J1), toJSON(Tn,Jn).
toJSON([],[]) :- !.
toJSON(T,J) :- atomic(T), !, T=J.
toJSON(D,J) :- is_dict(D), !, 
        dict_pairs(D,Tag,Pairs), 
        findall(Key-ValueJ,(member(Key-Value,Pairs), toJSON(Value,ValueJ)), JPairs),
        dict_pairs(J,Tag,JPairs).
toJSON(T,J) :- term_string(T,J).

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

% sanbox:safe_primitive(le_input:dict(_,_,_)).
% sanbox:safe_primitive(user:current_module(_)). 
% sanbox:safe_primitive(user:dict(_,_,_)).
