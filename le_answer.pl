/* le_answer: a prolog module with predicates to handle queries in Logical English

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Main predicate: answer/1, /2, /3, /4

which can be used on the new command interface of LE on SWISH
(e.g. answer/1 and others querying predicates):

? answer("query one with scenario test"). 

*/

:- module(le_answer, 
    [le_taxlog_translate/4, 
    translate_goal_into_LE/2, 
    translate_goal_into_LE/4, 
    op(1000,xfy,user:and),  % to support querying
    op(800,fx,user:resolve), % to support querying
    op(800,fx,user:answer), % to support querying
    op(800,fx,user:répondre), % to support querying in french
    op(850,xfx,user:with), % to support querying
    op(850,xfx,user:avec), % to support querying in french
    op(800,fx,user:risposta), % to support querying in italian
    op(850,xfx,user:con), % to support querying in italian
    op(800,fx,user:responde), % to support querying in spanish
    %op(1150,fx,user:show), % to support querying
    op(850,xfx,user:of), % to support querying
    dump/4, dump/3, dump/2, dump_scasp/3, split_module_name/3, just_saved_scasp/2, psem/1, set_psem/1,
    prepare_query/6, assert_facts/2, retract_facts/2, parse_and_query/5, parse_and_query_and_explanation/6, parse_and_query_all_answers/5,
    parse_and_query_and_explanation_text/6, le_expanded_terms/2, show/1, source_lang/1, targetBody/6, query_and_explanation_text/4,
    parse_and_load/5, literal_to_sentence/3
    ]).


:- if(exists_source(library(pengines_sandbox))).
    :- use_module(library(pengines_sandbox)). 
:- endif.

% required for sCASP justification (from ~/git/swish/pack/sCASP/examples)

:- if(exists_source(library(scasp))).
    :- use_module(library(scasp)).
    :- use_module(library(scasp/html)).
    :- use_module(library(scasp/output)).
    :- use_module(library(scasp/json)).
:- endif.


:- use_module('le_input.pl').  
:- use_module('syntax.pl').
:- if(\+current_module(wasm)).
    :- use_module('api.pl'). 
:- endif.
:- use_module('reasoner.pl'). 
:- use_module('./tokenize/prolog/tokenize.pl').


% html libs
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- if(exists_source(library(http/js_write))).
    :- use_module(library(http/js_write)).
:- endif.

:- if(exists_source(library(r/r_call))).
    :- use_module(library(r/r_call)).
:- endif.
:- if(exists_source(library(r/r_data))).
    :- use_module(library(r/r_data)).
:- endif.


:- multifile http:location/3.
:- dynamic   http:location/3.

% Does justification tree needs this?

%http:location(scasp, root(scasp), []).
%http:location(js,    scasp(js),   []).
%http:location(css,   scasp(css),  []).
    
:- discontiguous statement/3, declaration/4, _:example/2, _:query/2, _:is_/2. 

:- thread_local  just_saved_scasp/2, abducing/0, psem/1. 

set_psem(File) :-
    retractall(psem(_)), % cleaning id of previously consulted modules  
    assert(psem(File)).  % setting this module for further reasoning


/* ---------------------------------------------------------------  meta predicates CLI */

is_it_illegal(English, Scenario) :- % only event as possibly illegal for the time being
    (le_input:parsed -> true; fail), !, 
    translate_query(English, happens(Goal, T)), % later -->, Kbs),
    %print_message(informational, "Goal Name: ~w"-[GoalName]),predef_
    this_capsule(SwishModule), %SwishModule:query(GoalName, Goal), 
    %extract_goal_command(Question, SwishModule, Goal, Command), 
    %copy_term(Goal, CopyOfGoal), 
    %translate_goal_into_LE(CopyOfGoal, RawGoal),  name_as_atom(RawGoal, EnglishQuestion), 
    %print_message(informational, "Testing illegality: ~w"-[EnglishQuestion]),
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    get_assumptions_from_scenario(Scenario, SwishModule, Assumptions), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Assumptions), 
            %catch(SwishModule:holds(Goal), Error, ( print_message(error, Error), fail ) ), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            catch(SwishModule:it_is_illegal(Goal, T), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Assumptions)), 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer),  
    print_message(informational, "Answers: ~w"-[EnglishAnswer]).

% extract_goal_command/4
% extract_goal_command(WrappedGoal, Module, InnerGoal, RealGoal)
extract_goal_command(Goal, M, InnerGoal, Command) :- nonvar(Goal), !, 
    extract_goal_command_(Goal, M, InnerGoal, Command). 

extract_goal_command_((A;B), M, (IA;IB), (CA;CB)) :-
    extract_goal_command_(A, M, IA, CA), extract_goal_command_(B, M, IB, CB), !. 
extract_goal_command_((A,B), M, (IA,IB), (CA,CB)) :-
    extract_goal_command_(A, M, IA, CA), extract_goal_command_(B, M, IB, CB), !. 
extract_goal_command_(holds(Goal,T), M, Goal, (holds(Goal,T);M:holds(Goal,T))) :- !.
extract_goal_command_(happens(Goal,T), M, Goal, (happens(Goal,T);M:happens(Goal,T))) :- !.
extract_goal_command_(Goal, M, Goal, M:Goal). 

get_assumptions_from_scenario(noscenario, _, []) :- !.  
get_assumptions_from_scenario(Scenario, SwishModule, Assumptions) :-
    SwishModule:example(Scenario, [scenario(Assumptions, _)]), !.

translate_query(English_String, Goals) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    phrase(conditions(0, [], _, Goals), CTokens) -> true 
    ; ( le_input:error_notice(error, Me,Pos, ContextTokens), print_message(error, [Me,Pos,ContextTokens]), fail ). 

/* ----------------------------------------------------------------- Event Calculus  */
% holds/2
holds(Fluent, T) :-
    this_capsule(SwishModule), %trace, 
    SwishModule:happens(Event, T1), 
    rbefore(T1,T),  
    SwishModule:initiates(Event, Fluent, T1), 
    %(nonvar(T) -> rbefore(T1,T); T=(after(T1)-_)),  % T1 is strictly before T 'cos T is not a variable
    %(nonvar(T) -> rbefore(T1,T); true),
    not(interrupted(T1, Fluent, T)).

rbefore(T1, T) :-
    nonvar(T1), nonvar(T), isbefore(T1, T). %, !.
%rbefore(T1, T) :- (var(T1); var(T)), !. % if anyone is a variable, don't compute
%rbefore(T1, (after(T2)-_)) :-
%    nonvar(T1), nonvar(T2), before(T1, T2).

% interrupted/3
interrupted(T1, Fluent, T2) :- %trace, 
    this_capsule(SwishModule),
    SwishModule:happens(Event, T), 
    rbefore(T, T2), 
    SwishModule:terminates(Event, Fluent, T), 
    (rbefore(T1, T); T1=T), !.
    %(nonvar(T2) -> rbefore(T, T2) ; true ), !.  
    %(T2=(after(T1)-_)->T2=(after(T1)-before(T)); rbefore(T,T2)). 

/* ----------------------------------------------------------------- CLI English */
% answer/1
% answer(+Query or Query Expression)
answer(English) :- %trace, 
    answer(English, empty). 

% answer/2
% answer(+Query, with(+Scenario))
answer(English, Arg) :- %trace, 
    le_input:parsed,
    prepare_query(English, Arg, SwishModule, Goal, FactsPre), !, 
    % adding isa_a/2 connections 
    append(FactsPre, [(is_a(A, B):-(is_a(A, C),is_a(C, B)))], Facts), 
    ((SwishModule:just_saved_scasp(FileName, ModuleName), FileName\=null) -> 
        %print_message(informational, "To query file ~w in module ~w "-[FileName, ModuleName]),
        le_input:load_file_module(FileName, ModuleName, true), 
        %print_message(informational, "loaded scasp ~w "-[FileName]),    
        setup_call_catcher_cleanup(assert_facts(ModuleName, Facts), 
            catch(ModuleName:scasp(Goal, [model(_M), tree(_T)]), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
        retract_facts(ModuleName, Facts))
    ;   %print_message(error, "no scasp SwishModule: ~w Facts: ~w Command: ~w Goal: ~w"-[SwishModule, Facts, Command, Goal]),
        setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            %(listing(SwishModule:is_a/2), SwishModule:Goal), 
            call(SwishModule:Goal), 
            %catch_with_backtrace(Command, Error, print_message(error, Error)), 
            %catch((true, SwishModule:Goal), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)) 
    ),  
    %retractall(SwishModule:just_saved_scasp(_, _)), 
    show_answer(Goal). 

% answer/3
% answer(+QuestionOrQueryName, with(+Scenario), -Result)
answer(English, Arg, EnglishAnswer) :- %trace, 
    le_input:parsed, 
    prepare_query(English, Arg, SwishModule, Goal, FactsPre), 
    % adding isa_a/2 connections 
    append(FactsPre, [(is_a(A, B):-(is_a(A, C), is_a(C, B)))], Facts), 
    % this_capsule(SwishModule), 
    % translate_command(SwishModule, English, _, Goal, PreScenario), % later -->, Kbs),
    % %copy_term(Goal, CopyOfGoal), 
    % %translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    % ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    % extract_goal_command(Goal, SwishModule, _InnerGoal, Command),
    % (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), 
    %module(SwishModule), 
    %print_message(informational, "Calling ~w with ~w on ~w "-[Command, Facts, SwishModule]), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch_with_backtrace((SwishModule:Goal->right_answer(Goal,true, EnglishAnswer); right_answer(Goal,false, EnglishAnswer))
            , Error, print_message(error, Error)), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)).
    %print_message(informational, "The Answer is: ~w and the Result ~w"-[Goal, Result]). 
    %reasoner:query_once_with_facts(Goal,Scenario,_,_E,Result).

% right_answer/3     
% right_answer(Goal, Result, Answer).
right_answer(Goal, true, EnglishAnswer) :- 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer).
right_answer(InnerGoal, false, 'No') :-
    ground(InnerGoal), !.
right_answer(_InnerGoal, false, 'None').  

% answer/4
% answer(+English, with(+Scenario), -Explanations, -Output) :-
% answer(at(English, Module), Arg, E, Result) :- %trace,
answer(English, Arg, E, Output) :- %trace, 
    le_input:parsed, %myDeclaredModule(Module), 
    (psem(Module); this_capsule(Module)), 
    translate_command(Module, English, _, Goal, PreScenario), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    extract_goal_command(Goal, Module, InnerGoal, _Command),
    (Scenario==noscenario -> FactsPre = [] ; Module:example(Scenario, [scenario(FactsPre, _)])), !, 
    % adding isa_a/2 connections 
    % append(FactsPre, [(is_a(A, B):-(is_a(A, C),is_a(C, B))), (reasoner:is_a(X,Y) :- is_a(X,Y))], Facts),
    % append(FactsPre, [(is_a_(X,Y):-is_a(X,Y)), (is_a(A, B):-(is_a_(A, C),is_a(C, B)))], Facts),
    append(FactsPre, [(is_a(A, B):-(is_a(A, C),is_a(C, B)))], Facts),
    setup_call_catcher_cleanup(assert_facts(Module, Facts), 
            %catch((listing(SwishModule:is_a/2), reasoner:query(at(InnerGoal, SwishModule),Result,E,_)),             
            %catch((listing(Module:is_a/2), reasoner:query(at(InnerGoal, Module),_U,E,Result)), Error, ( print_message(error, Error), fail ) ),
            catch_with_backtrace(reasoner:query(at(InnerGoal, Module),_U,E,Result), Error, ( print_message(error, Error) ) ),
            _Result, 
            retract_facts(Module, Facts)), 
    (nonvar(Error)->Output=Error; Output=Result).   

% answer_all/3
% answer_all(+English, with(+Scenario), -Explanations) :-
answer_all(English, Arg, Results) :- %trace, !, 
    once( pre_answer(English, Arg, FactsPre, Module, InnerGoal) ),
    % adding isa_a/2 connections 
    % append(FactsPre, [(is_a(A, B):-(is_a(A, C),is_a(C, B))), (reasoner:is_a(X,Y) :- is_a(X,Y))], Facts),
    % append(FactsPre, [(is_a_(X,Y):-is_a(X,Y)), (is_a(A, B):-(is_a_(A, C),is_a(C, B)))], Facts),
    append(FactsPre, [(is_a(A, B):-(is_a(A, C),is_a(C, B)))], Facts),
    %print_message(informational, "Answering: ~w with ~w "-[English, Arg]),
    setup_call_catcher_cleanup( 
            assert_facts(Module, Facts), 
            catch_with_backtrace(
                findall(Answer, 
                    ( %listing(Module:is_a/2),
                      reasoner:query(at(InnerGoal, Module),_U, le(LE_Explanation), Result) ,
                      produce_html_explanation(LE_Explanation, E), correct_answer(InnerGoal, E, Result, Answer_),
                      % stringify to avoid breaking existing clients of this predicate:
                      numbervars(InnerGoal), term_string(InnerGoal,Bindings,[numbervars(true)]),
                      put_dict(bindings, Answer_, Bindings, Answer)                      
                      ),
                    Results), 
                Error, 
                ( print_message(error, Error)) 
                ),
            _Result, 
            retract_facts(Module, Facts)),
    Results \== [],
    !. 

answer_all(English, Arg, [ _{answer:'Failure', explanation:E}])  :-
    %print_message(error, "Failed to answer question: ~w"-[English]),
    with_output_to(string(E), 
        format("Failed to answer question: ~w : ~w", [English, Arg])), !.    

% pre_answer/5
pre_answer(English, Arg, FactsPre, Module, InnerGoal) :- !, 
    le_input:parsed, %myDeclaredModule(Module), 
    (psem(Module); this_capsule(Module)), %trace, 
    %print_message(informational, "Module: ~w "-[Module]), 
    translate_command(Module, English, _, Goal, PreScenario), 
    %print_message(informational, "English: ~w ~w ~w"-[English, Goal, PreScenario]), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    %print_message(informational, "Arg: ~w ~w"-[Arg, Scenario]), 
    extract_goal_command(Goal, Module, InnerGoal, _Command),
    %print_message(informational, "InnerGoal: ~w "-[InnerGoal]), 
    ((Scenario==noscenario;Scenario=='') -> FactsPre = [] ; 
        catch_with_backtrace((Module:example(Scenario, [scenario(FactsPre, _)])), Error, (print_message(error, Error), fail))). 

% correct_answer/5     
% correct_answer(InnerGoal, E, Result, Error, Answer).
correct_answer(_, E, true, _{answer:'Yes', explanation:E}) :- !. 
correct_answer(InnerGoal, E, false, _{answer:'No', explanation:E}) :-
    ground(InnerGoal), !.
correct_answer(_InnerGoal, E, false, _{answer:'None', explanation:E}).  

prepare_scenario(SwishModule, Arg, PreScenario, Scenario, LocalFacts, ModuleFacts, AllFacts) :-
    % extract the name of the scenario
    ((Arg = with(ScenarioName)) -> Scenario=ScenarioName; Scenario=PreScenario),
    % seach for scenarios in the same file and in a different module named the scenario
    (Scenario==noscenario -> AllFacts = [] ; 
        (
            (
                SwishModule:example(Scenario, [scenario(LocalFacts, _)]) -> 
                print_message(informational, "Loading local scenario ~w"-[Scenario]);  
                LocalFacts = [], print_message(informational, "Scenario: ~w not found in current file"-[Scenario])
            ),
            atomic_list_concat([Scenario,'.pl'], ScenarioFileName), 
            % print_message(informational, "ScenarioFileName ~w Scenario ~w"-[ScenarioFileName, Scenario]), 
            % print_message(informational, "LocalFacts ~w "-[LocalFacts]), 
            (
                le_input:load_file_module(ScenarioFileName, Scenario, true) -> 
                print_message(informational, "Loading scenario from module: ~w"-[Scenario]), 
                parse_scenario_from_file(Scenario, ModuleFacts); ModuleFacts=[]
            ),
            % print_message(informational, "ModuleFacts ~w "-[ModuleFacts]),
            append(LocalFacts, ModuleFacts, AllFacts)
        )
    ), !.

parse_scenario_from_file(ScenarioModuleName, Assumptions) :- %trace, 
    ScenarioModuleName:scenario(en(LEString)),
    tokenize(LEString, Tokens, [cased(true), spaces(true), numbers(true)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens),
    % print_message(informational, "CTokens: ~w "-[CTokens]), 
    phrase(le_input:assumptions_(Assumptions), CTokens).

% prepare_query(+QuestionOrQueryName, +Arguments, ?Module, -Goal, -Facts, -Command)
prepare_query(English, Arg, Module, Goal, Facts) :- %trace, 
    %restore_dicts, 
    var(Module), (psem(Module); this_capsule(Module)), % !, 
    %print_message(informational, "Module at prepare query ~w"-[Module]), 
    translate_command(Module, English, GoalName, Goal, PreScenario),
    %enrich_goal(PreGoal, Goal), 
    %print_message(informational, "Module: ~w, English ~w, GoalName ~w, Goal ~w, Scenario ~w"-[Module, English, GoalName, Goal, PreScenario]),
    copy_term(Goal, CopyOfGoal),   
    translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),
    show_question(GoalName, Scenario, EnglishQuestion),  
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    (Scenario==noscenario -> Facts = [] ; 
        (Module:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))), !.
    %print_message(informational, "Facts: ~w"-[Facts]), 
    %extract_goal_command(Goal, Module, _InnerGoal, Command), !.   
    %print_message(informational, "Command: ~w"-[Command]). 

prepare_query(English, Arg, SwishModule, Goal, Facts) :- %trace, 
    %restore_dicts, 
    nonvar(SwishModule),
    %with_output_to(string(Report), listing(dict/3)),
    %print_message(informational, "prepare_query (1): Dictionaries in memory ~w\n"-[Report]),  
    translate_command(SwishModule, English, GoalName, Goal, PreScenario),
    %enrich_goal(PreGoal, Goal), 
    copy_term(Goal, CopyOfGoal),
    %print_message(informational, "prepare_query (2): translated ~w into goalname ~w goal ~w with scenario ~w\n "-[English,GoalName,Goal,PreScenario]), 
    translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion),
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),
    show_question(GoalName, Scenario, EnglishQuestion),  
    %print_message(informational, "prepare_query (3): Scenario: ~w"-[Scenario]), 
    (Scenario==noscenario -> Facts = [] ; 
        (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))), !. 
    %print_message(informational, "prepare_query (4): Facts: ~w Goal: ~w Module: ~w\n "-[Facts, Goal, SwishModule]),  
    %extract_goal_command(Goal, SwishModule, _InnerGoal, Command), !.
    %print_message(informational, "prepare_query (5): Ready from ~w the command ~w\n"-[English, Command]).  

prepare_query(English, _, _, _, _, _) :- 
    print_message(error, "Don't understand this question: ~w "-[English]). %'

show_question(GoalName, Scenario, NLQuestion) :- (psem(M); this_capsule(M)), % current_module(M)),   
    (M:source_lang(en) -> print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),
    (M:source_lang(fr) -> print_message(informational, "La question ~w avec ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),
    (M:source_lang(it) -> print_message(informational, "Domanda ~w con ~w: ~w"-[GoalName, Scenario, NLQuestion]); true), 
    (M:source_lang(es) -> print_message(informational, "La pregunta ~w con ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),  
    (\+(M:source_lang(_)) -> print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),  
    !.  

show_answer(Goal) :- %trace, 
    (psem(M); this_capsule(M)), 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, NLAnswer), 
    (M:source_lang(en) -> print_message(informational, "Answer: ~w"-[NLAnswer]); true), 
    (M:source_lang(fr) -> print_message(informational, "La réponse: ~w"-[NLAnswer]); true), 
    (M:source_lang(it) -> print_message(informational, "Risposta: ~w"-[NLAnswer]); true), 
    (M:source_lang(es) -> print_message(informational, "La respuesta: ~w"-[NLAnswer]); true),
    (\+(M:source_lang(_)) -> print_message(informational, "Answer: ~w"-[NLAnswer]); true),  % english as default
    !. 

% translate_goal_into_LE/2
% translate_goal_into_LE(+Goals_after_being_queried, -Goals_translated_into_LEnglish_as_answers)
translate_goal_into_LE(Goal, LE) :- %trace, 
    translate_goal_into_LE([], Goal, LE, _).

% translate_goal_into_LE/4
% translate_goal_into_LE(Incoming_vars, +Goals_after_being_queried, -Goals_translated_into_LEnglish_as_answers, -Out_going_vars)
% Using the structure (Var, Name-Type) to id each Prolog variable 
translate_goal_into_LE(V0, (G,R), WholeAnswer, V2) :- 
    translate_goal_into_LE(V0, G, Answer, V1), 
    translate_goal_into_LE(V1, R, RestAnswers, V2), !, 
    %print_message(informational, "translate_goal_into_LE: and ~w ~w ~w\n"-[V0, V1, V2]),
    append(Answer, ['\n','\t',and|RestAnswers], WholeAnswer).
translate_goal_into_LE(V0, (G;R), WholeAnswer, V2) :- 
    translate_goal_into_LE(V0, G, Answer,V1), 
    translate_goal_into_LE(V1, R, RestAnswers,V2), !, 
    append(RestAnswers, [')'], PRestAnswers), % using parenthesis
    %print_message(informational, "translate_goal_into_LE: or ~w ~w ~w\n"-[V0, V1, V2]),
    append(['('|Answer], ['\n','\t','\t',or|PRestAnswers], WholeAnswer).
translate_goal_into_LE(V0, aggregate_all(sum(S),Conditions,R), [R,is,the,sum,of,each,number,such,that,'\n', '\t'|Answer], V2) :-
    translate_goal_into_LE([(S,number-number)|V0], Conditions, Answer, V2), !.
    %print_message(informational, "translate_goal_into_LE: aggregate all sum ~w ~w\n"-[V0, V2]).
translate_goal_into_LE(V0, forall(Conds, Goals), ProcessedWordsAnswers, V2) :-
    %print_message(informational, "translate_goal_into_LE: for all ~w ~w\n"-[Conds, Goals]),
    translate_goal_into_LE(V0, Conds, CondsWords, V1), 
    translate_goal_into_LE(V1, Goals, GoalsWords, V2), 
    !,
    %print_message(informational, "translate_goal_into_LE: for all ~w ~w ~w\n"-[V0, V1, V2]),
    append([for, every], CondsWords, FirstPart), 
    append( FirstPart, [it, is, the, case, that,':'|GoalsWords], ProcessedWordsAnswers).  
translate_goal_into_LE(V, setof(Pattern, _Conds, []), [no, solutions, were, found], [(Pattern,key-key)|V]).
translate_goal_into_LE(V, setof(Pattern, _Conds, Solutions), 
    [for, every, value, of, a, key, the, solutions, are, ':'|Solutions], [(Pattern,key-key)|V]).
translate_goal_into_LE(V, findall(Pattern, _Conds, []), [no, solutions, were, found], [(Pattern,key-key)|V]).
translate_goal_into_LE(V, findall(Pattern, _Conds, Solutions), 
    [for, every, value, of, a, key, the, solutions, are, ':'|Solutions], [(Pattern,key-key)|V]).
% translate_goal_into_LE(V, findall(Pattern, Conds, Solutions), ProcessedWordsAnswers, V1) :- % Verify this in the reasoner!
%     print_message(informational, "translate_goal_into_LE: findall all ~w ~w\n"-[Pattern, Conds]),
%     translate_goal_into_LE([(Pattern,key-key)|V], Conds, CondsWords, V1),
%     % translate_goal_into_LE(Solutions, SolutionsWords), 
%     % !,
%     append([give, a, key, the, solutions, are|Solutions], [for|CondsWords], ProcessedWordsAnswers). 
translate_goal_into_LE(V0, not(G), [it,is,not,the,case,that,'\n', '\t'|Answer], V1) :-
    translate_goal_into_LE(V0, G, Answer, V1), !.
    %print_message(informational, "translate_goal_into_LE: not ~w ~w\n"-[V0, V1]).
translate_goal_into_LE(V0, is_a(A,B), ProcessedWordsAnswers, V0) :- % check this one! A, if var, must be in V0
    (starts_with_vowel(B)->ProcessedWordsAnswers=[A, is, an, B]; ProcessedWordsAnswers=[A, is, a, B]), !.
    %print_message(informational, "translate_goal_into_LE: is_a ~w\n"-[V0]).
translate_goal_into_LE(V0, Goal, ProcessedWordsAnswers, V1) :- 
    %print_message(informational, "translated_goal_into_LE: (meta) from  ~w\n"-[Goal]), 
    Goal =.. [Pred|GoalElements], meta_dictionary([Pred|GoalElements], Types, WordsAnswer),
    process_types_or_names(V0, WordsAnswer, GoalElements, Types, ProcessedWordsAnswers, V1), !.
    %print_message(informational, "translated_goal_into_LE: (meta) from  ~w to ~w "-[V0, V1]).
translate_goal_into_LE(V0, Goal, ProcessedWordsAnswers, V1) :- 
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), %trace, 
    process_types_or_names(V0, WordsAnswer, GoalElements, Types, ProcessedWordsAnswers, V1), !.
    %print_message(informational, "translated_goal_into_LE: from dict  V0:~w to  V1:~w "-[V0, V1]).
translate_goal_into_LE(V0, happens(Goal,T), Answer, V1) :-    % simple goals do not return a list, just a literal
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(V0, WordsAnswer, GoalElements, Types, ProcessedWordsAnswers, V1), 
    process_time_term(T, TimeExplain), !, 
    Answer = ['At', TimeExplain, it, occurs, that|ProcessedWordsAnswers].
translate_goal_into_LE(V0, holds(Goal,T), Answer,V1) :- 
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(V0, WordsAnswer, GoalElements, Types, ProcessedWordsAnswers,V1), 
    process_time_term(T, TimeExplain),
    Answer = ['At', TimeExplain, it, holds, that|ProcessedWordsAnswers], !. 

starts_with_vowel(Term) :-
    atom(Term),
    atom_chars(Term, [FirstChar|Rest]), 
    ( member(FirstChar, ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'])
    ; (Rest=[SecondChar|_], 
       member(FirstChar, ['h', 'H']), 
       member(SecondChar, ['o', 'e', 'E', 'O']))),!.
    
starts_with_vowel(Term) :- % not needed fttb.
    compound(Term),
    Term =.. [Functor| _],
    atom(Functor),
    atom_chars(Functor, [FirstChar|Rest]), 
    ( member(FirstChar, ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'])
    ; (Rest=[SecondChar|_], 
       member(FirstChar, ['h', 'H']), 
       member(SecondChar, ['o', 'e', 'E', 'O']))).

starts_with_vowel(Term) :-
    atom(Term), Term='LLM'. 

process_time_term(T,ExplainT) :- var(T), name_as_atom([a, time, T], ExplainT). % in case of vars
process_time_term(T,T) :- nonvar(T), atom(T), !. 
process_time_term(T,Time) :- nonvar(T), number(T), T>100, unparse_time(T, Time), !.  
process_time_term(T,Time) :- nonvar(T), number(T), T=<100, T=Time, !.  % hack to avoid standard time transformation
process_time_term((after(T)-Var), Explain) :- var(Var), !,
    process_time_term(T, Time), 
    name_as_atom([any, time, after, Time], Explain).
process_time_term((after(T1)-before(T2)), Explain) :- !,
    process_time_term(T1, Time1), process_time_term(T2, Time2),
    name_as_atom([any, time, after, Time1, and, before, Time2], Explain).

%process_template_for_scasp/4
%process_template_for_scasp(WordsAnswer, GoalElements, Types, +FormatElements, +ProcessedWordsAnswers)
process_template_for_scasp([], _, _, [], []) :- !.
process_template_for_scasp([Word|RestWords], Elements, Types, [' @(~p:~w) '|RestFormat], [Word, TypeName|RestPrintWords]) :- 
    var(Word), matches_type(Word, Elements, Types, Type), 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords),
    tokenize_atom(Type, NameWords), delete_underscore(NameWords, [TypeName]), escape_uppercased(TypeName, _), !.
process_template_for_scasp([Word|RestWords], Elements, Types, [' @(~p:~p) '|RestFormat], [Word, TypeName|RestPrintWords]) :- 
    var(Word), matches_type(Word, Elements, Types, Type), !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords),
    tokenize_atom(Type, NameWords), delete_underscore(NameWords, [TypeName]).
process_template_for_scasp([Word|RestWords],  Elements, Types, RestFormat, RestPrintWords ) :- % skipping apostrofes by now
    nonvar(Word), Word = '\'', !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, ['~p'|RestFormat], [Word|RestPrintWords] ) :-
    le_input:op_stop(List), member(Word,List), !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, [' ~w '|RestFormat], [Word|RestPrintWords] ) :-
    escape_uppercased(Word, _), !, 
    %name(Word, List), 
    %print_message(informational, "processing word ~p ~q"-[Word, List]), 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, [' ~p '|RestFormat], [Word|RestPrintWords] ) :-
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).

escape_uppercased(Word, EscapedWord) :-
    name(Word, [First|Rest]), First >= 65, First =< 90,
    append([92, First|Rest], [92], NewCodes),
    name(EscapedWord, NewCodes).

assert_facts(M,F) :- 
    % print_message(informational,"~q"-[assert_facts(M,F)]),
    assert_facts_(M,F).

assert_facts_(_, []) :- !. 
assert_facts_(SwishModule, [F|R]) :- must_be(nonvar,F),  %print_message(informational, "asserting: ~w"-[SwishModule:F]),
    assertz(SwishModule:F), assert_facts_(SwishModule, R).

retract_facts(M,F) :- 
    % print_message(informational,"~q"-[retract_facts(M,F)]),
    retract_facts_(M,F).

retract_facts_(_, []) :- !. 
retract_facts_(SwishModule, [F|R]) :-  % print_message(informational, "retracting: ~w"-[SwishModule:F]),
    (nonvar(F) -> 
        (F=(Fact:-_) -> true ; F=Fact),
        retractall(SwishModule:Fact) 
        ; 
        true), 
    retract_facts_(SwishModule, R). 

% translate_command/5
translate_command(Module, English_String, GoalName, Goals, Scenario) :- %trace, 
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens),
    phrase(command_(GoalName, Scenario), CTokens),  
    %print_message(informational, "GoalName ~w Module ~w"-[GoalName, Module]), 
    %Module:query(GoalName, Goals). 
    catch_with_backtrace((Module:query(GoalName, Goals)), Error, (print_message(error, "No such query defined: ~w"-[Error]), fail)),
    !.
    %( Module:query(GoalName, Goals) -> true; (print_message(informational, "No goal named: ~w"-[GoalName]), fail) ), !. 

translate_command(_, English_String, GoalName, Goals, Scenario) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), Scenario=noscenario, GoalName=unamed, 
    (phrase(conditions(0, [], _, Goals), CTokens) ->  true  ;
        ( once(le_input:error_notice(error, Me,_, ContextTokens)), print_message(informational, "Error in query definition ~w ~w"-[Me,ContextTokens]), CTokens=[], fail )
    ). 

command_(Goal, Scenario) --> 
    %order_, goal_(Goal), with_, scenario_name_(Scenario). 
    goal_(Goal), with_, scenario_name_(Scenario).
command_(Goal, noscenario) --> 
    goal_(Goal).

%order_ --> [answer], spaces(_).
%order_ --> [run], spaces(_).
%order_ --> [solve], spaces(_).
%order_ --> [resolve], spaces(_).

goal_(Goal) --> query_or_empty, extract_constant([with], GoalWords), spaces(_), 
    {name_as_atom(GoalWords, Goal)}. % goal by name

query_or_empty --> query_.
query_or_empty --> []. 

with_ --> [with], spaces(_).

scenario_name_(Scenario) -->  scenario_or_empty_, extract_constant([], ScenarioWords), spaces(_), 
{name_as_atom(ScenarioWords, Scenario)}. % Scenario by name

scenario_or_empty_ --> [scenario], spaces(_). 
scenario_or_empty_ --> spaces(_). 

check_clause(Head, Body) :- not(predicate_property(Head, built_in)), !, clause(Head, Body). 

remove_duplicates([], []) :- !. 
remove_duplicates([H|R], RR) :- member(H, R), remove_duplicates(R, RR), !.
remove_duplicates([H|R], [H|RR]) :- remove_duplicates(R, RR). 

% show/1
show(prolog) :-
    %print_message(informational, "%This is prolog code"), 
    show(metarules), 
    show(rules),
    show(queries),
    show(scenarios). 

show(rules) :- %trace, 
    (psem(Module); this_capsule(Module)), 
    findall(Pr, le_input:filtered_dictionary(Pr), Preds), 
    remove_duplicates(Preds, PredsClean), 
    %print_message(informational, "Module ~w"-[Module]), 
    findall((Pred :- Body), 
        ( member(Pred, PredsClean), clause(Module:Pred, Body_), unwrapBody(Body_, Body)), Predicates),
    %print_message(informational, "Rules  ~w"-[Predicates]),
    %forall(member(Clause, [(is_(A,B) :- (nonvar(B), is(A,B)))|Predicates]), portray_clause_ind(Clause)).
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

% 
%(op2tokens(Pred, _, OpTokens) -> % Fixing binary predicates for scasp
%( append([X|_], [Y], GoalElements),
%  append([X|OpTokens],[Y], RevGoalElements), 
%  print_message(informational, "binary op ~w"-[Pred]) ) 
%; RevGoalElements = GoalElements 
%), 

show(metarules) :- %trace,  
    (psem(Module); this_capsule(Module)), 
    findall((Pred :- Body), 
        (meta_dict(PredicateElements, _, _), PredicateElements\=[], 
         Pred=..PredicateElements, clause(Module:Pred, Body_), unwrapBody(Body_, Body)), Predicates),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(queries) :- %trace, 
    (psem(Module); this_capsule(Module)),
    findall((query(A,B) :- true), 
        (clause(Module:query(A,B), _)), Predicates),
    %print_message(informational, "Queries  ~w"-[Predicates]),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(scenarios) :- %trace,
    (psem(Module); this_capsule(Module)),
    findall((example(A,B) :- true),
        (clause(Module:example(A,B), _)), Predicates),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(templates) :-
    findall(EnglishAnswer, 
        ( ( meta_dictionary([_|GoalElements], Types, WordsAnswer) ; 
            dictionary([_|GoalElements], Types, WordsAnswer)),
        process_types_or_names([], WordsAnswer, GoalElements, Types, ProcessedWordsAnswers, _),
        name_as_atom(ProcessedWordsAnswers, EnglishAnswer)), Templates), 
    forall(member(T, Templates), print_message(informational, "~w"-[T])). 

show(templates_scasp) :-
    findall(Term, 
        ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
            dict([Pred|GoalElements], Types, WordsAnswer)),
        Goal =.. [Pred|GoalElements],
        process_template_for_scasp(WordsAnswer, GoalElements, Types, FormatEl, LE),
        atomic_list_concat(['#pred ~w ::\''|FormatEl], Format),
        Elements = [Goal|LE],
        numbervars(Elements, 1, _),
        format(atom(Term), Format, Elements)), Templates),
    forall(member(T, Templates), (atom_string(T, R), print_message(informational, "~w\'."-[R]))). % '

show(types) :-
    %findall(EnglishAnswer, 
    %    ( dictionary([_|GoalElements], Types, _), 
    %      member((Name-Type), Types), 
    %    process_types_or_names([Type], GoalElements, Types, ProcessedWordsAnswers, _),
    %    name_as_atom(ProcessedWordsAnswers, EnglishAnswer)), Templates), 
    print_message(information, "Pre-defined Types:"-[]),
    setof(Tpy, pre_is_type(Tpy), PreSet), 
    forall(member(Tp, PreSet),print_message(informational, '~a'-[Tp])), 
    print_message(informational, "Types defined in the current document:"-[]), 
    setof(Ty, is_type(Ty), Set), 
    forall(member(T, Set), print_message(informational, '~a'-[T])). 

show(scasp) :-
    show(templates_scasp), 
    show(metarules), 
    show(rules). 

show(scasp, with(Q, S)) :-
    show(scasp), 
    this_capsule(SwishModule), 
    clause(SwishModule:query(Q,Query), _),
    clause(SwishModule:example(S, [scenario(Scenario, _)]), _),
    %print_message(informational, "% scenario ~w ."-[List]),
    forall(member(Clause, Scenario), portray_clause_ind(Clause)),
    print_message(informational, "/** <examples>\n?- ? ~w .\n **/ "-[Query]).

show(scasp, with(Q)) :-
    show(scasp), 
    this_capsule(SwishModule), 
    clause(SwishModule:query(Q,Query), _),
    print_message(informational, "/** <examples>\n?- ? ~w .\n **/ "-[Query]).

unwrapBody(_:targetBody(Body, _, _, _, _, _), Body). 

% hack to bring in the reasoner for explanations.  
targetBody(G, false, _, '', [], _) :-
    ((psem(Module), !); this_capsule(Module)), extract_goal_command(G, Module, _InnerG, Command), 
    %print_message(informational, "targetBody Reducing ~w to ~w in ~w"-[G,Command, Module]),
    %call(Command).
    catch(Command,Caught,print_message(error, "Caught: ~w:~q~n"-[Module, Caught])). 

collect_current_dicts(PredicatesDict,PredicatesMeta) :-
    findall(local_dict(Prolog, NamesTypes, Templates), 
        le_input:dict(Prolog, NamesTypes, Templates), 
        %TODO: Ideally we should use the following, but it hangs after loading the first test in the suite (cgt_assets.le):
        % le_input:dictionary(Prolog, NamesTypes, Templates), 
        PredicatesDict),
    findall(local_meta_dict(Prolog, NamesTypes, Templates), 
        le_input:meta_dictionary(Prolog, NamesTypes, Templates), 
        PredicatesMeta).

dump(templates, String) :-
    collect_current_dicts(PredicatesDict,PredicatesMeta), 
    with_output_to(string(String01), forall(member(Clause1, PredicatesDict), portray_clause_ind(Clause1))),
    (PredicatesDict==[] -> string_concat("local_dict([],[],[]).\n", String01, String1); String1 = String01), 
    with_output_to(string(String02), forall(member(Clause2, PredicatesMeta), portray_clause_ind(Clause2))),
    (PredicatesMeta==[] -> string_concat("local_meta_dict([],[],[]).\n", String02, String2); String2 = String02), 
    string_concat(String1, String2, String). 

dump(templates_scasp, String) :-
    findall(Pred/N, ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
                   dict([Pred|GoalElements], _, _) ),
                   length(GoalElements, N) ), 
        Functors),
    (Functors\=[] -> 
        write_functors_to_string(Functors, "", StringFunctors), 
        string_concat(":- dynamic ", StringFunctors, String0 ),
        string_concat(String0, ".\n", String1)
    ;   String1 = ""
    ), 
    findall(Term, 
        ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
            dict([Pred|GoalElements], Types, WordsAnswer)),
        Goal =.. [Pred|GoalElements],
        process_template_for_scasp(WordsAnswer, GoalElements, Types, FormatEl, LE),
        atomic_list_concat(['#pred ~p :: \''|FormatEl], Format),
        Elements = [Goal|LE], 
        numbervars(Elements, 1, _), 
        format(atom(Term), Format, Elements) ), Templates),
    with_output_to(string(String2), forall(member(T, Templates), (atom_string(T, R),write(R),write("\'.\n")))), %'
    string_concat(String1, String2, String). 

dump(source_lang, String) :-
    le_input:source_lang(L) -> 
    with_output_to(string(String), portray_clause_ind(source_lang(L))) ; String="". 

dump(source_lang_scasp, String) :-
    le_input:source_lang(L) -> 
    with_output_to(string(String), portray_clause_ind(:- set_prolog_flag(scasp_lang, L))) ; String="".     

% #abducible
dump(abducibles_scasp, List, String) :-
    findall(Term, ( member( abducible(Abducible, _), List), Abducible\=true, format(string(Term), "#abducible ~p", [Abducible]) ), Abds), 
    with_output_to(string(String), forall(member(S, Abds), (term_string(T, S), portray_clause_ind(T)))).


dump(scasp_scenarios_queries, List, String) :- 
    findall( example(Name, Scenario), 
        (member( example(Name, Scenario), List)), Scenarios),
    %print_message(informational, "Scenarios ~w"-[Scenarios]),
    % example(one, [scenario(
    %                       [(the_service_is_delivered_before(1654423200.0):-true),  
    %                       (the_service_recipient_maintains_all_communication_within_the_confines_of(domain):-true),  
    %                       (the_service_recipient_delivers_requested_information_before(1654077600.0):-true),  
    %                       (is_signed_by_the_service_provider(the_contract):-true),  
    %                       (is_also_signed_by_the_service_recipient(the_contract):-true)
    %                       ], true)]).
    with_output_to(string(StringScenarios),
        ( forall(member(example(S, [scenario(Scenario, _)]), Scenarios),
                ( write("/* Scenario "), write(S), write("\n"), % simple comment not for PlDoc
                  forall((member(Clause, Scenario),Clause\=(abducible(_,_) :- _)), portray_clause_ind(Clause)),
                  forall((member(Clause, Scenario),Clause=(abducible(Abd,_) :- _)), 
                        (format(string(String), "#abducible ~p", [Abd]), term_string(Term, String), portray_clause_ind(Term))),
                  write("% */ \n")
                )
            )
        )
    ),
    with_output_to(string(String00), write("/** <examples>\n")), 
    findall( Query, (member( query(_, Query), List), Query\=true), Queries),
    with_output_to(string(String01), forall(member(Q, Queries), ( write("?- ? "), writeq(Q), write(".\n") ))),
    with_output_to(string(String0N), write("**/")),
    string_concat(String00, String01, String02),
    string_concat(String02, String0N, StringQueries),
    string_concat(StringScenarios, StringQueries, String). 

dump(constraints, List, String) :- %trace, 
    findall((false :- Body), 
        (member( (:- Body_), List), unwrapBody(Body_, Body)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).
    %print_message(informational, " Constraints ~w"-[String]).

dump(rules, List, String) :- %trace, 
    findall((Pred :- Body), 
        (member( (Pred :- Body_), List), unwrapBody(Body_, Body)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).
    %print_message(informational, " Rules ~w"-[List]).

dump(queries, List, String) :- 
    findall( query(Name, Query), 
        (member( query(Name, Query), List)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).

dump(scenarios, List, String) :- 
    findall( example(Name, Scenario), 
        (member( example(Name, Scenario), List)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).

dump(all, Module, List, String) :-
    %print_message(informational, " To dump all"),
	dump(templates, StringTemplates), 
    %print_message(informational, " Templates ~w"-[StringTemplates]),
	dump(rules, List, StringRules),
    dump(scenarios, List, StringScenarios),
    dump(queries, List, StringQueries), 
    string_concat(":-module(\'", Module, Module01),  %'
    string_concat(Module01, "\', []).\n", TopHeadString),   %'
    dump(source_lang, SourceLang), 
    string_concat(TopHeadString, SourceLang, TopMost), 
	string_concat(TopMost, StringTemplates, HeadString),  
    string_concat(HeadString, "prolog_le(verified).\n", String0), % it has to be here to set the context
	string_concat(String0, StringRules, String1),
    string_concat(String1, StringScenarios, String2),
    string_concat(String2, StringQueries, String).   

dump_scasp(Module, List, String) :-
	dump(templates_scasp, StringTemplates), 
    dump(constraints, List, StringConstraints), 
	dump(rules, List, StringRules),
    dump(scasp_scenarios_queries, List, StringQueriesScenarios),
    dump(abducibles_scasp, List, StringAbds),  
    string_concat(":-module(\'", Module, Module01),  %'
    string_concat(Module01, "\', []).\n", TopHeadString),  %'
    dump(source_lang_scasp, SourceLang), 
    string_concat(TopHeadString, SourceLang, TopMost), 
    % headers for scasp
    string_concat("% s(CASP) Programming \n:- use_module(library(scasp)).\n% Uncomment to suppress warnings\n:- style_check(-discontiguous).\n",
                ":- style_check(-singleton).\n:- set_prolog_flag(scasp_forall, prev).\n", SCAPSHeader),
    string_concat(TopMost, SCAPSHeader, Header), 
	string_concat(Header, StringTemplates, HeadString), 
    string_concat(HeadString, StringAbds, String0), 
    %string_concat(String1, "prolog_le(verified).\n", String2), % not need for scasp
    string_concat(String0, StringConstraints, String1), 
	string_concat(String1, StringRules, String2),
    string_concat(String2, StringQueriesScenarios, String). 

restore_dicts :- 
    this_capsule(SwishModule), 
    restore_dicts_from_module(SwishModule). 

restore_dicts_from_module(Module) :-
    restore_dicts(Module,DictEntries), 
    order_templates(DictEntries, OrderedEntries), 
    process_types_dict(OrderedEntries, Types), 
    append(OrderedEntries, Types, MRules), 
    assertall(MRules), !. % asserting contextual information

restore_dicts(Module,DictEntries) :- 
    findall(dict(A,B,C), catch(Module:local_dict(A,B,C),_,fail), ListDict),
    findall(meta_dict(A,B,C), catch(Module:local_meta_dict(A,B,C),_,fail), ListMetaDict),
    %(local_dict(_,_,_) -> findall(dict(A,B,C), local_dict(A,B,C), ListDict) ; ListDict = []),
    %(local_meta_dict(_,_,_) -> findall(meta_dict(A,B,C), local_meta_dict(A,B,C), ListMetaDict); ListMetaDict = []),
    append(ListDict, ListMetaDict, DictEntries), 
    %print_message(informational, "the dictionaries being restored are ~w"-[DictEntries]),
    collect_all_preds(Module, DictEntries, Preds),
    %print_message(informational, "the dictionaries being set dynamics are ~w"-[Preds]),
    declare_preds_as_dynamic(Module, Preds). 

% collect_all_preds/3
collect_all_preds(_, DictEntries, ListPreds) :-
    findall(AA, ((member(dict(A,_,_), DictEntries); member(meta_dict(A,_,_), DictEntries)), A\=[], AA =.. A, not(predicate_property(AA,built_in))), ListPreds). 

declare_preds_as_dynamic(_, []) :- !. 
declare_preds_as_dynamic(M, [F|R]) :- functor(F, P, A),  % facts are the templates now
        catch( dynamic([M:P/A], [thread(local), discontiguous(true)]), _,true), 
        %HACK: we may be repeating this onto existing predicates, so we just skip them
        declare_preds_as_dynamic(M, R). 

%split_module_name(user, temporal, '') :- !. 

split_module_name(Name, Name, '') :-
   \+ sub_atom(Name, _, _, _, '+'),
   \+ sub_atom(Name, _, _, _, 'http'), !. 

split_module_name(Name, File, URL):-
	sub_atom(Name,U,1,_,'+'),
	sub_atom(Name,0,U,_,File),
	UU is U+1, 
	sub_atom(Name,UU,_,0,URL), 
	!. 
	%print_message(informational, URL). 

split_module_name(Name, Name, Name) :- % dangerous. But it maybe needed for earlier taxlog examples. 
	sub_atom(Name, _, _, _, 'http'), !. 

write_functors_to_string([F/N], Previous, StringFunctors) :- !, 
    with_output_to(string(StringF), format("~p/~d", [F,N])),
    string_concat(Previous, StringF, StringFunctors). 
write_functors_to_string([F/N|R], Previous, StringFunctors) :- !, 
    write_functors_to_string(R, Previous, NextString), 
    with_output_to(string(StringF), format("~p/~d, ", [F,N])),
    string_concat(StringF, NextString, StringFunctors). 

%%% ------------------------------------------------ Swish Interface to logical english
%% based on logicalcontracts' lc_server.pl

:- multifile prolog_colour:term_colours/2.
prolog_colour:term_colours(en(_Text),lps_delimiter-[classify]). % let 'en' stand out with other taxlog keywords
prolog_colour:term_colours(en_decl(_Text),lps_delimiter-[classify]). % let 'en_decl' stand out with other taxlog keywords

user:(answer Query with Scenario):- 
    %print_message(informational,"le_answer:answer ~w with ~w"-[Query, Scenario]), 
    answer(Query,with(Scenario)). 
user: (répondre Query avec Scenario):-
    answer(Query,with(Scenario)).
user: (risposta Query con Scenario):-
    answer(Query,with(Scenario)). 
user: (responde Query con Scenario):-
    answer(Query,with(Scenario)). 
%:- discontiguous (with)/2.
%user:(Query with Scenario):-  
%    answer(Query,with(Scenario)). 
%user:(Command1 and Command2) :-
%    call(Command1), call(Command2). 
user:answer( EnText) :- answer( EnText).
user:answer( EnText, Scenario) :- answer( EnText, Scenario).
user:answer( EnText, Scenario, Result) :- answer( EnText, Scenario, Result).
user:answer( EnText, Scenario, E, Result) :- answer( EnText, Scenario, E, Result).

%user:(show Something) :- 
%    le_answer:show(Something). 

user:(show(Something, With) ):- 
    le_answer:show(Something, With). 

user:is_it_illegal( EnText, Scenario) :- is_it_illegal( EnText, Scenario).

%user:query(Name, Goal) :- query(Name, Goal).

user:holds(Fluent, Time) :- holds(Fluent, Time). 

%user:has_as_head_before(List, Head, Rest) :- has_as_head_before(List, Head, Rest). 

% for term_expansion
%user:le_taxlog_translate( en(Text), Terms) :- le_taxlog_translate( en(Text), Terms)..
%user:le_taxlog_translate( en(Text), File, Base, Terms) :- le_taxlog_translate( en(Text),  File, Base, Terms).

user:op_stop(StopWords) :- le_input:op_stop(StopWords). 

%user:targetBody(G, B, X, S, L, R) :- targetBody(G, B, X, S, L, R). 

user:restore_dicts :- restore_dicts.

% user:source_lang(L) :- source_lang(L). 

le_taxlog_translate( EnText, Terms) :- le_taxlog_translate( EnText, someFile, 1, Terms).

% Baseline is the line number of the start of Logical English text
le_taxlog_translate( LEterm, File, BaseLine, Terms) :-
    LEterm =.. [Lang,Text],
    assertion( memberchk(Lang,[en,fr,it,es]) ),
    clear_errors,
    (le_input:text_to_logic(Text, Terms) -> clear_errors; showErrors(File,BaseLine)).
le_taxlog_translate( prolog_le(verified), _, _, prolog_le(verified)) :- %trace, % running from prolog file
    assertz(le_input:parsed), %this_capsule(M),  
    %assertz(M:just_saved_scasp(null, null)), 
    including -> true; restore_dicts. 

combine_list_into_string(List, String) :-
    combine_list_into_string(List, "", String).

combine_list_into_string([], String, String).
combine_list_into_string([HS|RestS], Previous, OutS) :-
    string_concat(Previous, HS, NewS),
    combine_list_into_string(RestS, NewS, OutS).

%user:showtaxlog :- showtaxlog.
%user:is_type(T) :- is_type(T).
%user:dict(A,B,C) :- dict(A,B,C).
%user:meta_dict(A,B,C) :- meta_dict(A,B,C). 

showtaxlog:-
    % ?????????????????????????????????????????
	% psyntax:lps_swish_clause(en(Text),Body,_Vars),
	once(text_to_logic(_,Taxlog)),
    showErrors(someFile,0), 
	writeln(Taxlog),
	fail.
showtaxlog.

% le_expanded_terms/2 is being used for term expansion in user_module_for_swish
le_expanded_terms(TaxlogTerms, ExpandedTerms) :-
    %print_message(informational, " Translated ~w"-[TaxlogTerms]), 
	(TaxlogTerms\=[]-> 
        findall(PrologTerm, (
        member(TT_,TaxlogTerms), 
        (is_list(TT_)->member(TT,TT_);TT=TT_), % the LE translator generates a list of lists... and non lists
        ((member(target(prolog),TaxlogTerms);member(target(scasp),TaxlogTerms)) -> 
            semantics2prolog(TT,_,PrologTerm);taxlog2prolog(TT,_,PrologTerm))
        ), ExpandedTerms_0) 
    ; ExpandedTerms_0 = []),
    %print_message(informational, " First Expansion ~w"-[ExpandedTerms_0]), 
    % to save as a separated file
    (member(target(prolog),TaxlogTerms) -> 
        ( myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-prolog','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-prolog', '+', URL], NewModule); atomic_list_concat([FileName,'-prolog'], NewModule)),
        %print_message(informational, " Processing module ~w filename ~w URL ~w"-[Name, FileName, URL]),  
        dump(all, NewModule, ExpandedTerms_0, String), 
        %print_message(informational, " To dump this ~w"-[String]),
        update_file(NewFileName, URL, String),
        ExpandedTerms_1 = [just_saved_scasp(null, null)|ExpandedTerms_0]) ; ExpandedTerms_1 = ExpandedTerms_0),
    %print_message(informational, " Terms ~w"-[ExpandedTerms_1]), 
    (member(target(scasp),TaxlogTerms) -> 
        ( myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-scasp','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-scasp', '+', URL], NewModule); atomic_list_concat([FileName,'-scasp'], NewModule)), 
        %print_message(informational, "sCASP module name ~w"-[NewModule]), 
        dump_scasp(NewModule, ExpandedTerms_0, String), 
        %print_message(informational, "sCASP content to assert: ~w \n"-[String]), 
        update_file(NewFileName, NewModule, String),
        ExpandedTerms_2 = [just_saved_scasp(NewFileName, NewModule)|ExpandedTerms_1] ) ; ExpandedTerms_2 = ExpandedTerms_1),
        ExpandedTerms = ExpandedTerms_2. 

:- multifile kp_loader:myDeclaredModule_/1. 

% asserting/2
asserting(File, ExpandedTerms) :-
    %print_message(error, "Asserting on ~w this ~w"-[M, ExpandedTerms]), 
    forall(member(T, [(:-module(File,[])), source_lang(en)|ExpandedTerms]), (
        %print_message(informational, "Asserting File:T ~w:~w"-[File,T]), 
            T = (A=B:-_) -> 
                (var(A) -> Term=B ; Term=A),
                assert_semantic_error(error,"Missing template for '~w'"-[Term],'rule head') 
            ; 
            assertz(File:T)
    )). % simulating term expansion

%retracting/2
retracting(File, ExpandedTerms) :- 
    %print_message(error, "Cleaning ~w of ~w"-[M, ExpandedTerms]), 
    % cleaning memory
    forall(member(T, [(:-module(File,[])), source_lang(en)|ExpandedTerms]), 
        ( %print_message(informational, "Removing File:T ~w:~w"-[File,T]), 
         retractall(File:T))).

parse_and_load(File, Document,TaxlogTerms,ExpandedTerms,NewFileName) :-
    %print_message(informational, "parse_and_query ~w ~w ~w ~w"-[File, Document, Question, Scenario]),
	%prolog_load_context(source,File), % atom_prefix(File,'pengine://'), % process only SWISH windows
	%prolog_load_context(term_position,TP), stream_position_data(line_count,TP,Line),
    clear_dicts,
    clear_semantic_errors,
    le_taxlog_translate(Document, File, 1, TaxlogTerms),
    set_psem(File),
	non_expanded_terms(File, TaxlogTerms, ExpandedTerms,NewFileName_),
    absolute_file_name(NewFileName_, NewFileName),
    asserting(File, ExpandedTerms),
    % now save dicts ointo the local dict and meta relations in the module:
    collect_current_dicts(PredicatesDict,PredicatesMeta),
    append(PredicatesDict,PredicatesMeta,Dicts),
    asserting(File, Dicts),
    assert_missing_templates_in_bodies(File),
    xref_source(NewFileName), assert(module_xref_source(File,NewFileName)),
    undefined_le_predicates(UndefinedTemplateStrings),
    forall(member(TS,UndefinedTemplateStrings),assert_semantic_error(warning,"Undefined predicate ~q"-[TS],"rule body")),
    unqueried_predicates(UnqueriedTemplateStrings),
    forall(member(TS,UnqueriedTemplateStrings),assert_semantic_error(warning,"This predicate is not tested by any query: ~q"-[TS],"queries")),
    % base syntax errors are shown by showerrors called from le_taxlog_translate:
    show_semantic_errors.

% HACK: discover atoms that are LIKELY to be missing templates, as they map to bad =/2 subgoals
% TODO: there should be a much better way to obtain these more precisely near literal_(..) et. al. ;-)
assert_missing_templates_in_bodies(Module) :-
    forall((
        current_predicate(Module:F/N), functor(Pred,F,N), clause(Module:Pred,Body), 
        sub_term(T,Body), nonvar(T), T=(A=B), 
        ((atomic(A),var(B)) -> Term=A ; (atomic(B),var(A)) -> Term=B),
        \+ number(Term),
        atomic_list_concat([_,_|_],' ',Term) % at least one space
        ),
            assert_semantic_error(warning,"Missing template for '~w'"-[Term],'rule body') 
            
    ).

:- dynamic module_xref_source/2. % LEmodule, Source_TemporaryFile

unqueried_predicates(TemplateStrings) :-
    psem(Module),
    unqueried_predicates(Module,TemplateStrings).

unqueried_predicates(Module,TemplateStrings) :-
    findall(TemplateString, unqueried_predicate(Module,TemplateString), TemplateStrings_),
    sort(TemplateStrings_,TemplateStrings).

unqueried_predicate(Module,TemplateString) :-
    module_xref_source(Module,Source), 
    xref_defined(Source,Unqueried,_How), 
    \+ Module:query(_,Unqueried),
    \+ (  a_caller(Unqueried,[],Source,Query), Module:query(_Name,Query)),
    Unqueried=..Ulist,
    catch((
        Module:local_dict(Ulist,TypesAndNames,Template),
        bindTemplate(Template,TypesAndNames,TemplateString)
        ),
        Ex,
        (print_message(warning,"~q"-[Ex]), Template='???')
    ).


% a_caller(?Called,+AncestorsPath,+XREFSource,-Caller) is nondet
a_caller(Called,Path,Source,Caller) :- 
    xref_called(Source,Called,By), 
    \+ member(By,Path), 
    (Caller = By ; a_caller(By,[By|Path],Source,Caller)).

undefined_le_predicates(TemplateStrings) :-
    psem(Module),
    undefined_le_predicates(Module,TemplateStrings).

undefined_le_predicates(Module,TemplateStrings) :-
    findall(TemplateString, undefined_le_predicate(Module,_Called,TemplateString), TemplateStrings_),
    sort(TemplateStrings_,TemplateStrings).

%duplicates included:
undefined_le_predicate(Module,Called,TemplateString) :-
	module_xref_source(Module,Source), xref_called(Source,Called,_By), 
    \+ xref_defined(Source,Called,_How), 
    \+ (Module:example(Name,Scenarios), Name\=null, member(scenario(Facts,_Flag),Scenarios), member(Called:-_,Facts)),
    \+ my_system_predicate(Called),
    Called=..CalledList,
    catch((
        Module:local_dict(CalledList,TypesAndNames,Template),
        % TODO: could also consider user_predef_dict, prolog_predef_dict, mostly system predicates
        bindTemplate(Template,TypesAndNames,TemplateString)
        ),
        Ex,
        (print_message(warning,"~q"-[Ex]), Template='???')
    ).

:- multifile prolog:xref_source_identifier/2. % missing this would cause xref_called etc to fail:
prolog:xref_source_identifier(File, File).

literal_to_sentence(Literal,Module,Sentence) :-
    Literal =..CalledList,
    catch((
        Module:local_dict(CalledList,_,Sentence_),
        atomic_list_concat(Sentence_,' ',Sentence)
        ),
        Ex,
        (print_message(warning,"Missing template for ~q ? ~q"-[Literal,Ex]), fail)
    ).


% bindTemplate('Template,+TypesAndNames,-TemplateString)
% Template is [Functor,Arg1,..,ArgN]
% TypesAndNames is a list of Type-Name
bindTemplate(Template,TypesAndNames,TemplateString) :-
    bindTemplate(Template,TypesAndNames),
    atomic_list_concat(Template, ' ', TemplateString).

bindTemplate([Var|Template],[Type-_|TypesAndNames]) :- var(Var), !,
        sub_atom(Type,0,1,_,First),
        (member(First,[a,e,i,o,u,'A','E','I','O','U']) -> Prefix=an; Prefix=a),
        format(string(Var),"*~a ~w*",[Prefix,Type]),
        bindTemplate(Template,TypesAndNames).
bindTemplate([_|Template],TypesAndNames) :- !,
        bindTemplate(Template,TypesAndNames).
bindTemplate([],_).

% Not using kp_loader:system_predicate, which depends on kp_dir
my_system_predicate(P) :- 
    predicate_property(P,built_in), !.
my_system_predicate(P) :- 
    predicate_property(P,file(Path)), 
    sub_atom(Path,_,_,_, 'swipl/library' ), !.


:- thread_local semantic_error_notice/3. % error/warning, Message, Context
clear_semantic_errors :- 
    retractall(semantic_error_notice(_,_,_)).

% Message can be string or Format-Args
assert_semantic_error(Type,Message,Context) :-
    assertion(Type==error;Type==warning),
    assert(semantic_error_notice(Type,Message,Context)).

show_semantic_errors :-
    forall(semantic_error_notice(Type,Message_,Context),(
        (Message_ = Format_-Args -> format(string(Format),"~a in ~w",[Format_,Context]) ; 
            format(string(Format),"~w in ~w",[Message_,Context]), Args=[] ),
        print_message(Type,Format-Args)
    )).

% parse_and_query(+OutputFileName, +Document, +QuestionOrQueryName, +ScenarioName, -AnswerExplanation)
% Document is a Language(LEtext) term, where Language is either of en, fr, it, es
% Returns ONLY the first answer
parse_and_query(File, Document, Question, Scenario, AnswerExplanation) :-
    parse_and_load(File, Document,_,ExpandedTerms,_), 
    answer( Question, Scenario, AnswerExplanation), 
    retracting(File, ExpandedTerms). 

parse_and_query_all_answers(File, Document, Question, Scenario, AnswerExplanation) :-
    parse_and_load(File, Document,_,ExpandedTerms,_), 
    answer_all(Question, Scenario, AnswerExplanation), !, 
    retracting(File, ExpandedTerms).  

% Generate an answer in html, with a nested list representing the explanation.
parse_and_query_and_explanation(File, Document, Question, Scenario, Answer, Result) :-
    parse_and_load(File, Document,_,ExpandedTerms,_), 
    hack_module_for_taxlog(File), % to enable the reasoner
    %print_message(informational, " Asserted ~w"-[ExpandedTerms]), 
    answer( Question, Scenario, le(LE_Explanation), Result), 
    retracting(File, ExpandedTerms), % cleaning memory
    %with_output_to(string(Answer), write(LE_Explanation)). 
    produce_html_explanation(LE_Explanation, Answer). 

% Generate a text-based answer, with a nested list representing the explanation.
% This does not retract the kb, to permit multiple queries.
parse_and_query_and_explanation_text(File, Document, Question, Scenario, Answer, Result) :-
    parse_and_load(File, Document,TaxlogTerms,_,_),
    hack_module_for_taxlog(File),
    (member(target(scasp),TaxlogTerms) -> answer(Question, Scenario);
    answer( Question, Scenario, le(LE_Explanation), Result)),    % cleaning memory
    % forall(member(T, [(:-module(File,[])), source_lang(en)|ExpandedTerms]), 
    %     ( %print_message(informational, "Removing File:T ~w:~w"-[File,T]), 
    %      retract(File:T))), 
    produce_text_explanation(LE_Explanation, Answer).

% Only query and return the text_explanation, without parsing again.
query_and_explanation_text(Question, Scenario, Answer, Result) :-
    answer(Question, Scenario, le(LE_Explanation), Result),
    produce_text_explanation(LE_Explanation, Answer). 

% non_expanded_terms/2 is just as the one above
non_expanded_terms(Name, TaxlogTerms, ExpandedTerms, NewFileName) :-
    %print_message(informational, " Translated ~w"-[TaxlogTerms]), 
	(TaxlogTerms\=[]-> 
        findall(PrologTerm, (
        member(TT_,TaxlogTerms), 
        (is_list(TT_)->member(TT,TT_);TT=TT_), % the LE translator generates a list of lists... and non lists
        ((member(target(prolog),TaxlogTerms);member(target(scasp),TaxlogTerms)) -> 
            semantics2prolog(TT,_,PrologTerm); true) % disabling taxlog translation
        ), ExpandedTerms_0) 
    ; ExpandedTerms_0 = []),
    % member(target(prolog),TaxlogTerms), 
    %print_message(informational, " Expanded ~w"-[ExpandedTerms_0]), 
    %kp_loader:myDeclaredModule(Name),
    %print_message(informational, " Module ~w "-[Name]), 
    % ExpandedTerms=ExpandedTerms_0. 
    % to save as a separated file
    (member(target(prolog),TaxlogTerms) -> 
        ( %myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-prolog','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-prolog', '+', URL], NewModule); atomic_list_concat([FileName,'-prolog'], NewModule)),
        %print_message(informational, " Processing module ~w filename ~w URL ~w"-[Name, FileName, URL]),  
        dump(all, NewModule, ExpandedTerms_0, String), 
        %print_message(informational, " To dump this ~w"-[String]),
        update_file(NewFileName, URL, String),
        ExpandedTerms_1 = [just_saved_scasp(null, null)|ExpandedTerms_0]) ; ExpandedTerms_1 = ExpandedTerms_0),
    %print_message(informational, " Terms ~w"-[ExpandedTerms_1]), 
    (member(target(scasp),TaxlogTerms) -> 
        ( %myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-scasp','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-scasp', '+', URL], NewModule); atomic_list_concat([FileName,'-scasp'], NewModule)), 
        %print_message(informational, "sCASP module name ~w"-[NewModule]), 
        dump_scasp(NewModule, ExpandedTerms_0, String), 
        %print_message(informational, "sCASP content to assert: ~w \n"-[String]), 
        update_file(NewFileName, NewModule, String),
        ExpandedTerms_2 = [just_saved_scasp(NewFileName, NewModule)|ExpandedTerms_1] ) ; ExpandedTerms_2 = ExpandedTerms_1),
        ExpandedTerms = ExpandedTerms_2. 

clean_explanation([], []) :- !. 
clean_explanation([s(P,_Ref, _Source, _, _, R)|RestConj], [s(P, RR)|NewConj]) :- 
    clean_explanation(R, RR), clean_explanation(RestConj, NewConj). 
clean_explanation([f(P,_Ref, _Source, _, _, R)|RestConj], [f(P, RR)|NewConj]) :- 
    clean_explanation(R, RR), clean_explanation(RestConj, NewConj).

produce_html_explanation(le_Explanation(Trees), Explanation) :-
    explanationLEHTML(Trees,HTML), 
    % phrase(html( 
	% 	div([ 'data-render'('An explanation')],[
    %         div([], ul(id="myUL", HTML))
	% 	]) 
	% ), ExplanationInHtml),
    phrase(html(HTML), ExplanationInHtml),
    with_output_to(string(Explanation), print_html(ExplanationInHtml)). 

explanationLEHTML(s(G,_Ref,_,_,_,C),[li(title="Rule inference",[span(class=Class," "), b(G)|RestTree])]) :- 
    %Navigator=' a rule', 
    explanationLEHTML(C,CH), 
    (CH\=[] -> 
        ( RestTree =  ul(class="nested", ['because'|CH]), Class = "box" )
    ;   ( RestTree = [], Class = "leaf" )
    ). 
explanationLEHTML(u(G,_Ref,_,_,_,[]),[li(title="Unknown",["~w ?"-[G],Navigator])]) :- Navigator=' a hypothesis'. 
explanationLEHTML(f(G,_Ref,_,_,_,C),[li(title="Failed goal",[span(class=Class, " "), span(style="color:red","It cannot be proved that "), b(G)|RestTree])]) :- 
    %Navigator=' in the rules', 
    explanationLEHTML(C,CH), 
    %print_message(informational, "G vs C: ~w .. ~w ... ~w"-[G, C, CH]), 
    %(CH\=[] -> (C=[s(_,_,_,_,_,[])] -> Because = 'although' ;  Because = 'because'); Because=''). % this is filtered out before (reasoner.pl)
    (CH\=[] -> 
        ( RestTree =  ul(class="nested", ['because'|CH]), Class = "box" )
    ;   ( RestTree = [], Class = "leaf" )
    ).
explanationLEHTML([C1|Cn],CH) :- explanationLEHTML(C1,CH1), explanationLEHTML(Cn,CHn), (CHn\=[] -> Joint = ['and '|CHn]; Joint = CHn), append(CH1,Joint,CH).
    %append(CH1,CHn,CH).
explanationLEHTML([],[]).

produce_text_explanation(le_Explanation(Trees), Explanation) :-
    explanationLEText(Trees,Text), 
    % phrase(html(Text), ExplanationInHtml),
    with_output_to(string(Explanation), write(Text)). 

explanationLEText(s(G,_Ref,_,_,_,C),[Gs|RestTree]) :- 
    %Navigator=' a rule', 
    explanationLEText(C,CH), 
    (CH\=[] -> 
        ( RestTree = [CH] )
    ;   ( RestTree = [] )
    ),
    with_output_to(string(Gs), format('"~w"', G)).
explanationLEText(u(G,_Ref,_,_,_,[]),[Gs]) :-
    with_output_to(string(Gs), format('"~w"', G)).
explanationLEText(f(G,_Ref,_,_,_,C),[Gs|RestTree]) :- 
    explanationLEText(C,CH), 
    (CH\=[] -> 
        ( RestTree = [CH] )
    ;   ( RestTree = [] )
    ),
    with_output_to(string(Gs), format('"It is not the case that: ~w"', G)).
explanationLEText([C1|Cn],CH) :- explanationLEText(C1,CH1), explanationLEText(Cn,CHn), append(CH1,CHn,CH).
explanationLEText([],[]).

% Moved here so it loads in the wasm version, that doesn't have access to api.pl
:- if(current_module(wasm)).
hack_module_for_taxlog(M) :-  
    retractall(kp_loader:module_api_hack(_)),
    assert(kp_loader:module_api_hack(M)).
:- endif.

%sandbox:safe_meta(term_singletons(X,Y), [X,Y]).
:- if(exists_source(library(pengines_sandbox))).
sandbox:safe_meta(le_answer:is_a(X,Y), [X,Y]). 
%sandbox:safe_primitive(user:is_a(_SubClass, _Class)).
sandbox:safe_primitive(le_answer:targetBody(_, _, _, _, _, _)). 
sandbox:safe_primitive(le_answer:answer( _EnText)).
sandbox:safe_primitive(le_answer:show( _Something)).
sandbox:safe_primitive(le_answer:show( _Something, _With)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario, _Result)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario, _Explanation, _Result)).
sandbox:safe_primitive(le_answer:le_taxlog_translate( _EnText, _File, _Baseline, _Terms)).
sandbox:safe_primitive(le_answer:translate_goal_into_LE(_,_)). 
sandbox:safe_primitive(le_answer:dump_scasp(_,_,_)). 
sandbox:safe_primitive(current_output(_)). 
sandbox:safe_primitive(le_answer:(show _)). 
sandbox:safe_primitive(le_answer:parse_and_query(_,_,_,_,_)).
sandbox:safe_primitive(le_answer:parse_and_query_and_explanation(_,_,_,_,_,_)). 
sandbox:safe_primitive(kp_loader:module_api_hack(_)).
:- endif.

%sandbox:safe_primitive(term_singletons(_,_)).  % this would not work as term_singletons/2 is an undefined, C-based primitive