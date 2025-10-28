% Simple wrapper to use LogicalEnglish from the PROLOG command line
% Launch with /Applications/SWI-Prolog9.3.7-1.app/Contents/MacOS/swipl -l le_cli.pl
:- module(_,[
    load_program/6, load_program/1, undefined_le_predicates/2, 
    query_program_all/5, query_program_all/4,
    verify_expectations/1]).


:- multifile prolog:message//1.
prolog:message(S-Args) --> {atomic(S),is_list(Args)},[S-Args].

:- use_module(le_answer).

load_program(FileOrTerm,Language,DeleteFile,Module,TaxlogTerms,ExpandedTerms) :- 
    must_be(boolean,DeleteFile),
    (var(Module) -> uuid(Module) ; true),
    ((atomic(FileOrTerm), exists_file(FileOrTerm)) -> 
        (var(Language) -> Language=en; true),
        read_file_to_string(FileOrTerm, Text, [])
        ;
        FileOrTerm=..[Language,Text]
    ),
    assertion(member(Language,[en,fr,es,it])),
    LEterm=..[Language,Text],
    %TODO: this NOT deleting file when parsing fails:
    setup_call_cleanup(
        true, 
        (
            parse_and_load(Module, LEterm,TaxlogTerms,ExpandedTerms,File),
            % Dict = dict(_PredAsList,_TypesAndNames, _Template),
            % findall(Dict,le_input:Dict,Dicts),
            (atomic(FileOrTerm) -> xref_source(File), assert(module_xref_source(Module,File,FileOrTerm)) ; true)
        ),
        (DeleteFile==true -> nonvar(File), delete_file(File); true)
    ).

load_program(FileOrTerm) :- 
    load_program(FileOrTerm,_Language,true,Module,_TaxlogTerms,_ExpandedTerms),
    print_message(informational,"Loaded into module ~w"-[Module]).

:- dynamic module_xref_source/3. % LEmodule, Source_TemporaryFile, OriginalFile

undefined_le_predicates(TemplateStrings) :-
    psem(Module),
    undefined_le_predicates(Module,TemplateStrings).

undefined_le_predicates(Module,TemplateStrings) :-
    findall(TemplateString, undefined_le_predicate(Module,_Called,TemplateString), TemplateStrings_),
    sort(TemplateStrings_,TemplateStrings).

%duplicates included:
undefined_le_predicate(Module,Called,TemplateString) :-
	module_xref_source(Module,Source,_File), xref_called(Source,Called,_By), 
    \+ xref_defined(Source,Called,_How), 
    \+ (Module:example(Name,Scenarios), Name\=null, member(scenario(Facts,_Flag),Scenarios), member(Called:-_,Facts)),
    \+ my_system_predicate(Called),
    Called=..CalledList,
    catch((
        Module:local_dict(CalledList,TypesAndNames,Template),
        bindTemplate(Template,TypesAndNames,TemplateString)
        ),
        Ex,
        (print_message(warning,"~q"-[Ex]), Template='???')
    ).

:- multifile prolog:xref_source_identifier/2. % missing this would cause xref_called etc to fail:
prolog:xref_source_identifier(File, File).

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

query_program_one(Module,Question, Scenario_, AnswerExplanation) :-
    set_psem(Module),
    le_answer:restore_dicts_from_module(Module),
    (Scenario_=with(_) -> Scenario=Scenario_ ; Scenario=with(Scenario_)),
    le_answer:answer( Question, Scenario, AnswerExplanation).

query_program_one(Question, Scenario, AnswerExplanation) :-
    psem(Module),
    query_program_one(Module,Question, Scenario, AnswerExplanation).

% query_program_all(+Module,+Question, +Scenario, -AnswerExplanations,-Answers)
% Answers is a list of positive answers; if Answers is [], AnswerExplanations will contain items with the negative explanations
query_program_all(Module,Question, Scenario_, AnswerExplanations,Answers) :-
    set_psem(Module),
    le_answer:restore_dicts_from_module(Module),
    (Scenario_=with(_) -> Scenario=Scenario_ ; Scenario=with(Scenario_)),
    le_answer:answer_all( Question, Scenario, AnswerExplanations),
    findall(Answer, (
            member(AnswerExplanation,AnswerExplanations), 
            get_dict(answer,AnswerExplanation,'Yes'),
            get_dict(bindings, AnswerExplanation, Answer_),
            term_string(Answer,Answer_)
            ), Answers),
    length(Answers,Positives), length(AnswerExplanations,All),
    assertion(Positives+1>=All).

query_program_all(Question, Scenario, AnswerExplanations,Answers) :-
    psem(Module),
    query_program_all(Module,Question, Scenario, AnswerExplanations,Answers).

% generate_expectations(+LEfileOrDir)
% WARNIKNG: this will OVERWRITE all test results files
generate_expectations(TestsDir) :- exists_directory(TestsDir), !,
    all_files_in(TestsDir,'.le',[],LEfiles),
    forall(member(LEfile,LEfiles), (
        print_message(informational,"Generating expectations for ~w"-[LEfile]),
        generate_expectations(LEfile)
        )).

generate_expectations(LEfile) :-
    Language = en, %TODO: how to accept other languages?
    (load_program(LEfile,Language,true,Module,_TaxlogTerms,ExpandedTerms) -> 
        findall(example(Name,Facts), (member(example(Name,Facts),ExpandedTerms), Name\==null), Examples),
        findall(query(Name,Goal), (member(query(Name,Goal),ExpandedTerms), Name\==null), Queries),
        findall(expected(Query,Scenario,Answers), (
            member(query(Query,Goal),Queries), member(example(Scenario,Facts),Examples),
            % print_message(informational,"Generating for query ~w, scenario ~w"-[Query,Scenario]),
            query_program_all(Module,Query, with(Scenario), _AnswerExplanations, Answers_),
            term_to_clean_string(Answers_,Answers)
            ), Expectations)
            ; Expectations = []),
    format(string(NewFile),"~a.tests",[LEfile]),
    open(NewFile,write,Stream),
    forall(member(Expectation,Expectations), format(Stream,"~q.~n",[Expectation])),
    close(Stream).

verify_expectations(TestsDir) :- exists_directory(TestsDir), !,
    all_files_in(TestsDir,'.tests',[],TestFiles),
    length(TestFiles,Nfiles),
    get_time(Start),
    findall(TestFile-Result,(
        member(TestPath,TestFiles),
        file_base_name(TestPath, TestFile),
        print_message(informational,"Running tests in ~w..."-[TestPath]),
        verify_expectations(TestPath,Result)
        ), 
        Results),
    get_time(End), Duration is round((End-Start)*1000)/1000,
    findall(Ntests, (member(_-Result,Results), Result=..[_,Ntests|_]), TestCounts),
    sum_list(TestCounts, NtestsTotal),
    print_message(informational,"Ran ~w tests in ~w files in ~w seconds~n~nRESULTS:~n"-[NtestsTotal,Nfiles,Duration]),
    format(string(ResultsFile),"~a/LEtests.log",[TestsDir]),
    open(ResultsFile,write,Stream),
    forall(member(File-Result,Results),(
        (Result=..[ok|_] -> Kind=informational, Prefix='' ; Kind=warning, Prefix='NOT OK: '),
        print_message(Kind,"~w: ~q"-[File,Result]),
        format(Stream,"~a~w: ~q~n",[Prefix,File,Result])
    )),
    ( forall(member(_-Result,Results), Result=..[ok|_]) -> 
        print_message(informational,"~nALL GOOD :-)"-[]), 
        format(Stream,"~nALL GOOD :-)~n",[])
        ;
        findall(File,(member(File-Result,Results), \+ Result=..[ok|_]), BadFiles_), sort(BadFiles_,BadFiles),
        length(BadFiles,BadFilesCount),
        print_message(error,"~nTESTS HAVE FAILED in ~w of ~w files:-("-[BadFilesCount,Nfiles]),
        format(Stream,"~nTESTS HAVE FAILED :-(~n",[])
    ),
    close(Stream).


verify_expectations(TestFile,Result) :-
    atom_concat(LEfile,'.tests',TestFile),
    read_file_to_terms(TestFile, Expectations, []),
    ( load_program(LEfile) ->
        undefined_le_predicates(TemplateStrings),
        (TemplateStrings \= [] -> 
            forall(member(TS,TemplateStrings),print_message(warning," Undefined: ~q"-[TS]))
            ; true),
        findall(Outcome,(
            member(expected(Query,Scenario,ExpectedAnswers),Expectations),
            (   query_program_all(Query, with(Scenario), AnswerExplanations,Answers) -> 
                    ((term_to_clean_string(Answers,ExpectedAnswers_), ExpectedAnswers_=ExpectedAnswers) -> 
                        Outcome=ok 
                        ; 
                        format("scenario ~w query ~w~n",[Scenario,Query]),
                        format("AE: ~q~n",[AnswerExplanations]),
                        format("expected ~q got ~q~n",[ExpectedAnswers, Answers]),
                        Outcome=expected(ExpectedAnswers)-got(Answers)
                    )
                ;
                Outcome=failed
            )
            ),Outcomes)
        ; Outcomes = [failed(load_program)]),
    length(Expectations,Ntests),
    (forall(member(Outcome,Outcomes),Outcome==ok) -> 
        Result=ok(Ntests) ;
        member(failed(What),Outcomes) -> 
            Result=failed(Ntests,What) ;
        nth1(Index, Outcomes, failed) -> 
            nth1(Index,Expectations,FailedExpectation), Result=failed(Ntests,FailedExpectation) ;
        findall(Expected-Got, member(Expected-Got,Outcomes), Unexpecteds), 
        Result=unexpected(Ntests,Unexpecteds)
    ).


% Show a HTML fragment in the user's browser
show_html(H) :-
    tmp_file(explanation, TmpName_),
    format(string(TmpName),"~a.html",[TmpName_]),
    open(TmpName,write,Stream),
    format(Stream,"<html><body>",[]),
    writeln(Stream,H),
    format(Stream,"</body></html>",[]),
    close(Stream),
    www_open_url(TmpName).

% Suffix typically being .extension
% collects all files with given extension suffix in Directory subtree, recursively
% excludes base filenames in the Except arg
all_files_in(Directory,Suffix,Except,Files) :-
    must_be(list,Except),
    (is_list(Suffix)->Suffix=SuffixCodes;atom_codes(Suffix,SuffixCodes)),
    directory_files(Directory,DFiles), 
	findall( File, (
        member(F,DFiles), 
		F \== '.', F \== '..', \+ member(F,Except),
		concat_atom([Directory,'/',F],FullF),
		(exists_directory(FullF) -> 
			all_files_in(FullF,Suffix,DFiles), member(File,DFiles) 
			; 
			atom_codes(F,FC), 
			append(_PFcodes,SuffixCodes,FC), 
			concat_atom([Directory,'/',F],File))
		),
		Files).

all_files_in(Directory,Suffix,Files) :- all_files_in(Directory,Suffix,[],Files).

%%% EXAMPLES

example1(en("the target language is: prolog.


the templates are:
*a person* acquires British citizenship on *a date*.
*a person* is born in *a place* on *a date*,
*a date* is after commencement,
*a person* is the mother of *a person*,
*a person* is the father of *a person*,
*a person* is a British citizen on *a date*,
*a person* became a British citizen on *a date*,
*a person* is settled in the UK on *a date*,
*a person* says that *a sentence*,
*a person* is authorised to determine fatherhood.


the knowledge base citizenship includes:
a person acquires British citizenship on a date if 
    the person is born in the UK on the date
    and the date is after commencement
    and an other person is the mother of the person
        or the other person is the father of the person
    and the other person is a British citizen on the date
        or the other person is settled in the UK on the date.

a person is a British citizen on a date if
    the person became a British citizen on another date D 
    and D =< the date.

a date is after commencement if 
    the date >= 1983-01-01.


scenario alice is:
John is born in the UK on 2021-10-09.
Mike is born in the UK on 2022-10-09.
Alice is the mother of John.
Alice is the mother of Mike.
Alice became a British citizen on 1990-10-09.
        
query one is:
    
which person acquires British citizenship on which date.")).

% Example:
% example1(LE), parse_and_query(foobar, LE, one, with(alice), AnswerExplanation).
% example1(LE), parse_and_query_all_answers(foobar, LE, one, with(alice), AnswerExplanation).
% example1(LE), parse_and_query_and_explanation(foobar, LE, one, with(alice), AnswerExplanation,Result).
% example1(LE), parse_and_query_and_explanation_text(foobar, LE, one, with(alice), AnswerExplanation,Result).
% load_program('/Users/mc/git/LogicalEnglish/moreExamples/payg.le'), ...
