% Simple wrapper to use LogicalEnglish from the PROLOG command line

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
        parse_and_load(Module, LEterm,TaxlogTerms,ExpandedTerms,File),
        (DeleteFile==true -> nonvar(File), delete_file(File); true)
    ).


load_program(FileOrTerm) :- 
    load_program(FileOrTerm,_Language,true,Module,_TaxlogTerms,_ExpandedTerms),
    print_message(informational,"Loaded into ~w"-[Module]).

query_program_one(Module,Question, Scenario, AnswerExplanation) :-
    set_psem(Module),
    le_answer:answer( Question, Scenario, AnswerExplanation).

query_program_one(Question, Scenario, AnswerExplanation) :-
    psem(Module),
    query_program_one(Module,Question, Scenario, AnswerExplanation).

query_program_all(Module,Question, Scenario, AnswerExplanation) :-
    set_psem(Module),
    le_answer:answer_all( Question, Scenario, AnswerExplanation).

query_program_all(Question, Scenario, AnswerExplanation) :-
    psem(Module),
    query_program_all(Module,Question, Scenario, AnswerExplanation).

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
