% Simple wrapper to use LogicalEnglish from the PROLOG command line

:- use_module(le_answer).

:- multifile prolog:message//1.
prolog:message(S-Args) --> {atomic(S),is_list(Args)},[S-Args].

example_en("the target language is: prolog.


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
    
which person acquires British citizenship on which date.").

% Example:
% example_en(LE), parse_and_query(foobar, en(LE), one, with(alice), AnswerExplanation).
% example_en(LE), parse_and_query_all_answers(foobar, en(LE), one, with(alice), AnswerExplanation).
% example_en(LE), parse_and_query_and_explanation(foobar, en(LE), one, with(alice), AnswerExplanation,Result).