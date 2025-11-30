#!/usr/bin/env swipl

% For example 
% /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=list /Users/mc/git/LogicalEnglish/moreExamples
% /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=verify /Users/mc/git/LogicalEnglish/moreExamples/citizenship.le
% /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=query --query="which person is the mother of which other person." --scenario="Clara is the mother of Miguel." /Users/mc/git/LogicalEnglish/moreExamples/citizenship.le
% cd ~/git/LogicalEnglish ; /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=query --query="which person acquires British citizenship on which date." --scenario="John is born in the UK on 2021-10-09. 2021-10-09 is after commencement. Harry is the father of John. Harry is settled in the UK on 2021-10-09." /Users/mc/git/LogicalEnglish/moreExamples/citizenship.le
% --command=list/verify/load/query
% Useful to obtain error messages from loading and/or verifying expectations (test results) for a program, as well as straight querying a LE program
:- use_module(le_cli).

:- initialization(main, main).
:- use_module(library(optparse)).

% Define the option specifications
opt_spec([
    [opt(command), 
     type(atom),
     shortflags([c]),
     longflags([command]),
     help('Command to execute')],
    [opt(query), 
     type(atom),
     shortflags([q]),
     longflags([query]),
     help('extra LE query to execute')],
    [opt(scenario), 
     type(atom),
     shortflags([s]),
     longflags([scenario]),
     help('extra LE scenario to consider')]
]).

main(Argv) :-
    opt_spec(OptsSpec),
    opt_parse(OptsSpec, Argv, Opts, PositionalArgs),
    (   PositionalArgs = [File] ->  true 
        ; 
        format(user_error, 'Expected exactly one Logical English file~n', []),
        halt(1) 
    ),   
    ( memberchk(command(Command), Opts) ->  
        (Command == load -> load_program(File) ; 
            Command == verify -> verify_expectations(File) ;
            Command == list -> print_programs_and_main_predicates(File) ;
            Command == mcp -> mcpLoop(File) ;
            Command == top_predicates -> print_top_predicates(File) ;
            Command == query -> 
                memberchk(query(Q), Opts), memberchk(scenario(S), Opts),
                load_and_query_program_all(File,S,Q,AnswerExplanations,_Answers,Sentences),
                forall(nth1(Index, Sentences,Sentence), (
                    format("ANSWER ~w:~n",[Index]),
                    nth1(Index,AnswerExplanations,Explanation),
                    format("~a because: ~w",[Sentence,Explanation.explanation])
                ))
            ;
        format(user_error, "Invalid command", []))
        ;  
        format('No command specified for file ~w~n', [File])
    ),
    halt(0).

mcpLoop(Dir) :-
    introspect_programs_and_main_predicates(Dir,Pairs),
    writeln(Pairs).