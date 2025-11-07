#!/usr/bin/env swipl

% For example /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=verify /Users/mc/git/LogicalEnglish/moreExamples/citizenship.le
% --command=verify/load
% Useful to obtain error messages from loading and/or verifying expectations (test results) for a program
:- use_module(le_cli).

:- initialization(main, main).
:- use_module(library(optparse)).

% Define the option specifications
opt_spec([
    [opt(command), 
     type(atom),
     shortflags([c]),
     longflags([command]),
     help('Command to execute')]
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
        format('File: ~w~n', [File]),
        format('Command: ~w~n', [Command]),
        (Command == load -> load_program(File) ; 
            Command == verify -> verify_expectations(File) ;
        format(user_error, "Invalid command", []))
        ;  
        format('No command specified for file ~w~n', [File])
    ),
    halt(0).