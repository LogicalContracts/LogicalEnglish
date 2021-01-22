% To start as a local development (serverless) engine:
% /Applications/SWI-Prolog8.1.9.app/Contents/MacOS/swipl -l user_module_for_barebones.pl

:- use_module('syntax.pl').
:- use_module('kp_loader.pl').
:- use_module('reasoner.pl'). 
:- use_module('api.pl').
:- use_module('drafter.pl').

:- initialization( (discover_kps_in_dir, setup_kp_modules, xref_all, writeln("Ready!"))).

% counterpart to our little homebrewn SWISH logger
% mylog(M) :- thread_self(T), writeq(T:M), nl.
:- open('mylog.txt',write,S), assert(mylogFile(S)).
mylog(M) :- mylogFile(S), thread_self(T), writeq(S,T:M), nl(S), flush_output(S).

:- multifile prolog_colour:term_colours/2.
% Wire our colouring logic into SWI's:
prolog_colour:term_colours(T,C) :- taxlog2prolog(T,C,_).

% This at the end, as it activates the term expansion (no harm done otherwise, just some performance..):
user:term_expansion(T,NT) :- taxlog2prolog(T,_,NT).
