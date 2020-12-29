% Things that need to be defined in the user module, so Swish finds them
% To start as local server:
% /Applications/SWI-Prolog8.1.9.app/Contents/MacOS/swipl -l user_module_for_swish.pl -l ../../swish/server.pl -g server:server
%  (local Docker testing:
% docker run -it -p 3051:3050 -v /Users/mc/git/TaxKB/swish/data:/data -v /Users/mc/git/TaxKB:/app -e LOAD='/app/swish/user_module_for_swish.pl' logicalcontracts/customprivateswish
% or (Docker on Ubuntu server):
% docker run -d --restart always -p 8082:3050 -v /home/ubuntu/TaxKB_swish_data:/data -v /home/ubuntu/TaxKB:/TaxKB -e LOAD='/TaxKB/swish/user_module_for_swish.pl' logicalcontracts/customprivateswish

:- multifile sandbox:safe_primitive/1.
 
% For debugging:
% can not print output as usual, would interfere with http responses; uncomment the following for a log:
/*
:- open('mylog.txt',write,S), assert(mylogFile(S)).
mylog(M) :- mylogFile(S), thread_self(T), writeq(S,T:M), nl(S), flush_output(S).
% :- asserta((prolog:message(A,B,C) :-  mylog(message-A), fail)).
sandbox:safe_primitive(user:mylog(_M)). 
*/
:- use_module(library(settings)).
%:- use_module(library(http/http_log)). % uncomment to produce httpd.log
%:- set_setting_default(http:logfile, 'data/httpd.log'). % swish's writable sub directory

:- multifile swish_config:config/2.
swish_config:config(show_beware,true).
swish_config:config(community_examples,false).
% don't see the point: swish_config:config(public_access,true). % HACK here
swish_config:config(chat,false).
% Forcing SWISH to accept both of the following as legitimate; 
% only the first will get to the Javascript side, I think... but no harm done (apparently):
swish_config:config(include_alias,	example).
% the above facts must come before this...:
:- use_module('../../swish/swish').

:- use_module(swish(lib/render)).
:- use_module(library(http/http_dispatch)).
:- use_module(swish(lib/plugin/login)).
:- use_module(library(pengines)). % used only under SWISH

:- prolog_load_context(directory, D), atomic_list_concat([D,/,passwd],F), set_setting(swish_http_authenticate:password_file,F), format("Password file at ~a~n",[F]).
:- use_module(swish(lib/authenticate)).

:- use_module('../reasoner.pl'). 
:- use_module('../syntax.pl').
:- use_module('../kp_loader.pl').

:- initialization( (discover_kps_gitty,xref_all, writeln("Ready!"))).

sandbox:safe_primitive(reasoner:query(_,_)).

:- use_rendering(user:table, [
    header(at('Unknown Predicate','Knowledge Page')) 
    ]).

:- use_rendering(graphviz).

:- multifile user:file_search_path/2.
user:file_search_path(example, D) :- kp_dir(D).

% PATCH to swish to avoid duplicate example and help menu and profile entries on Linux
% list_without_duplicates(+L,-NL) 
% Remove duplicates from list L, but keeping first occurrences in their original order
list_without_duplicates([X|L],[X|NL]) :- select(X,L,LL), !, list_without_duplicates(LL,NL).
list_without_duplicates([X|L],[X|NL]) :- !, list_without_duplicates(L,NL).
list_without_duplicates([],[]).
:- dynamic(swish_help:help_files/1). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((
swish_help:help_files(AllExamples) :-
	findall(Index,
		absolute_file_name(swish_help(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs_), 
	list_without_duplicates(ExDirs_,ExDirs), % patch
	maplist(swish_help:index_json, ExDirs, JSON),
	append(JSON, AllExamples),
	!
)).
:- use_module(library(http/http_path)).
:- dynamic(swish_examples:swish_examples_no_cache/1). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((
	swish_examples:swish_examples_no_cache(SWISHExamples) :-
		http_absolute_location(swish(example), HREF, []),
		findall(Index,
			absolute_file_name(example(.), Index,
					   [ access(read),
						 file_type(directory),
						 file_errors(fail),
						 solutions(all)
					   ]),
			ExDirs_),
		list_without_duplicates(ExDirs_,ExDirs), % patch..
		maplist(swish_examples:index_json(HREF), ExDirs, SWISHExamples)
)).

:- multifile prolog_colour:term_colours/2, prolog_colour:goal_classification/2 /*??, prolog_colour:goal_colours/2*/.
% Wire our colouring logic into SWI's:
prolog_colour:term_colours(T,C) :- taxlog2prolog(T,C,_).

%prolog_colour:goal_classification(Goal, Class) :- syntax:goal_classification(Goal, Class).

% This at the end, as it activates the term expansion (no harm done otherwise, just some performance..):
:- use_module('../syntax.pl').
