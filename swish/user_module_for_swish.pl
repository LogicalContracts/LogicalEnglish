% Things that need to be defined in the user module, so Swish finds them
% To start as local server:
% export SPACY_HOST=localhost:8080; /Applications/SWI-Prolog8.1.9.app/Contents/MacOS/swipl -l user_module_for_swish.pl -l ../../swish/server.pl -g server:server
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

:- use_module(library(http/http_dispatch)).
:- use_module(swish(lib/plugin/login)).
:- use_module(library(pengines)). % used only under SWISH

:- use_module(library(http/html_write)).
:- use_module(swish(lib/page)).

:- prolog_load_context(directory, D), atomic_list_concat([D,/,passwd],F), set_setting(swish_http_authenticate:password_file,F), format("Password file at ~a~n",[F]).
:- use_module(swish(lib/authenticate)).

:- use_module('../syntax.pl').
:- use_module('../kp_loader.pl').
:- use_module('../reasoner.pl'). 
:- use_module('../api.pl').
:- use_module('../spacy/spacy.pl').

:- use_module('../spacy/hierplane_renderer.pl',[]).
:- use_rendering(hierplane,[]).

:- use_module('explanation_renderer',[]).
:- use_rendering(explanation_renderer).

:- initialization( (discover_kps_gitty, setup_kp_modules, xref_all, writeln("Ready on SWISH!"))).

:- use_module(swish(lib/render)).

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

:- multifile swish_config:reply_page/1. % redefine SWISH's page maker, namely so we can inject common scripts or CSSs:
swish_config:reply_page(Options) :- 
	reply_html_page(
	    swish(main),
	    \(swish_page:swish_title(Options)),
	    \my_swish_page(Options)).


:- multifile user:forbidden_url/1.
forbidden_url(_) :- fail. % all URLs allowed by default
% Example:
% forbidden_url('/example/bankTransfer.pl') :- lps_user(unknown_user).


my_swish_page(Options) -->
	{
		((option(url(URL),Options), forbidden_url(URL)) ->
			throw(no_permission_for(URL))
			; true)
	},
	my_swish_navbar(Options),
	swish_content(Options). % no need to inject resources here again... is there??

my_swish_navbar(Options) -->
	my_swish_resources, % this may have to move to after the next call's inhards...
	swish_navbar(Options).
	
my_swish_resources -->
	{findall(R,extra_swish_resource(R),Resources)},
	html_post_resources(Resources).

html_post_resources([R|Resources]) --> {!}, html_post(head, R), html_post_resources(Resources).
html_post_resources([]) --> {true}.

:- multifile user:extra_swish_resource/1. % declare a link or script resource to include in the SWISH page's head
% extra_swish_resource(link([ type('text/css'),rel('stylesheet'),href('/lps/lps.css') ])).
% extra_swish_resource(script(JS)) :- google_analytics_script(JS).


:- multifile prolog_colour:term_colours/2.
% Wire our colouring logic into SWI's:
prolog_colour:term_colours(T,C) :- taxlog2prolog(T,C,_).

% This at the end, as it activates the term expansion (no harm done otherwise, just some performance..):
user:term_expansion(T,NT) :- taxlog2prolog(T,_,NT).

