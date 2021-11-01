% Things that need to be defined in the user module, so Swish finds them

% To start as local server:
% export SPACY_HOST=localhost:8080; export LOAD_KB=true; export SUDO=true; /Applications/SWI-Prolog8.2.1-1.app/Contents/MacOS/swipl -l user_module_for_swish.pl -l ../../swish/server.pl -g server:server
%  (local Docker testing:
% docker run -it -p 3051:3050 -v /Users/mc/git/TaxKB/swish/data:/data -v /Users/mc/git/TaxKB:/app -e LOAD='/app/swish/user_module_for_swish.pl' -e SPACY_HOST='host.docker.internal:8080' -e LOAD_KB=false -e SUDO=true logicalcontracts/patchedprivateswish
% or (Docker on Ubuntu server):
% docker run -d --restart always -p 8082:3050 -v /home/ubuntu/TaxKB_swish_data:/data -v /home/ubuntu/TaxKB:/TaxKB -e LOAD='/TaxKB/swish/user_module_for_swish.pl' -e SPACY_HOST='172.31.6.64:8080' -e LOAD_KB=false -e SUDO=true logicalcontracts/patchedprivateswish

:- multifile sandbox:safe_primitive/1.

% For debugging:
% can not print output as usual, would interfere with http responses; uncomment the following for a log:
/*
mylog(M_) :- mylogFile(S), thread_self(T), ((M_=Template-Args,is_list(Args)) -> (format(S,"~w:",[T]), format(S,Template,Args), nl(S)) ; (M_=M,writeq(S,T:M), nl(S))), flush_output(S).
:- prolog_load_context(directory, D), atomic_list_concat([D,/,'mylog.txt'],F), open(F,write,S), assert(mylogFile(S)), mylog('Log started').
% :- asserta((prolog:message(A,B,C) :-  mylog(message-A), fail)).
sandbox:safe_primitive(user:mylog(_M)). 
*/

:- multifile prolog:message//1.
% too strong, would override swipl-devel/boot/messages.pl:  prolog:message(X) --> {atomic(X)}, ['~w'-[X]].
prolog:message(S-Args) --> {atomic(S),is_list(Args)},[S-Args].

user:sudo(G) :- (G).
sandbox:safe_primitive(user:sudo(_)) :- getenv('SUDO',true). 

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

% :- prolog_load_context(directory, D), atomic_list_concat([D,/,passwd],F), set_setting(swish_http_authenticate:password_file,F), format("Password file at ~a~n",[F]).
% :- use_module(swish(lib/authenticate)).

:- use_module('../syntax.pl').
:- use_module('../kp_loader.pl').
:- use_module('../reasoner.pl'). 
:- use_module('../api.pl').
:- use_module('../spacy/spacy.pl').
:- use_module('../drafter.pl').
:- use_module('../le_output.pl').
:- use_module('../le_input.pl').

:- use_module(swish(lib/html_output),[html/1]).
myhtml(H) :-  pengine_self(SwishModule), !, SwishModule:html(H).
myhtml(Spec) :-  % for command line usage:
	phrase(html(Spec), Tokens),
	with_output_to(
		string(HTML),
		print_html(current_output, Tokens)),
	format('~w', [HTML]).


sandbox:safe_primitive(user:myhtml(_)). 


:- use_module('../spacy/hierplane_renderer.pl',[]).
:- use_rendering(hierplane,[]).

% This Javascript function and clauseNavigator are used by the renderers below
:- multifile user:extra_swish_resource/1. % declare a link or script resource to include in the SWISH page's head
user:extra_swish_resource(script("
    function myPlayFile(filename,line){
    console.log(filename+' '+line);
	var available = $('body').find('.storage').storage('match', {file:filename});
	if (available) {
		var message = null;
		$('body').find('.storage').storage('match', {file:filename}) . storage('expose',null);
		$('.active').find('.prolog-editor').prologEditor('gotoLine', line, null).focus();  
	} else $('body').swish('playFile', { file:filename, line:line }); 
}
")).

% clauseNavigator(+ClauseRef,-HTML)
% clauseNavigator(Ref,a([onclick="myPlayFile('cgt_affiliates.pl',26);"],"SOURCE")).
clauseNavigator(C,H) :- clauseNavigator_(C,H,_).
clauseNavigator(C,H,AP) :- clauseNavigator_(C,H,AP).

% returns an element, as well as an anchor property
clauseNavigator_(Ref,span([a([onclick=Handler]," TaxLog")|Origin]), onclick=Handler) :- 
    blob(Ref,clause), clause_property(Ref,file(F_)), clause_property(Ref,line_count(L)),
    myClause2(_H,_Time,Module_,_Body,Ref,_IsProlog,_URL,_E), 
    !,
    % Module_ will be the temporary SWISH module with the current window's program
    % This seems to break links to source: (shouldMapModule(Module,Module_)-> kp_location(Module,F,true) ;(
    (moduleMapping(Module,Module_)-> must_succeed(kp_location(Module,F,true),one) ;(
        % strip swish "file" header if present:
		(sub_atom(F_,0,_,R,'swish://'), sub_atom(F_,_,R,0,F)) -> true ; F=F_
        )),
    refToOrigin(Ref,URL),
    % could probably use https://www.swi-prolog.org/pldoc/doc_for?object=js_call//1 , but having trouble embedding that as attribute above:
    format(string(Handler),"myPlayFile('~a',~w);",[F,L]),
    Origin = [a([href=URL, target='_self']," Text")].
clauseNavigator_(Ref,i(" ~w"-[Ref]),onclick='').


:- use_module(explanation_renderer,[]).
:- use_rendering(explanation_renderer).
:- use_module(unknowns_renderer,[]).
:- use_rendering(unknowns_renderer).
:- use_module(le_link_renderer,[]).
:- use_rendering(le_link_renderer).

:- initialization( (
	(getenv('LOAD_KB',true) -> (
		print_message(informational,"Updating SWISH storage with latest KB"-[]), load_gitty_files
		) ; true),
	discover_kps_gitty, setup_kp_modules, xref_all, 
	current_prolog_flag(version_data,V), print_message(informational,V),
	print_message(informational,"Ready on SWISH!"-[])
)).

:- use_module(swish(lib/render)).

:- use_rendering(user:table, [
	% header(at('Unknown Predicate','Knowledge Page')),  We now use the unknowns_renderer instead
	%_I,_Offset,Word,_Lemma,_POS,_Tag,_Head,_Dep,_Absorbed
    header(t('#','Offset','Word','Lemma','POS','Tag','Head','Dep','Absorbed')) 
    ]).


:- use_rendering(graphviz).

:- multifile user:file_search_path/2.
user:file_search_path(example, D) :- kp_dir(D).
user:file_search_path(profile, PD) :- taxkb_dir(D), format(string(PD),"~a/swish/profiles",[D]).


% PATCH to swish to avoid duplicate example and help menu and profile entries on Linux
% list_without_duplicates(+L,-NL) 
% Remove duplicates from list L, but keeping first occurrences in their original order
list_without_duplicates([X|L],[X|NL]) :- remove_all(L,X,LL), !, list_without_duplicates(LL,NL).
list_without_duplicates([],[]).
remove_all(L,T,NL) :- select(T,L,LL), !, remove_all(LL,T,NL).
remove_all(L,_,L).

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

% The Google Analytics key file must be placed in the SWISH data directory:
% :- catch(read_file_to_string('data/googleAnalyticsKey',Key,[]),_,Key=''), 
% 	format(atom(JS),'
%   (function(i,s,o,g,r,a,m){i[\'GoogleAnalyticsObject\']=r;i[r]=i[r]||function(){
%   (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
%   m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
%   })(window,document,\'script\',\'https://www.google-analytics.com/analytics.js\',\'ga\');

%   ga(\'create\', \'~w\', \'auto\');
%   ga(\'send\', \'pageview\');
% ',[Key]), assert(google_analytics_script(JS)).


:- multifile user:file_search_path/2.
user:file_search_path(taxkb_resources, D) :- taxkb_dir(XD), concat_atom([XD,"/swish/web"],D).

:- http_handler(root(taxkb), serve_taxkb_resources, [prefix]). 
serve_taxkb_resources(Request) :- 
	option(path(Info), Request),  
	http_reply_file(taxkb_resources(Info), [], Request).


:- multifile prolog_colour:term_colours/2.
% Wire our colouring logic into SWI's:
prolog_colour:term_colours(T,C) :- taxlog2prolog(T,C,_).

% first term expansion to support en/1 
%user:term_expansion(NiceTerm,'$source_location'(File, Line):ExpandedTerms) :- 
user:term_expansion(NiceTerm, ExpandedTerms) :-  % hook for LE extension
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(
	context_module(user), % LPS programs are in the user module
	prolog_load_context(source,File), % atom_prefix(File,'pengine://'), % process only SWISH windows
	prolog_load_context(term_position,TP), stream_position_data(line_count,TP,Line),
	%catch(le_taxlog_translate(NiceTerm,File,Line,TaxlogTerms),E,	
	%	(print_message(error,"Translation Error: ~w"-[E]),fail)), 
	le_taxlog_translate(NiceTerm,File,Line,TaxlogTerms), 
	(TaxlogTerms\=[]-> 
		( findall(PrologTerm, (
			member(TT_,TaxlogTerms), 
			(is_list(TT_)->member(TT,TT_);TT=TT_), % the LE translator generates a list of lists... and non lists
			(member(target(prolog),TaxlogTerms) -> semantics2prolog(TT,_,PrologTerm) ; taxlog2prolog(TT,_,PrologTerm))
			), ExpandedTerms) 
		)
	; ExpandedTerms = []).  
	%print_message(informational,"File: ~w"-[File]),
	%print_message(informational,"expanded LE to ~w"-[ExpandedTerms]).
% This at the end, as it activates the term expansion (no harm done otherwise, just some performance..):
%user:term_expansion(T,NT) :- taxlog2prolog(T,_,NT).
%user:term_expansion(T,NT) :- (member(target(prolog),T) -> semantics2prolog(T,_,NT) ; taxlog2prolog(T,_,NT)).
