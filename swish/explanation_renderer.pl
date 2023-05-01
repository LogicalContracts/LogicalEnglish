:- module(_, [term_rendering//3]). % +Term, +Vars, +Options

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).

:- register_renderer(explanation_renderer, "An explanation tree").

:- use_module('../reasoner.pl').
:- use_module('../kp_loader.pl').

term_rendering(Explanation, _Vars, _Options) --> 
	{Explanation=taxlogExplanation(Trees), is_list(Trees)}, % validate...
    !,
    {
        explanationHTML(Trees,HTML)
    },
	html( 
		div([ 'data-render'('As taxlog explanation')],[
            div([],HTML)
		]) 
	).

term_rendering(Explanation, _Vars, _Options) --> 
	{Explanation=le_Explanation(Trees), is_list(Trees)}, % validate...
    !,
    {
        explanationLEHTML(Trees,HTML)
    },
	html( 
		div([ 'data-render'('As Logical English explanation')],[
            div([],HTML)
		]) 
	).

% explanationHTML(ExpandedExplanationTree,TermerizedHTMLlist)
% works ok but not inside SWISH because of its style clobbering ways:
explanationHTML(s(G,Ref,_,_,_,C),[li(title="Rule inference step",["~w"-[NG],Navigator]),ul(CH)]) :- 
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator), explanationHTML(C,CH).
explanationHTML(u(G,Ref,_,_,_,[]),[li(title="Unknown",["~w ?"-[NG],Navigator])]) :-
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator).
%explanationHTML(unknown(at(G,K)),[li([style="color:blue",title="Unknown"],a(href=K,"~w"-[G]))]).
% explanationHTML(unknown(at(G,K)),[li([p("UNKNOWN: ~w"-[G]),p(i(K))])]).
explanationHTML(f(G,Ref,_,_,_,C),[li(title="Failed goal",[span(style="color:red","~w ~~"-[NG]),Navigator]), ul(CH)]) :- 
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator), explanationHTML(C,CH).
%explanationHTML(at(G,K),[li(style="color:green",a(href=K,"~w"-[G]))]).
%explanationHTML(at(G,K),[li([p("~w"-[G]),p(i(K))])]).
explanationHTML([C1|Cn],CH) :- explanationHTML(C1,CH1), explanationHTML(Cn,CHn), append(CH1,CHn,CH).
explanationHTML([],[]).

% explanationLEHTML(ExpandedExplanationTree,TermerizedHTMLlist)
explanationLEHTML(s(G,Ref,_,_,_,C),[li(title="Rule inference step",["It is the case that: ", b("~w"-[NG]), " as proved by", Navigator]), Because, ul(CH)]) :- 
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator), explanationLEHTML(C,CH), (CH\=[] -> Because = 'because'; Because=''). 
explanationLEHTML(u(G,Ref,_,_,_,[]),[li(title="Unknown",["~w ?"-[NG],Navigator])]) :-
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator).
%explanationHTML(unknown(at(G,K)),[li([style="color:blue",title="Unknown"],a(href=K,"~w"-[G]))]).
% explanationHTML(unknown(at(G,K)),[li([p("UNKNOWN: ~w"-[G]),p(i(K))])]).
explanationLEHTML(f(G,Ref,_,_,_,C),[li(title="Failed goal",[span(style="color:red","There is no evidence that: ~w ~~"-[NG]),Navigator]), Because, ul(CH)]) :- 
    niceModule(G,NG),
    clauseNavigator(Ref,Navigator), explanationLEHTML(C,CH), 
    %print_message(informational, "G vs C: ~w .. ~w ... ~w"-[G, C, CH]), 
    (CH\=[] -> (C=[s(_,_,_,_,_,[])] -> Because = 'although' ;  Because = 'because'); Because=''). % this must be filtered out before
%explanationHTML(at(G,K),[li(style="color:green",a(href=K,"~w"-[G]))]).
%explanationHTML(at(G,K),[li([p("~w"-[G]),p(i(K))])]).
explanationLEHTML([C1|Cn],CH) :- explanationLEHTML(C1,CH1), explanationLEHTML(Cn,CHn), append(CH1,CHn,CH).
explanationLEHTML([],[]).