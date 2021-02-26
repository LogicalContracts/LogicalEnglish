:- module(_, [term_rendering//3]). % +Term, +Vars, +Options

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).

:- register_renderer(unknowns_renderer, "A list of unknown predicate calls").

:- use_module('../reasoner.pl').
:- use_module('../kp_loader.pl').

term_rendering(Unknowns, _Vars, _Options) --> 
	{Unknowns=unknowns(U), is_list(U)}, % validate...
    !,
    {
        unknownsHTML(U,HTML)
    },
	html( 
		div([ style('display:inline-block'), 'data-render'('As taxlog unknown predicates')],[
            div([],HTML)
		]) 
	).


unknownsHTML(U,table(class('render-table'),[thead(tr(class(hrow),[th("Unknown Predicate"),th("Knowledge Page")])),tbody(Rows)])) :- 
    unknownsRows(U,Rows).

unknownsRows([at(G,KP)/c(Cref)|U], [tr([td(a(AnchorPropery,"~w"-[G])),td("~w"-[KP])])|Rows]) :- 
    clauseNavigator(Cref,_,AnchorPropery),
    unknownsRows(U,Rows).
unknownsRows([],[]).


