% Copyright Miguel Calejo, 2019-2020; open source, licensed with 3-clause BSD
% SWISH renderer for dependency trees using https://allenai.github.io/hierplane/
:- module(hierplane,
	  [ term_rendering//3 % +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).

:- register_renderer(hierplane, "Term in expandable hierachical boxes").

% Javascript and CSS Preloaded below
term_rendering(hierplane(Tree), _Vars, _Options) --> 
	{Tree=_}, % validate ? beware: if called from SWISH's top level, Tree is bound to 'Tree'...
	{gensym(hierplane,ID)},
	html(
		div([ style('display:inline-block'), 'data-render'('As hierplane')],[
		div(id(ID), []),
		\js_script({|javascript(Tree,ID)||
			hierplane.renderTree(Tree, {target:'#'+ID, theme: 'light'}) ;
		|})
	])).

:- multifile user:extra_swish_resource/1. % for running with the LPS SWISH environment
user:extra_swish_resource( link([ type('text/css'),rel('stylesheet'),href('//unpkg.com/hierplane/dist/static/hierplane.min.css') ]) ).
user:extra_swish_resource( script([src('//unpkg.com/hierplane/dist/static/hierplane.min.js')],[]) ).
