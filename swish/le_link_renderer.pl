:- module(_, [term_rendering//3]). % +Term, +Vars, +Options

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).

:- register_renderer(le_link_renderer, "A navigation link to a Logical English page").

% Just a form containing the HTML in a hidden field, to be posted to the "external" rendering page

term_rendering(LE, _Vars, _Options) --> 
	{LE=logicalEnglish(HTML), is_list(HTML), mylog(html/HTML)}, 
    !,
    {phrase(html(HTML), Tokens), with_output_to( string(Text), print_html(current_output, Tokens) )},
	html( 
		div(
            [ style('display:inline-block'), 'data-render'('As link to Logical English page')],
            [
                form( [method('POST'), action(location_by_id(handle_le)), target('_blank'), enctype('multipart/form-data')],[
                    input([type=hidden,name=html,value=Text]),
                    input([type(submit), title('Please click me to open a new window'), value('Show Logical English')])
                ])
            ]
        )
	).

