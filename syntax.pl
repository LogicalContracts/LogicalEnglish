:- module(_,[
    op(1200,xfx,user:(then)),
    op(1200,xfx,user:(must)),
    op(1185,fx,user:(if)),
    op(1190,xfx,user:(if)),
    op(1100,xfy,user:else),
    op(835,xfy,user:and),
    op(840,xfy,user:or),
    op(820,fx,user:not),
    op(700,xfx,user:in),
    op(600,xfx,user:on),
    op(600,xfy,at)
    ]).
% For later definition in SWISH backend:

% For quick and dirty SWISH experimentation:
% :- op(1200,xfx,(then)).
% :- op(1200,xfx,(must)).
% :- op(1185,fx,(if)).
% :- op(1190,xfx,(if)).
% :- op(1100,xfy,else). 
% :- op(835,xfy,and).
% :- op(840,xfy,or). 
% :- op(820,fx,not).
% :- op(700,xfx,in),
% :- op(600,xfx,on). % explicit datetime
% :- op(600,xfy,at). % external predicate, meaning defined in URL

user:term_expansion(if(H,B),(H:-B)).
