:- module(_,[
    op(1200,xfx,user:(then)),
    op(1200,xfx,user:(must)),
    op(1185,fx,user:(if)),
    op(1190,xfx,user:(if)),
    op(1100,xfy,user:else),
    op(1000,xfy,user:and), % same as ,
    op(1050,xfy,user:or), % same as ;
    op(900,fx,user:not), % same as \+
    op(700,xfx,user:in),
    op(600,xfx,user:on),
    op(995,xfx,user:at), % note vs. negation...compatible with LPS
    and/2
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

and(_,_) :- throw(use_the_interpreter). % to avoid "undefined predicate" messages in editor
%TODO: add other connectives

user:term_expansion(if(H,B),(H:-B)).
