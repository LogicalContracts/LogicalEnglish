:- module('after_problem+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

% uncomment to use
% the meta predicates are:
% <fill as desired>

the templates are:
*a cause* applies.
*a cause* is just. 

the knowledge base after_problem includes:

a cause applies if
    the cause is just. 
    
% add as many as you need    
scenario one is:
    climate change is just. 
    climate change after 2022 is just. 

% add as many as you need    
query one is:
    which cause applies. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
?- answer(one, with(one), le(E), R). 
*/
