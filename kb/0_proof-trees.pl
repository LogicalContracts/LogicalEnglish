% Logical English document

% an example of a prolog module name. Please adjust as convenient
:- module('proof+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
   predicate one *a number*,
   predicate two *a number*,
   predicate three *a number*,
   predicate four *a number*,
   predicate five *a number*, 
   solution.
 
the knowledge base proof includes:
solution
    if predicate one a first choice
    and predicate two a second choice
    and predicate three a third choice
     	or predicate five the third choice
    and predicate four a fourth choice
    	or predicate five the fourth choice.

scenario one is:
    predicate one one.
    predicate two two.
    predicate five three.
    predicate four four. 

query one is:
	solution.

").
/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one")
*/