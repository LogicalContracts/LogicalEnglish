:- module('predicate_is+http://tests.com',[]).

en("the target language is: prolog. 
    
the templates are:
    *an A* is the sum of *a B* plus *a C*. 

the knowledge base includes:
    man is person. 
    
    an A is the sum of a B plus a C if A is B+C. 
   
query one is:
    which thing is which other thing. 
    
query two is:
    which amount is the sum of 1 plus 1. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
*/
