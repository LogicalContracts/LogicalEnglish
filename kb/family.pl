:- module('family+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
*a person* is a parent of *a person*.
*a person* is a grandparent of *a person*.

the knowledge base family includes:
a person is a grandparent of a third person
if the person is a parent of an other person
and the other person is a parent of the third person.
    
% add as many as you need    
scenario one is:
john is a parent of mary.
mary is a parent of john.

% add as many as you need    
query one is:
which person is a grandparent of which other person.
").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
?- answer one with one.
*/
