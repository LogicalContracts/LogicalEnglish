:- module('impossible ancestor+http://tests.com',[]).

en("the target language is: prolog. 

the templates are:
    
*a person* is a parent of *a person*.
*a person* is a grandparent of *a person*.
*a person* is an ancestor of *a person*.
*a person* is impossible.

the knowledge base impossible ancestor includes:
    
a person is a grandparent of an other person
	if the person is a parent of a third person
	and the third person is a parent of the other person.
    
a person is an ancestor of an other person
	if the person is a parent of the other person.
    
a person is an ancestor of an other person
	if the person is a parent of a third person
	and the third person is an ancestor of the other person.
    
scenario one is:
john is a parent of mary.
mary is a parent of alice.
    
query one is:
which person is a grandparent of which other person.
    
scenario two is:
john is a parent of mary.
mary is a parent of john.
    
a person is impossible 
    if the person is an ancestor of the person.

query two is:
which person is an ancestor of which other person.
    
query three is:
which person is impossible.
    
").

/** <examples>
?- answer one with one.
?- answer two with two.
?- show prolog.
*/
