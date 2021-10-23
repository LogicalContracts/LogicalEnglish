:- module('011+http://tests.com',[]).

en("the target language is: prolog.

the templates are:

*a set* is a subset of *a set*,
*a thing* is a set,
*a thing* belongs to *a set*,
*a thing* followed by *a list* is *an other list*,
the concatenation of *a first list* then *a second list* is *a third list*.

the knowledge base subset includes:

a set A is a subset of a set B
	if set A is a set
    and set B is a set
    and  for all cases in which  
	a thing belongs to set A
	it is the case that 
	the thing belongs to set B.
 
the concatenation of [] then a list is the list.
the concatenation of [a thing|a first list] then a second list is [the thing|a third list]
    if the concatenation of the first list then the second list is the third list.

a thing followed by a list is an other list 
    if the other list has the thing as head before the list.

scenario one is:
family one is a set.
family two is a set.
Bob belongs to family one.
Alice belongs to family one.
Alice belongs to family two.
    
query one is:
family one is a subset of family two.
    
query two is:
family two is a subset of family one.
    
query three is:
which family is a subset of family one.
    
query four is:
family two is a subset of which family.
    
query five is:
 which  family is a subset of which family.
    
query 6 is:
 which list has a as head before [b, c].

query 7 is:
 which thing followed by which other thing is [b, c].
    
query 8 is:
the concatenation of which first list then which second list is [a, b, c].
   
").

/** <examples>
?- answer("query 8"). 
?- answer("query 7 with scenario one"). 
*/
