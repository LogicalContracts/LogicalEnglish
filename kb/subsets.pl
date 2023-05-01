:- module('subsets',[]).

en("the target language is: prolog.

the templates are:
	*a set* is a subset of *a set*,
	*a thing* is a set,
	*a thing* belongs to *a set*.

the knowledge base subsets includes:

a set A is a subset of a set B
	if set A is a set
    and set B is a set
    and  for all cases in which  
	a thing belongs to set A
	it is the case that 
	the thing belongs to set B.

scenario one is:
	family one is a set.
	family two is a set.
	Bob belongs to family one.
	Alice belongs to family one.
    
	Alice belongs to family two.
      
query one is:
 	which first family is a subset of which second family.
    
scenario two is:
	[Alice, Bob] is a set.
	[Alice] is a set.

	a thing belongs to a set
    if the thing is in the set.
    
query two is:
 	which set is a subset of which other set.
   
").

/** <examples>
?- answer one with one.
?- show prolog.
?- answer two with two.
*/