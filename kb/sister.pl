:- module('sister',[]).

en("the target language is: prolog. 
    
the templates are:
    *a person* is a sister of *a person*.
    *a person* is a parent of *a person*. 

the ontology is:
    Mary is a female. 
    Bob is a parent of Mary.
    Bob is a parent of Alice. 
    
the knowledge base sister includes:
    a person X is a sister of a person Y
    	if a person Z is a parent of X
    	and Z is a parent of Y
    	and X is a female
    	and it is not the case that
    		X is Y. 
   
query sister is:
    which person is a sister of which other person. 

  "). 


/** <examples>
?- answer("query sister").
?- show prolog.
?- answer(sister, with(noscenario), le(E), R). 
*/