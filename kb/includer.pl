:- module('includer',[]).

en("the target language is: prolog.
    
the templates are:    
	*a thing* is similar to *a thing*.    
        
the knowledge base includer includes these files:
	included.
    
scenario one is:
    the earth is similar to oranges. 
 
    
"). 



/** <examples>
?- show templates.
?- answer(one, with(one), le(E), R). 
*/
