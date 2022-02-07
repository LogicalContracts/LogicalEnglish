:- module('indenta000+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon
% Logical English document
% above example of a prolog module name. Please adjust as convenient. 
% comments only after the en( 

the templates are:
    *a thing* goes to *a thing*. 

the knowledge base indenta000 includes:
    b goes to c 
    if b goes to d
    and
    		b goes to e
    		and e goes to c
    	or b goes to f
    		and f goes to c. 
    
% add as many as you need    
% scenario one is:
    
% add as many as you need    
query one is:
    which one goes to which two. 

").

/** <examples>
?- answer("query one").

