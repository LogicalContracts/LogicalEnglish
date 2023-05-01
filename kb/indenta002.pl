:- module('indenta002+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon
% Logical English document
% above example of a prolog module name. Please adjust as convenient. 
% comments only after the en( 

the templates are:
    *a thing* goes to *a thing*. 

the knowledge base indenta002 includes:
    b goes to c 
    if b goes to d
    and     d goes to e
    		and e goes to c
		or d goes to f 
            and f goes to c. 
    
    b goes to d.
    d goes to e.
    e goes to c. 
    
% add as many as you need    
% scenario one is:
    
% add as many as you need    
query one is:
    b goes to c. 

").

/** <examples>
?- answer("query one").
*/
