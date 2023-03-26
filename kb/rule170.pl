:- module('rule170',[]).

en("the target language is: prolog. 

the templates are:
    *A vehicle* is permitted to enter *a junction* at *a time*. 
    *A vehicle* have watched *a junction* before *a time*. 
    there are users crossing *a junction* at *a time*.
    there is a safe gap between *a vehicle* and any user approaching *a junction* at *a time*.

the knowledge base rule170 includes:
    A vehicle is permitted to enter a junction at a time if
    	the vehicle have watched the junction before the time
    and it is not the case that
    	there are users crossing the junction at the time
    and there is a safe gap between the vehicle and any user approaching the junction at the time. 
    
scenario one is:
    av1 have watched junction01 before 2.
    there is a safe gap between av1 and any user approaching junction01 at 2. 

query one is:
    which vehicle is permitted to enter which junction at which time. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
*/
