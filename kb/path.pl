:- module('path',[]).

en("the target language is: prolog.

the templates are:
    *a vertex* edge *an other vertex*,
    *a vertex* path *an other vertex*,

the knowledge base path includes:

a vertex X path a vertex Y 
    if X edge Y.

a vertex X path a vertex Y 
    if X edge a vertex Z
    and Z path Y. 

scenario one is:
	a edge b.
    b edge c.
    c edge d.
    d edge e.
    d edge f.
      
query one is:
 	which b path a X. 
   
").

/** <examples>
?- answer one with one.
?- show prolog.
*/