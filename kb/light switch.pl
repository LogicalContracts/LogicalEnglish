:- module('light switch+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *a fluents* holds at *a time*,
    *an event* happens at *a time*,
    *an event* initiates *a fluent* at *a time*,
    *an event* terminates *a fluent* at *a time*,
    *a first time* is on or before *a second time*.

the knowledge base light switch includes:
       
	a fluent holds at a time T2
    	if an event happens at a time T1
    	and the event initiates the fluent at T1
    	and T1 is before T2
    	and it is not the case that
    		an other event happens at a time T
    		and the other event terminates the fluent at T
    		and T1 is on or before T
    		and T is before T2.

 	switch up initiates light at a time.
    switch down initiates dark at a time.
  	switch up terminates dark at a time.
    switch down terminates light at a time.
    
 	a X is on or before a Y
    	if X = Y.
   	a X is on or before a Y
    	if X is before Y.

scenario switching is:
    
   a X is before a Y
         if X is in [1,2,3,4,5,6,7]
         and Y is in [1,2,3,4,5,6,7]
         and X < Y.
    
    switch up happens at 1.
    switch down happens at 4.

query one is:
    which fluent holds at 3
    and which other fluent holds at 5.
    
query two is:
    which fluent holds at which time. 
    
query three is:
    which number is on or before 5.
").

/** <examples>
?- answer two with switching.
*/