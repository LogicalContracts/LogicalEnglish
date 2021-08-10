:- module('http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *a fluents* holds at *a time*,
    *an event* happens at *a time*,
    *an event* initiates *a fluent* at *a time*,
    *an event* terminates *a fluent* at *a time*,
    *a fluent* is interrupted between *a first time* and *a second time*, 

the knowledge base EC includes:
	a fluent holds at a T
    	if an event happens at a T1
    	and the event initiates the fluent at T1
    	and T1 =< T
    	and it is not the case that
    		the fluent is interrupted between T1 and T. 
    
    a fluent is interrupted between a T1 and a T2
    	if a second event happens at a T3
    	and the second event terminates the fluent at T3
    	and T1 =< T3
    	and T3 =< T2.

 	switch up initiates light at a time.
    switch down initiates dark at a time.
  	switch up terminates dark at a time.
    switch down terminates light at a time.

scenario switching is:
    switch up happens at 1.
    switch down happens at 4.

query one is:
    which fluent holds at 3
    and which other fluent holds at 5.
").

/** <examples>
?- answer("one with switching").
*/