:- module('isda permission+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
	It is permitted that *an eventuality*,
   	*a party* gives notice to *a party* at *a time* that *a message*,
	*a party* designates at *a time* that *an eventuality*,
	the Schedule specifies that *an specification*,
	*an Event* of Default occurs with respect to *a party* at *a time*,
	*an event* occurs at *a time*,
	*an Event* is continuing at *a time*,
	*a date* is on or before *another date*,
	Automatic Early Termination applies to *a party* for *an Event* of Default,
	*a time one* and *a time two* are at most *a number* days apart, 

the knowledge base isda permission includes:
It is permitted that a party designates at a time T2 that Early Termination in respect of all outstanding Transactions occurs at a time T3
if  an Event of Default occurs with respect to an other party at a time T1
and the Event is continuing at T2
and the party gives notice to the other party at T2 that the Event occurs at T1
and T2 is on or before T3 
and T3 and T2 are at most 20 days apart
and it is not the case that 
	the Schedule specifies that Automatic Early Termination applies to the other party for the Event of Default.
	the Schedule specifies that Automatic Early Termination applies to the other party for the Event of Default. 

scenario one is:
	event001 of Default occurs with respect to Bob at Monday.
	event001 is continuing at Tuesday.
	Alice gives notice to Bob at Tuesday that event001 occurs at Monday.
	Tuesday is on or before Friday.
	Friday and Tuesday are at most 20 days apart. 
    
query one is:
It is permitted that which party designates at which first time that which event occurs at which second time.
   
").