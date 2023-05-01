:- module('isda-permission-corrected+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
	it is permitted that *an eventuality*,   
	*a party* designates that *an eventuality*,  
	*an event* occurs at *a time*. 
	*an event* of Default occurs with respect to *a party* at *a time*,  
	*an event* is continuing at *a time*, 
	*a party* gives notice to *a party* at *a time* that *a message*,   
   *a date* is on or before *another date*, 
	*a second time* is not more than *a number* days after *a first time*, 
	the Schedule specifies that *a specification*, 
    Automatic Early Termination applies to *a party* for *an event* of Default.


the knowledge base isda-permission-corrected includes:

it is permitted that a party designates that Early Termination in respect of all outstanding Transactions occurs at a time T3
if  an Event of Default occurs with respect to an other party at a time T1
and the Event is continuing at a time T2
and the party gives notice to the other party at T2 that the Event occurs at T1
and T2 is on or before T3 
and T3 is not more than 20 days after T2
and it is not the case that 
    the Schedule specifies that Automatic Early Termination applies to the other party for the Event of Default. 

scenario one is:
	event001 of Default occurs with respect to Bob at Monday.  
	event001 is continuing at Tuesday.  
	Alice gives notice to Bob at Tuesday that event001 occurs at Monday.  
	Tuesday is on or before Friday.  	
	Friday is not more than 20 days after Tuesday. 

    
query one is:
it is permitted that which party designates that which event occurs at which second time.

query two is:
    it is not the case that 
    the Schedule specifies that Automatic Early Termination applies to  Bob for e001 of Default. 

query three is:
    
    the Schedule specifies that Automatic Early Termination applies to a party for an Event of Default. 

").