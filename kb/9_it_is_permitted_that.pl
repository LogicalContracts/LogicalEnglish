- module('six+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    
    it is permitted that *a designation of an event by an action*,
    *a party* designates by *an action* at *a time* that *an event*,
    *an event* happens at *a time*,
    *a party* performs *an action* at *a time*.
    *a time* is on or before *a time*.
    
the knowledge base metaevent includes:

An event happens at a time T2
    if it is permitted that a party designates by an action at a time T1 that the event happens at T2
    and the party designates by the action at T1 that the event happens at T2.
       
scenario test is:
    it is permitted that bob designates by email at a time T1 that a meeting happens at a time T2
    if T1 is on or before T2.
    
    bob designates by email at Monday that this meeting happens at Friday.
    Monday is on or before Friday.

query one is:
    which event happens at which time.
   
").


/** <examples>
?- answer("query one with scenario test").
*/