:- module('minicontract+http://tests.com',[]).

en("the target language is: prolog.
% Logical English document

the templates are:
    the terms of *a contract* are met,
    *a contract* is a valid contract, 
    *a schedule* of *a contract* is valid, 
    *a contract* is signed by *a provider* known as *an entity*, 
    service datetime: *a date*,
    *A schedule* is of type payment after delivery of service and before *a N* days have elapsed from service datetime,
    *A schedule* is of type payment before service delivery datetime, 
    *A schedule* is of type staggered payment schedule,
    payment after delivery of service and before time *a date*,
    payment before service delivery time *a date*.

the knowledge base minicontract includes:
    
A contract C is a valid contract if
	the contract C is signed by the service provider known as a X
	and the contract C is signed by the service recipient known as a Y
	and the terms of the contract C are met. 
    
the terms of a contract are met if
    a payment schedule of the contract is valid.

A payment schedule of a contract is valid 
	if the payment schedule is of type payment after delivery of service and before 2 days have elapsed from service datetime
	or the payment schedule is of type payment after delivery of service and before 30 days have elapsed from service datetime
	or the payment schedule is of type payment before service delivery datetime
	or the payment schedule is of type staggered payment schedule.

the payment schedule is of type payment after delivery of service and before a N days have elapsed from service datetime
    if payment after delivery of service and before time a second date
    and service datetime: a first date
    and the second date is a M days after the first date
    and M < N. 
    
the payment schedule is of type payment before service delivery datetime
    if payment before service delivery time a date.
    
Scenario one is:
    the payment schedule is of type staggered payment schedule.
    the contract one is signed by the service provider known as Logical Contracts.
    the contract one is signed by the service recipient known as Andrew N. 
     
Scenario two is:      
    payment after delivery of service and before time 2022-06-06T10:00:00. 
    service datetime: 2022-06-05T10:00:00. 
    the contract two is signed by the service provider known as Logical Contracts.
    the contract two is signed by the service recipient known as Andrew N.
     
Scenario three is:
    payment before service delivery time 2022-06-05T10:00:00. 
    the contract three is signed by the service provider known as Logical Contracts.
    the contract three is signed by the service recipient known as Andrew N.

Query one is:
    which contract is a valid contract.    
").


/** <examples>
?- answer(one, with(one), le(E), R). 
?- answer("query one with scenario one").
?- answer("query one with scenario two").
?- answer("query one with scenario three").
?- show prolog.
*/