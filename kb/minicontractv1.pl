:- module('minicontractv1+http://tests.com',[]).

en("the target language is: prolog.
% Logical English document

the templates are:
	The terms of the contract are met,
    *a contract* is a valid contract, 
    *a contract* is signed by the service provider, 
    *a contract* is also signed by the service recipient,
    the service is delivered before *a date*,
    the service recipient maintains all communication within the confines of *a domain*,
    the service recipient delivers requested information before *a date*. 

the knowledge base minicontractv1 includes:
A contract is a valid contract if
the contract is signed by the service provider
and the contract is also signed by the service recipient.

The terms of the contract are met if
the service is delivered before 2022-06-05T10:00:00
and the service recipient maintains all communication within the confines of domain
and the service recipient delivers requested information before 2022-06-01T10:00:00.
  
scenario one is:
	the service is delivered before 2022-06-05T10:00:00.
    the service recipient maintains all communication within the confines of domain.
    the service recipient delivers requested information before 2022-06-01T10:00:00.
    the contract is signed by the service provider.
    the contract is also signed by the service recipient.
    
query one is:
	which contract is a valid contract. 
    
query two is:
    The terms of the contract are met. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query two with scenario one").
?- show prolog. 
*/