:- module('014+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
  *an agent* has payed itself *a fee*.

the event predicates are:
	*a payer* sets up *an escrow* for *an agent* to pay *an amount* to *a payee* and deduct *a fee*.
	*an agent* pays out *an amount* of *an escrow* to *a payee*.
	*an agent* pays back *an amount* of *an escrow* to *a payer*.

the fluents are:
	*a payer* has *an escrow* for *an agent* to pay *an amount* to *a payee* and deduct *a fee*.
	*an escrow* of *an amount* has been paid out to *a payee*.

the knowledge base escrow includes:

it becomes the case that
	a payer has an escrow for an agent to pay an amount to a payee and deduct a fee
when
	the payer sets up the escrow for the agent to pay the amount to the payee and deduct the fee.

it becomes the case that
	an escrow of an amount has been paid out to a payee
when
	an agent pays out the amount of the escrow to the payee
if a payer has the escrow for the agent to pay a sum to the payee and deduct a fee
and the agent has payed itself the fee
and the amount is the sum - the fee.

it becomes not the case that
	a payer has an escrow for an agent to pay a sum to a payee and deduct a fee
when
	the agent pays out an amount of the escrow to the payee
if the payer has the escrow for the agent to pay the sum to the payee and deduct the fee
and the agent has payed itself the fee
and the amount is the sum - the fee.

it becomes not the case that
	a payer has an escrow for an agent to pay a sum to a payee and deduct a fee
when
	the agent pays back an amount of the escrow to the payer
if the payer has the escrow for the agent to pay the sum to the payee and deduct the fee
and the agent has payed itself the fee
and the amount is the sum - the fee.    
    
% initial act  
    
the payer sets up the escrow for the agent to pay 500 to the payee and deduct 5, at 1.     
    
scenario clause one is:
  the agent has payed itself each fee. 
  the agent pays out 495 of the escrow to the payee, at 5. 
    
scenario clause two is:
  the agent has payed itself each fee. 
  the agent pays back 495 of the escrow to the payee, at 5. 
   
query one is:
    a payer has an escrow for an agent to pay an amount to a payee and deduct a fee, at 2. 

query two is:
    a payer has an escrow for an agent to pay an amount to a payee and deduct a fee, at 8. 
    
query three is:
    which agent pays back which amount of which escrow to which payee. 

").

/** <examples>
?- answer("query one with scenario clause one").
?- answer("query one with scenario clause two").
?- answer("query three with scenario clause two").
*/