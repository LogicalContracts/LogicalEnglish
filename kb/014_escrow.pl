:- module('014+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
  *an agent* has obtained *a fee*.

the event predicates are:
	*a payer* sets up *an escrow* for *an agent* to pay *an amount* to *a payee* and deduce *a fee*.
	*an agent* pays out *an amount* of *an escrow* to *a payee*.

the fluents are:
	*a payer* has *an escrow* for *an agent* to pay *an amount* to *a payee* and deduce *a fee*.
	*an escrow* of *an amount* has been paid out to *a payee*.

the knowledge base escrow includes:

it becomes the case that
	a payer has an escrow for an agent to pay an amount to a payee and deduce a fee
when
	the payer sets up the escrow for the agent to pay the amount to the payee and deduce the fee.

it becomes the case that
	an escrow of an amount has been paid out to a payee
when
	an agent pays out the amount of the escrow to the payee
if a payer has the escrow for the agent to pay a sum to the payee and deduce a fee
and the agent has obtained the fee
and the amount is the sum - the fee.

it becomes not the case that
	a payer has an escrow for an agent to pay a sum to a payee and deduce a fee
when
	the agent pays out an amount of the escrow to the payee
if the payer has the escrow for the agent to pay the sum to the payee and deduce the fee
and the agent has obtained the fee
and the amount is the sum - the fee.

scenario one is:
  the payer sets up the escrow for the agent to pay 500 to the payee and deduce 5.
  the agent has obtained a fee.
    
scenario two is:
  the payer sets up the escrow for the agent to pay 500 to the payee and deduce 5.
  the agent has obtained each fee.
  the agent pays out 495 of the escrow to the payee.
   
scenario three is:
  the payer sets up the escrow for the agent to pay 500 to the payee and deduce 5, at 2021-10-15.
  the agent has obtained each fee.
  the agent pays out 495 of the escrow to the payee, at 2021-10-17.

query one is:
    a payer has an escrow for an agent to pay an amount to a payee and deduce a fee.

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query one").
*/
