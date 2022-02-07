:- module('contract_with_ec+http://tests.com',[]).

en("the target language is: prolog.
the meta predicates are:
*a fluent* holds at *a date*,
*a fluent* begins after *a date*,
*a fluent* ends at *a date*,
    
the templates are:
*an agent* pays *an amount* to *an agent* on *a date* for *an agent*,
*a thing* is of type *a thing*,
*an agent* has a debt with *an agent*,
*an agent* exits at *a date*,
*an agent* walks,
*a date* is on or before *a date*.

the knowledge base contract_with_ec includes:

a fluent holds at a date T2
    if the fluent begins after a date TB
    and TB is before T2
    and it is not the case that
      the fluent ends at a date TE
      and TE is on or before T2.
    
%and TB is before T2
    
a date T1 is on or before a date T2
    if T2 is a X days after T1
    and X >= 0.

% an agent walks begins after a T
% if the agent exits at T.

an agent B has a debt with an agent L begins after a date TB
	if the agent L is of type lender
	and the agent B is of type borrower
	and the agent L pays an amount to a contract on TB for the agent B.
    
an agent B has a debt with an agent L ends at a date TE
    if the agent L is of type lender
    and the agent B is of type borrower	
    and the agent L pays an amount A1 to a contract on a date T0 for the agent B
    and the agent B pays an amount X to the contract on TE for the agent L
    and an amount A3 is the sum of each amount A2 such that
    	the agent B pays the amount A2 to the contract on a date T3 for the agent L
    	and T3 is before TE
    		or T3 is the same date as TE
    and the amount A1 is equal to the amount A3.

% add as many as you need    
scenario one is:
Marco Billi is of type borrower.
Banca Intesa is of type lender.
Contratto1 is of type contract.
Banca Intesa pays 100 to Contratto1 on 2021-12-13 for Marco Billi.
Marco Billi pays 30 to Contratto1 on 2022-01-15 for Banca Intesa.
Marco Billi pays 20 to Contratto1 on 2022-02-15 for Banca Intesa.
Marco Billi pays 30 to Contratto1 on 2022-03-15 for Banca Intesa.
Marco Billi pays 20 to Contratto1 on 2022-04-15 for Banca Intesa.
    
scenario giovanni is:
giovanni exits at 2021-12-10.
giovanni walks ends at 2021-12-14.
    
query one is:
which agent has a debt with which other agent ends at which date.
    
query two is:
which agent has a debt with which other agent begins after which date.
    
query three is:
giovanni walks holds at 2021-12-13.
    
query active is:
which fluent holds at 2022-03-15.
    
query ended is:
which fluent holds at 2022-05-15.

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
?- answer(four, with(giovanni), le(E), R).
*/
