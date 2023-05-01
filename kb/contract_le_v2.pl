:- module('contract_le_v2+http://tests.com',[]).

en("the target language is: prolog.
the meta predicates are:
*a fluent* holds at *a time*,
*a fluent* begins after *a time*,
*a fluent* ends at *a time*,

the templates are:

*an agent* is of type *type*,
*a time* is on or before *a time*,
*a time* is *a X* days after *a time*,
*an agent* has a debt with *an agent* for *an amount* through *a contract*,
*a contract* sends *an amount* to *an agent* on *a time* for *an agent*,
*an agent* accepts the loan  with *an agent* for *an amount* on *a time* through *a contract*,
*an agent* pays *an amount* to *an agent* on *a time* for *an agent*,
*an agent* offers a loan to *an agent* for *an amount* in *a contract* on *a time*,
*an agent* asks for *an amount* from *an agent*,
*an agent* gives the collateral to *a contract* on *a time*,
*a contract* for *an amount* is waiting for acceptance by *an agent* from *an agent*,
*a time* is *a number* days afters *a time*,
*a date* is within 30 days from *a date*.

 
the knowledge base includes:    
a fluent holds at a time T2
    if the fluent begins after a time TB
    and TB is before T2
    and it is not the case that
      the fluent ends at a time TE
      and TE is on or before T2.
    
a time T1 is on or before a time T2
    if T1 < T2.
    
a time T1 is a X days afters a time T2
	if a T = T1 - T2.

an agentB has a debt with an agentL for an amount through a contract begins after a time TB
	if the agentL is of type lender
	and the contract sends the amount to the agentB on the time TB for the agentL.

an agentB has a debt with an agentL for an amount through a contract ends at a time TE
    if the agentB pays the amount to the contract on TE for the agentL
    and the contract sends the amount to the agentB on a time TB for the agentL
    and TE is within 30 days from TB.
    
a contract sends an amount to an agentB on a time TB for an agentL
    if the contract for the amount is waiting for acceptance by the agentL from the agentB ends at TB.
    
a contract for an amount is waiting for acceptance by an agentL from an agentB begins after a time TB
    if the agentB offers a loan to the agentL for the amount in the contract on TB.

an agentB offers a loan to an agentL for an amount in a contract on a time TB
	if the agentL is of type lender
	and the agentB is of type borrower
    and the agentB asks for the amount from the agentL
    and the agentB gives the collateral to the contract on TB.
    
a contract for an amount is waiting for acceptance by an agentL from an agentB ends at a time TE
    if the agentL accepts the loan with the agentB for the amount on TE through the contract.
    
scenario one is:
Marco Billi is of type borrower.
Banca Intesa is of type lender.
Contratto1 is of type contract.
% Banca Intesa pays 100 to Contratto1 on 2021-12-13 for Marco Billi.
% Contratto1 sends 100 to Marco Billi on 2021-12-14 for Banca Intesa.
Banca Intesa accepts the loan with Marco Billi for 100 on 2021-12-12 through Contratto1.
Marco Billi gives the collateral to Contratto1 on 2021-11-11.
Marco Billi asks for 100 from Banca Intesa.
Marco Billi pays 100 to Contratto1 on 2022-01-10 for Banca Intesa.
2022-01-10 is within 30 days from 2021-12-12.

    
query one is:
which fluent holds at 2021-12-01.
    
query two is:
which fluent holds at 2022-01-01.

query three is:
Marco Billi has a debt with Banca Intesa for 100 through Contratto1 holds at 2022-01-01.

query four is:
Marco Billi has a debt with Banca Intesa for 100 through Contratto1 ends at which date.
    



").

/** <examples>
?- answer("query one with scenario one").
?- answer("query two with scenario one").
?- answer("query three with scenario one").
?- answer(one, with(one), le(E), R).
*/
