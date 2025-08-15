:- module('new_loan_with_cure',[]).

en("the target language is: prolog.

the templates are:
   *a person* notifies *an other person* on *a date* that *a message*,
   *a person* has the obligation that *an eventuality*,
   the borrower cures on or before *a date* the failure to satisfy the obligation that *an eventuality*,
   the borrower fails to satisfy the obligation that *an eventuality*,
   the borrower pays *an amount* to the lender on *a date*,
   *a person* defaults on *a date*,
   the loan is accelerated on *a date*,
   *a date* is on or before *a date*.

the knowledge base new loan with cure includes:

A date D1 is on or before a date D2
    if D2 is a X days after D1
    and X >= 0.

the borrower has the obligation that the borrower pays 550 to the lender on 2015-06-01.

the borrower has the obligation that the borrower pays 525 to the lender on 2016-06-01.

the borrower defaults on a date D2
  	if the borrower has the obligation that an eventuality
  	and the borrower fails to satisfy the obligation that the eventuality 
  	and the lender notifies the borrower on a date D1 that the borrower fails to satisfy the obligation that the eventuality
  	and D2 is 2 days after D1
  	and it is not the case that
		the borrower cures on or before D2 the failure to satisfy the obligation that the eventuality.

the borrower cures on or before a date D2  the failure to satisfy the obligation that 
    the borrower pays an amount to the lender on a date 
	if the borrower pays the amount to the lender on a date D0
	and the borrower notifies the lender on a date D1 that the borrower pays the amount to the lender on D0
	and D0 is on or before D2
	and D1 is on or before D2.

the borrower fails to satisfy the obligation that 
    the borrower pays an amount to the lender on a date
	if it is not the case that
		the borrower pays the amount to the lender on the date.

scenario payment is:
  	the lender notifies the borrower on 2016-06-04 
		that the borrower fails to satisfy the obligation 
    		that the borrower pays 525 to the lender on 2016-06-01.
 	the borrower pays 525 to the lender on 2016-06-05.
    the borrower notifies the lender on 2016-06-06
    	that the borrower pays 525 to the lender on 2016-06-05.

query defaults is:
	which person defaults on which day.

").

/** <examples>
?- answer defaults with payment. 
?- answer(defaults,with(payment), le(E), R).
?- show prolog. 
*/
