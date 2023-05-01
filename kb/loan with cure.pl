:- module('loan with cure+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *an obligation* is that *a description*, 
    *a lender* notifies *a borrower* on *a date* that *a message*,
    *a person* has *an obligation*,
    *a borrower* cures the failure of *an obligation* on or before *a date*,
    *a borrower* fails on *a date* to fulfil *an obligation*,
    *a borrower* pays *an amount* to *a lender* on *a date*,
    *a borrower* defaults on *a date*,
    the loan is accelerated on *a date*,
    *a date* is on or before *a later date*.

the knowledge base loan with cure includes:

A date D1 is on or before a date D2
    if D2 is a X days after D1
    and X >= 0.
    
the borrower has obligation1.
obligation1 is that the borrower pays 550 to the lender on 2015-06-01.

the borrower has obligation2.
obligation2 is that the borrower pays 525 to the lender on 2016-06-01.

the borrower defaults on a date D2
  	if the borrower has an obligation 
  	and the borrower fails on a date D0 to fulfil the obligation 
  	and the lender notifies the borrower on a date D1 
    	that the borrower fails on D0 to fulfil the obligation 
  	and D2 is 2 days after D1
  	and it is not the case that
		the borrower cures the failure of the obligation on or before D2.
    
the borrower cures the failure of an obligation on or before a date D3 
	if the obligation is 
    	that the borrower pays an amount to the lender on a date D0
	and the borrower pays the amount to the lender on a date D1
    and the borrower notifies the lender on a date D2
    	that the borrower pays the amount to the lender on D1
	and D1 is on or before D3
	and D2 is on or before D3.
	
the borrower fails on a date to fulfil an obligation
	if the obligation is 
    	that the borrower pays an amount to the lender on the date
	and it is not the case that
		the borrower pays the amount to the lender on the date.

scenario payment is:
  	the lender notifies the borrower on 2016-06-04 
		that the borrower fails on 2016-06-01 to fulfil obligation2.  
 	the borrower pays 525 to the lender on 2016-06-05.
    the borrower notifies the lender on 2016-06-06
    	that the borrower pays 525 to the lender on 2016-06-05.
    
query defaults is:
which person defaults on which day.

").

/** <examples>
?- show prolog.
?- answer defaults with payment.
*/