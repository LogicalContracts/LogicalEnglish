:- module('loan obligation+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *an obligation* is that *a description*, 
    *a lender* gives notice to *a borrower* on *a date* that *a message*,
    *a person* must fulfil *an obligation*,
    *a borrower* cures on *a date* the failure of *an obligation*,
    *a borrower* fails on *a date* to fulfil *an obligation*,
    *a borrower* pays *an amount* to *a lender* on *a date*,
    *a borrower* defaults on *a date*,
    the loan is accelerated on *a date*,
    *a date* is on or before *a later date*,
    *a date* is the latest of *a date one* and *a date two*.

the knowledge base loan obligation includes:

the borrower must fulfil obligation1.
obligation1 is that the borrower pays 550 to the lender on 2015-06-01.
the borrower must fulfil obligation2.
obligation2 is that the borrower pays 525 to the lender on 2016-06-01.

the borrower defaults on a date D3
    if the borrower must fulfil an obligation 
    and the borrower fails on a date D0 to fulfil the obligation 
    and the lender gives notice to the borrower on a date D1 that the borrower fails on D0 to fulfil the obligation 
    and D3 is 2 days after D1
    and it is not the case that
    	the borrower cures on a date D2 the failure of the obligation 
    	and D2 is on or before D3.


the borrower fails on a date to fulfil an obligation
	if the obligation is that the borrower pays an amount to the lender on the date
	and it is not the case that
		the borrower pays the amount to the lender on the date.

the borrower cures on a date D the failure of an obligation  
	if the obligation is that the borrower pays an amount to the lender on an other date
	and the borrower pays the amount to the lender on a date D1
    and the borrower gives notice to the lender on a date D2 that the borrower pays the amount to the lender on D1
    and D is the latest of D1 and D2.

a day is on or before the day.
2016-06-02 is on or before 2016-06-04.
2016-06-04 is the latest of 2016-06-02 and 2016-06-04.

scenario one is:
  the lender gives notice to the borrower on 2016-06-02 that the borrower fails on 2016-06-01 to fulfil obligation2.
  the borrower pays 525 to the lender on 2016-06-02.
  the borrower gives notice to the lender on 2016-06-04 that the borrower pays 525 to the lender on 2016-06-02. 

scenario two is:
  the lender gives notice to the borrower on 2016-06-02 that the borrower fails on 2016-06-01 to fulfil obligation2.
  the borrower pays 525 to the lender on 2016-06-02.
  the borrower gives notice to the lender on 2016-06-05 that the borrower pays 525 to the lender on 2016-06-02. 

query one is:
     which person defaults on which date.

query two is:
	which first person gives notice to which second person on which date that which message.   
    
query three is:
    the loan is accelerated on which date.
    
query four is:
    the borrower cures on which date D2 the failure of which obligation. 
    
query five is:
    it is not the case that 
    the borrower cures on a date D2 the failure of obligation2.
   
").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
*/
