:- module('012+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *an obligation* is that *a description*,
    *a person* gave notice to *an other person* at *a date* that *a message*, 
    *a borrower* has *an obligation*,
    *a borrower* cured on *a date* the failure of *an obligation*,
    *a date* is on or before *an other date*,

the event predicates are:
    *a lender* gives notice to *a borrower* at *a date D1* that *an event*,
    *a person* pays *an amount* to *an other person* on *a date*,
    it is the end of *a date*,
    *a date* elapsed, 
    
the fluents are:
    *a lender* gave notice to *a borrower* at *a date D1* that *an event*,
    the borrower defaulted on *a date*,
    *a person* paid *an amount* to *an other person* on *a date*,
    *a borrower* failed on *a date* to fulfil *an obligation*,        

the knowledge base obligation includes:
    
A date D1 is on or before a date D2
    if D2 is a X days after D1
    and X >= 0.    

it becomes the case that
   a person paid an amount to an other person on a date
when the person pays the amount to the other person on the date.

it becomes the case that
   the borrower failed on a date to fulfil an obligation
when the date elapsed
if the obligation is that the borrower pays an amount to the lender on the date
and it is not the case that
   the borrower paid the amount to the lender on the date.
   
it becomes the case that
   a lender gave notice to a borrower at a date that an event
when the lender gives notice to the borrower at the date that the event.   

it becomes the case that
   the borrower defaulted on a date D3
when D3 elapsed
if the borrower has an obligation
and the borrower failed on a date D0 to fulfil the obligation
and the lender gave notice to the borrower at a date D1 that the borrower failed on D0 to fulfil the obligation
and a limit is 2 days after D1
and the limit is on or before D3 
and it is not the case that
    the borrower cured on a date D2 the failure of the obligation
    and D2 is on or before D3.

scenario default is:
    the borrower has obligation1.
    obligation1 is that the borrower pays 525 to the lender on 2016-06-01.
    2016-06-01 elapsed.
    2016-06-06 elapsed. 
    the lender gives notice to the borrower at 2016-06-04 that the borrower failed on 2016-06-01 to fulfil obligation1, at 2016-06-03. 

query one is:
    the borrower defaulted on which date.
    
query two is:
    a lender gave notice to a borrower at a date that an event.
    
query three is:
    the borrower failed on a date to fulfil an obligation.
    
query four is:
    a person paid an amount to an other person on a date.
    
query five is:
    2016-06-01 is on or before 2016-06-05.

"). 

/** <examples>
?- answer("query one with scenario default").
?- answer("query two with scenario default").
?- answer("query three with scenario default").
?- answer("query four with scenario default").
?- answer("query five with scenario default").
*/
