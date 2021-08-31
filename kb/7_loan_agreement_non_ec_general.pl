:- module('http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *a person* has *an obligation* that *a requirement*,
    it is permitted that *a party* designates by *an action* at *a time T1* that *an event*,
    *a borrower* cures the failure of *an obligation* on *a day* that *a requirement*,
    *a lender* notifies *a borrower* on *a date* that *a message*,
    *a borrower* fails to fulfil *an obligation* that *a requirement*,
    *an event* occurs at *a time*,
    *a borrower* pays *an amount* to *a lender* on *a date*,
    *a party* performs *an action* at *a time*,
    *a borrower* defaults on *a date*,
    *a date* is on or before *a later date*.

the knowledge base obligation includes:

the borrower has an obligation that the borrower pays 550 to the lender on 2015-06-01.
the borrower has an obligation that the borrower pays 525 to the lender on 2016-06-01.

the borrower defaults on a day D3
  if the borrower has an obligation that an eventuality
  and the borrower fails to fulfil the obligation that the eventuality
  and the lender notifies the borrower on a day D1 that the borrower fails to fulfil the obligation that the eventuality
  and D3 is 2 days after D1
  and it is not the case that
	   the borrower cures the failure of the obligation on a day D2 that the eventuality
	   and D2 is on or before D3.

the borrower fails to fulfil an obligation that the borrower pays an amount to the lender on a date
    if it is not the case that
          the borrower pays the amount to the lender on the date.

the borrower cures the failure of an obligation on a first date that the borrower pays an amount to the lender on a second date
  if the borrower pays the amount to the lender on the first date.

A date is on or before a second date
    if second is a X days after the date
    and X >= 0.

scenario test is:
  the lender notifies the borrower on 2016-06-02 that the borrower fails to fulfil an obligation that the borrower pays 525 to the lender on 2016-06-01.
  the borrower pays 525 to the lender on 2016-06-02.

query one is:
     which person defaults on which day.
   
query three is:
	the borrower cures the failure of an obligation on a first date that the borrower pays an amount to the lender on a second date.
   
").

/** <examples>
?- answer("one with test").
?- answer("three with test").
?- answer("query three with scenario test").
*/