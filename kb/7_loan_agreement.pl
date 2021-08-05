:- module('http://tests.com',[]).

en("the target language is: prolog.

the timeless predicates are:
  *an amount* is due on *a date* from *a borrower* to *a lender*,
  *a payment* is that *a borrower* pays *an amount* to *a lender*.

the time-varying predicates are:
  *a lender* is liable to litigation after *a date*,
  *a borrower* requested *an amount* on *a date*,
  *a lender* advanced *an amount* on *a date*,
  the agreement is terminated at *a date*,
  nothing at *a date*,
  *a payment* is potentially defaulted at *a date*,
  *a borrower* paid *an amount* to *a lender* on *a date*,

the event predicates are:
  it is *a date*.
  it is the end of *a date*,
  *a borrower* requests *an amount* at *a date*,
  *a lender* advances *an amount* at *a date*,
  end of the world at *a date*,
  *a borrower* pays *an amount* to *a lender* on *a date*.

the knowledge base Loan includes:

it becomes the case that
    lender is liable to litigation after 2014-06-02-00-00-00
when
    it is the end of 2014-06-02-00-00-00
if borrower requested 1000 on 2014-06-01-06-00-00
and it is not the case that
    lender advanced 1000 on a date
    and the date is before 2014-06-02-00-00-00.

it becomes the case that
    the agreement is terminated at 2014-06-01-00-00-00
when
    it is the end of 2014-06-01-00-00-00
if it is not the case that
    borrower requested 1000 on 2014-06-01-00-00-00.

it becomes the case that
    borrower requested an amount on a date
when
    borrower requests the amount at the date.
%if it is the date.

it becomes the case that
    lender advanced an amount on a date
when
    lender advances the amount at the date.
%if it is the date.

it becomes not the case that
    nothing at a date
when
    end of the world at the date.

550 is due on 2015-06-01-00-00-00 from borrower to lender.
525 is due on 2016-06-01-00-00-00 from borrower to lender.

it is illegal that
    borrower pays an amount to lender on a date
if an amount is due on a date from borrower to lender
and it is a new date
and the new date is before the date.

scenario test1 is:
    it is the end of 2014-06-01-00-00-00.
    borrower requests 1000 at 2014-06-01-06-00-00.
    lender advances 1000 at 2014-06-02-00-00-00.
    borrower pays 550 to lender on 2015-06-01-12-00-00.
    borrower pays 525 to lender on 2016-06-01-12-00-00.

scenario test2 is:
    it is 2014-08-02-00-00-00.

query 1 is:
    an amount is due on a date from borrower to lender.

query 2 is:
    lender is liable to litigation after 2014-06-02-00-00-00.

query 3 is:
    borrower requested an amount on a date.

query 4 is:
    a borrower pays an amount to a lender on a date.

query 5 is:
    a person is liable to litigation after a date.

query 6 is:
    the agreement is terminated at a date.
    
").

/** <examples>
?- answer("1 with test2").
*/