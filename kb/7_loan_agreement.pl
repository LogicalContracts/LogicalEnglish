:- module('http://tests.com',[]).

en("the target language is: prolog.

the timeless predicates are:
  *an amount* is due on *a date* from *a borrower* to *a lender*,
  *a payment* is that *a borrower* pays *an amount* to *a lender*,
  *a message* is that *a requirement* is in default,
  *a message* is that *a requirement* is remedy,
  *a thing* is different from *a second thing*,
  *a requirement* is cured,
  *a reason* is that *a person* is bankrupted by insolvency,
  *a borrower* represents-warrants *a requirement*, 
  *an amount* is the total due,

the time-varying predicates are:
  *a lender* is liable to litigation after *a date*,
  *a borrower* requested *an amount* on *a date*,
  *a lender* advanced *an amount* on *a date*,
  *an agreement* is terminated at *a date*,
  nothing at *a date*,
  *a payment* is potentially defaulted at *a date*,
  *a borrower* paid *an amount* to *a lender* on *a date*,
  *a person* notified *a message* on *a date*,
  *a requirement* is defaulted on *a date*,
  *a requirement* is remedied on *a date*,
  *a borrower* is liable to litigation at *a date*, 

the event predicates are:
  it is *a date*.
  it is the end of *a date*,
  *a borrower* requests *an amount* at *a date*,
  *a lender* advances *an amount* at *a date*,
  end of the world at *a date*,
  *a borrower* pays *an amount* to *a lender* on *a date*,
  *a person* notifies *a message* on *a date*,
  *a requirement* remedies on *a date*,
  *a borrower* goes bankrupt at *a date*,
  *a requirement* is proved untrue at *a previous time*.

the knowledge base Loan includes:

% Article 1
    
it becomes the case that 
    lender is liable to litigation after 2014-06-02
when
    it is the end of 2014-06-02
if borrower requested 1000 on 2014-06-01
and it is not the case that
    lender advanced 1000 on a date
    and the date is before 2014-06-02.

it becomes the case that
    the agreement is terminated at 2014-06-01
when
    it is the end of 2014-06-01T00:00:00
if it is not the case that
    borrower requested 1000 on 2014-06-01.

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

% testing rules    
it becomes not the case that
    nothing at a date
when
    end of the world at the date.

% Articles 2 and 10
    
550 is due on 2015-06-01T00:00:00 from borrower to lender.
525 is due on 2016-06-01T00:00:00 from borrower to lender.

it becomes the case that
    a payment is potentially defaulted at a later date
when
    it is the end of a date
if the payment is that borrower pays an amount to lender
and the amount is due on the date from borrower to lender
and it is not the case that
    borrower paid the amount to lender on a third date
    and the date is before the third date.
    
it becomes the case that
    borrower paid an amount to lender on a date
when 
    borrower pays the amount to lender on a payment date
if it is the payment date. 
    
it is illegal that
    borrower pays an amount to lender on a first date
if an amount is due on a second date from borrower to lender
and it is the first date
and the first date is before second date.

% Article 5    

It becomes the case that
    a person notified a message on a date
when
    the person notifies the message on a previous date
if it is the date
and the previous date is before the date. 
     
It becomes the case that
    a requirement is defaulted on a date
when
    it is the end of a first date
if the requirement is potentially defaulted at the first date
and lender notified a message on a second date  
and the message is that the requirement is in default
and the first date is before the second date
and it is not the case that
    a second requirement is defaulted on a third date
    and the second requirement is different from the requirement
and it is not the case that
    the requirement is cured.              
    
A requirement is cured 
    if the requirement is remedied on a date
    and borrower notified a message on a second date
    and the message is that the requirement is remedy
    and the second date is before the date. 

It becomes the case that
    a reparation is remedied on a date
when
    borrower pays an amount to lender on a first date
if it is the date
and a requirement is potentially defaulted at the date
and the requirement is that the borrower pays an amount to lender
and the reparation is that borrower pays an amount to lender
and the first date is before the date. 

It becomes the case that
    a requirement is remedied on a date
when
    the requirement remedies on a previous date.

It becomes the case that
    a default is potentially defaulted at a time
when
    borrower goes bankrupt at a previous time
if the default is that borrower is bankrupted by insolvency.
    
% Article 3

It becomes the case that
    a requirement is potentially defaulted at a time
when
    the requirement is proved untrue at a previous time
if a person represents-warrants the requirement. 
    
borrower represents-warrants warrants information. 
    
% Article 4
    
It becomes the case that
    borrower is liable to litigation at a date
when
    it is the end of a previous date
if a requirement is defaulted on the previous date
and the previous date is before 2020-06-01
and a sum is the total due
and it is not the case that
    borrower paid the sum to lender on the previous date. 

It becomes the case that
    agreement is terminated at a time
when
    borrower pays a sum to lender on a previous time
if a requirement is defaulted on the date
and it is the date
and the sum is the total due. 
    
It becomes the case that
    agreement is terminated at a time
when 
    borrower pays 525 to lender on a previous time
if it is not the case that
    a requirement is defaulted on a date
and borrower paid 550 to lender on a second date. 
    
1075 is the total due
    if it is not the case that
    	borrower paid 550 to lender on a date. 

525 is the total due
	if borrower paid 550 to lender on a date
	and it is not the case that
    	borrower paid 525 to lender on a  date. 

scenario test1 is:
    it is the end of 2014-06-01T00:00:00.
    borrower requests 1000 at 2014-06-01T06:00:00.
    lender advances 1000 at 2014-06-02T00:00:00.
    borrower pays 550 to lender on 2015-06-01T12:00:00.
    borrower pays 525 to lender on 2016-06-01T12:00:00.

scenario test2 is:
    it is 2014-08-02T00:00:00.

query 1 is:
    an amount is due on a date from borrower to lender.

query 2 is:
    lender is liable to litigation after 2014-06-02T00:00:00.

query 3 is:
    borrower requested an amount on a date.

query 4 is:
    a borrower pays an amount to a lender on a date.

query 5 is:
    a person is liable to litigation after a date.

query 6 is:
    the agreement is terminated at a date.
    
query 7 is:
    borrower paid 525 to lender on a date.
   
").

/** <examples>
?- answer("1 with test2").
?- is_it_illegal("borrower pays an amount to lender on a date", test2). 
*/