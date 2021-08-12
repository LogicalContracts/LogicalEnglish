:- module('http://tests.com',[]).

en("the target language is: prolog.

the timeless predicates are:
  *an amount* is due on *a date* from *a borrower* to *a lender*,
  *a payment* is that *a borrower* pays *an amount* to *a lender*,
  *a message* is that *a requirement* is in default,
  *a message* is that *a requirement* is remedy,
  *a thing* is different from *a second thing*,
  *a reason* is that *a person* is bankrupted by insolvency,
  *a borrower* represents-warrants *a requirement*, 
  *an amount* is the total due,
  *a borrower* covenants *a covenance*,

the time-varying predicates are:
  *a lender* is liable to litigation,
  *a borrower* requested *an amount* on *a date*,
  *a lender* advanced *an amount* on *a date*,
  *an agreement* is terminated,
  nothing,
  *a payment* is potentially defaulted,
  *a borrower* paid *an amount* to *a lender* on *a date*,
  *a person* notified *a message* on *a date*,
  *a requirement* is defaulted on *a date*,
  *a requirement* is cured,
  *a requirement* is remedied,
  *a borrower* is liable to litigation, 

the event predicates are:
  it is *a date*.
  it is the end of *a date*,
  *a borrower* requests *an amount*,
  *a lender* advances *an amount*,
  end of the world,
  *a borrower* pays *an amount* to *a lender*,
  *a person* notifies *a message*,
  *a requirement* remedies,
  *a borrower* goes bankrupt,
  *a borrower* is insolvent, 
  *a requirement* is proved untrue,
  *a requirement* is failure to perform,

the knowledge base Loan includes:

% Article 1
    
it becomes the case that 
    lender is liable to litigation
when
    it is the end of 2014-06-02
if borrower requested 1000 on 2014-06-01
and it is not the case that
    lender advanced 1000 on a date.

it becomes the case that
    the agreement is terminated
when
    it is the end of 2014-06-01T00:00:00
if it is not the case that
    borrower requested 1000 on 2014-06-01.

it becomes the case that
    borrower requested an amount on a date
when
    borrower requests the amount
if it is the date.

it becomes the case that
    lender advanced an amount on a date
when
    lender advances the amount
if it is the date.

% Articles 2 and 10
    
550 is due on 2015-06-01T00:00:00 from borrower to lender.
525 is due on 2016-06-01T00:00:00 from borrower to lender.

it becomes the case that
    a payment is potentially defaulted
when
    it is the end of a date
if the payment is that borrower pays an amount to lender
and the amount is due on the date from borrower to lender
and it is not the case that
    borrower paid the amount to lender on a second date.
    
it becomes the case that
    borrower paid an amount to lender on a date
when 
    borrower pays the amount to lender
if it is the date. 
    
it is illegal that
    borrower pays an amount to lender
if an amount is due on a date from borrower to lender
and it is a second date
and the second date is before the date.

% Article 5    

It becomes the case that
    a person notified a message on a date
when
    the person notifies the message
if it is the date. 
     
It becomes the case that
    a requirement is defaulted on a date
when
    it is the end of the date
if the requirement is potentially defaulted
and lender notified a message on a second date  
and the message is that the requirement is in default
and it is not the case that
    a second requirement is defaulted on a third date
    and the second requirement is different from the requirement
and it is not the case that
    the requirement is cured.              
    
A requirement is cured 
    if the requirement is remedied
    and borrower notified a message on a date
    and the message is that the requirement is remedy. 

It becomes the case that
    a reparation is remedied
when
    borrower pays an amount to lender
if a requirement is potentially defaulted
and the requirement is that the borrower pays an amount to lender
and the reparation is that borrower pays an amount to lender. 

It becomes the case that
    a requirement is remedied
when
    the requirement remedies.

It becomes the case that
    a default is potentially defaulted
when
    borrower goes bankrupt
if the default is that borrower is bankrupted by insolvency.
    
% Article 3

It becomes the case that
    a requirement is potentially defaulted
when
    the requirement is proved untrue
if a person represents-warrants the requirement. 
    
borrower represents-warrants warrants information. 

% Article 5

It becomes the case that
    a requirement is potentially defaulted
when
    the requirement is failure to perform 
if borrower covenants the requirement. 

a borrower covenants covenants information. 

% Article 6
    
It becomes the case that
    borrower is liable to litigation
when
    it is the end of a date
if a requirement is defaulted on the date
and the date is before 2020-06-01
and a sum is the total due
and it is not the case that
    borrower paid the sum to lender on the date. 

It becomes the case that
    agreement is terminated
when
    borrower pays a sum to lender
if a requirement is defaulted on a date
and it is the date
and the sum is the total due. 
    
It becomes the case that
    agreement is terminated
when 
    borrower pays 525 to lender
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
    it is 2014-08-02T00:00:00.
    it is the end of 2014-06-01.
    borrower requests 1000, at 2014-06-01T00:00:00.
     %lender advances 1000, at 2014-06-02T00:00:00.
     %borrower pays 550 to lender, at 2015-06-01T12:00:00.
     %borrower pays 525 to lender, at 2016-06-01T12:00:00.

scenario test2 is:
    it is 2014-08-02T00:00:00.

query 1 is:
    an amount is due on a date from borrower to lender.

query 2 is:
    lender is liable to litigation, at 2014-06-02T00:00:00.

query 3 is:
    borrower requested an amount on a date.

query 4 is:
    a borrower pays an amount to a lender.

query 5 is:
    a person is liable to litigation. 

query 6 is:
    the agreement is terminated.
    
query 7 is:
    borrower paid 525 to lender on a date.
   
").

/** <examples>
?- answer("1 with test2").
?- is_it_illegal("borrower pays an amount to lender, at a date", test2). 
?- answer("2 with test1").
*/