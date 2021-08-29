:- module('http://tests.com',[]).

en("the target language is: prolog.

the meta predicates are:
    *a fluents* holds at *a time*,
    *an event* happens at *a time*,
    *an event* initiates *a fluent* at *a time*,
    *an event* terminates *a fluent* at *a time*,
    *a fluent* is interrupted between *a first time* and *a second time*,
  	*a concept* is that *a definition*,
  	*a thing* is different from *a second thing*, 
    it is illegal that *an event* at *a time*,
    
the templates are:
  *an amount* is due on *a date* from *a borrower* to *a lender*,
  *a borrower* represents-warrants *a requirement*,
  *an amount* is the total due,
  *a borrower* covenants *a covenance*,
  *a borrower* pays *an amount* to *a lender*,
  *a requirement* is in default,
  *a requirement* is remedy,
  *a person* is bankrupted by insolvency,
  *a lender* is liable to litigation,
  *a borrower* requested *an amount* on *a date*,
  *a lender* advanced *an amount* on *a date*,
  *an agreement* is terminated,
  *a payment* is potentially defaulted,
  *a borrower* paid *an amount* to *a lender* on *a date*,
  *a person* notified *a message* on *a date*,
  *a requirement* is defaulted on *a date*,
  *a requirement* is cured,
  *a requirement* is remedied,
  *a borrower* is liable to litigation,
  it is the end of *a date*,
  it is *a date*.
  *a borrower* requests *an amount*,
  *a lender* advances *an amount*,
  *a borrower* pays *an amount* to *a lender*,
  *a person* notifies *a message*,
  *a requirement* remedies,
  *a borrower* goes bankrupt,
  *a borrower* is insolvent,
  *a requirement* is proved untrue,
  *a requirement* is failure to perform,
        
the knowledge base loan agreement includes:
    
% the event calculus general axioms
	
a fluent holds at a T
    	if an event happens at a T1
    	and the event at T1 initiates the fluent % at T1
    	and T1 is before T
    		or T1 = T
    	and it is not the case that
    		the fluent is interrupted between T1 and T. 
    
a fluent is interrupted between a T1 and a T2
    	if a second event happens at a T3
    	and the second event terminates the fluent at T3
    	and T1 is before T3
    		or T1 = T3
    	and T3 is before T2
    		or T3 = T2. 

% preliminars
    
it is the end of a day initiates it is a new date at the date
    if the new date is 1 day after the day. 
    
% Article 1
    
it is the end of 2014-06-02 at 2014-06-02  initiates the lender is liable to litigation     
   	if the borrower requested 1000 on 2014-06-01 holds at 2014-06-02 
    and it is not the case that
    	the lender advanced 1000 on a date holds at 2014-06-02. 

it is the end of 2014-06-01 initiates the agreement is terminated at 2014-06-01
    if it is not the case that
    	the borrower requested 1000 on 2014-06-01 holds at 2014-06-01. 

%the borrower requests an amount initiates the borrower requested the amount on each date at the date.
the borrower requests an amount  initiates the borrower requested the amount on a date  at a time
    if the time is on the date.
    
the lender advances an amount initiates the lender advanced the amount on each date at the date. 

% Articles 2 and 10
    
550 is due on 2015-06-01 from the borrower to the lender.
525 is due on 2016-06-01 from the borrower to the lender.

it is the end of a date initiates a payment is potentially defaulted at a new date
    if the payment is equal to a borrower pays an amount to a lender
    and the amount is due on the date from the borrower to the lender
    and it is not the case that
        the borrower paid the amount to the lender on a second date holds at the new date.

a borrower pays a amount to a lender initiates the borrower paid the amount to the lender on each date at the date. 

it is illegal that a borrower pays an amount to a lender at a given date
    if an amount is due on a date from the borrower to the lender
    and the given date is different from the date.  

% Article 5 

a person notifies a message initiates the person notified the message on each date at the date. 

it is the end of a date initiates a requirement is defaulted on the date at the date
    if the requirement is potentially defaulted holds at the date
    and the lender notified a message on a second date 
    and the message is that requirement is in default 
    and it is not the case that
        a second requirement is defaulted on a third date
     
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


    
"). 