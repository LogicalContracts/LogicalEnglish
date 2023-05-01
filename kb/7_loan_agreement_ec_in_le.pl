:- module('four+http://tests.com',[]).

en("the target language is: prolog.

the meta predicates are:
    *a fluent* holds at *a date*,
    *an event* happens at *a date*,
    *an event* at *a time* initiates *a fluent*,
    *an event* at *a time* terminates *a fluent*,
    *a fluent* is interrupted between *a first time* and *a second time*,
  	*a concept* is that *a definition*,
  	*a thing* is different from *a second thing*, 
    it is illegal that *an event* at *a time*,
    
the templates are:
  *a day* ends, 
  it is *a date*,
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
    	and T1 is before T
    	and the event at T1 initiates the fluent 
    	and it is not the case that
    		the fluent is interrupted between T1 and T. 
    
a fluent is interrupted between a T1 and a T2
    	if a second event happens at a T3
    	and the second event at T3 terminates the fluent 
    	and T1 is before T3
    	and T3 is before T2.

% preliminars

a day ends at a time initiates it is a new day
    if the new day is 1 day after the day.
    %and the time is equal to the day T 23:59:59. 
    
% Article 1
    
2014-06-02 ends at a time initiates the lender is liable to litigation     
   	if the borrower requested 1000 on 2014-06-01 holds at the time 
    and it is not the case that
    	the lender advanced 1000 on a date holds at 2014-06-02. 

2014-06-01 ends at a time initiates the agreement is terminated
    if it is not the case that
    	the borrower requested 1000 on 2014-06-01 holds at 2014-06-01. 

the borrower requests each amount at each date initiates the borrower requested the amount on the date.
    
the lender advances each amount at each date initiates the lender advanced the amount on the date. 

% Articles 2 and 10
    
550 is due on 2015-06-01 from the borrower to the lender.
525 is due on 2016-06-01 from the borrower to the lender.

a day ends at a date initiates a payment is potentially defaulted
    if the payment is equal to a borrower pays an amount to a lender
    and the amount is due on the date from the borrower to the lender
    and it is a new day holds at the new day
    and it is not the case that
        the borrower paid the amount to the lender on a different date holds at the new day.

each borrower pays each amount to each lender at each date initiates the borrower paid the amount to the lender on the date. 

% below still under review    
    
it is illegal that a borrower pays an amount to a lender at a given date
    if an amount is due on a date from the borrower to the lender
    and the given date is different from the date.  
    
scenario test is:
    2014-06-02 ends happens at 2014-06-02. 
    2014-06-01 ends happens at 2014-06-01. 
    2014-06-02 is before 2014-06-03.
    2014-06-01 is before 2014-06-02.
    the borrower requests 1000 happens at 2014-06-01.
    %the lender advances 1000 happens at 2014-06-01.

query one is:
    it is which day holds at which time. 
    
query two is:
    the lender is liable to litigation holds at which date. 

    
"). 

/** <examples>
?- answer("query one with scenario test").
?- answer("query two with scenario test").
*/
