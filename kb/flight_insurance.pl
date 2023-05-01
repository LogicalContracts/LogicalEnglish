:- module('flight_insurance+http://tests.com',[]).

en("the target language is: prolog. 

the templates are:
*a flight* is covered,
*a flight* is cancelled,
*a flight* is diverted,
*a flight* is scheduled to arrive at *a time*,
*a flight* actually arrives at *a time*,
*a flight* is sufficiently delayed,
*a flight* is specified in *an email*,
*a min delayed arrival time* is stated in *an email*, %This is a hack, because I can't specify hour differences right now
*an email* is issued on *a date*,
*an email* is most recent,
the cause was within the control of the policyholder,
a benefit should be paid.


the knowledge base flight_insurance includes:
an email is most recent
	if the email is issued on a date D1
	and it is not the case that
		an other email is issued on a date D2
		and D1 < D2.
    
a flight is covered
	if the flight is specified in an email
	and the email is most recent.
    
    
a benefit should be paid
	if a flight is covered
    and 	it is not the case that
    		the cause was within the control of the policyholder
    		and the flight actually arrives at a time T1
			and the flight is scheduled to arrive at a time T2
    		and an email is the most recent
			and a min delayed arrival time is stated in the email
    		and T2 < T1
    		and T1 >= the min delayed arrival time
    	or the flight is cancelled
		or the flight is diverted.

% Can't use multi-word variable names when chaining hypotheses

% Past delay period
scenario pay_benefit_1 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.
2021-12-14T02:00:00 is stated in Email1.
Flight1 actually arrives at 2021-12-14T02:30:00.
Flight1 is scheduled to arrive at 2021-12-14T01:00:00.


% Check diversion
scenario pay_benefit_2 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.
Flight1 is diverted.


% Check cancellation
scenario pay_benefit_3 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.
Flight1 is cancelled.


% Before delay period
scenario pay_no_benefit_1 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.
2021-12-14T03:00:00 is stated in Email1.
Flight1 actually arrives at 2021-12-14T02:30:00.
Flight1 is scheduled to arrive at 2021-12-14T01:00:00.

    
scenario covered_email_1 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.


scenario covered_email_2 is:
Flight1 is specified in Email1.
Flight1 is specified in Email2.
Email1 is issued on 2021-12-12.
Email2 is issued on 2021-12-13.
    
scenario covered_email_3 is:
Flight1 is specified in Email2.
Email1 is issued on 2021-12-12.
Email2 is issued on 2021-12-13.


scenario uncovered_email_1 is:
Flight1 is specified in Email1.
Email1 is issued on 2021-12-12.
Email2 is issued on 2021-12-13.



query one is:

which flight is covered.
    
query two is:

a benefit should be paid.
").





/** <examples>
?- answer(one,with(pay_benefit_1), le(Explanations), R).
?- show prolog.
*/
