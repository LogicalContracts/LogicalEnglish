:- module('two+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
    *a concept* is such that *a definition*,
    it is permitted that *an event*,
    *a party* designates by *an action* at *a time T1* that *an event*,
    *an event* occurs at *a time*,
    *a party* performs *an action* at *a time*,
    it is *a requirement* that *a definition*,
    *a borrower* pays *an amount* to *a lender* on *a date*,
    *a borrower* defaults on *a date*,
    *a lender* notifies *a borrower* on *a date* that *a message*,
    *a borrower* fails to fulfil *a requirement* on *a date*,
    *a borrower* cures the failure of *a requirement* on *a day*,
    *a date* is on or before *a later date*,
    *a requirement* becomes failure on *a date*,
    *a requirement* is timely cured,
    *a number* maps to *a date*,
    *a date* is *a number* days away from *a second date*,


the knowledge base metaevent includes:

An event occurs at a T2
    if it is permitted that a party designates by an action at a T1 that the event occurs at T2
    and the party performs the action at T1.

it is a requirement that borrower pays 550 to lender on 2015-06-01.
it is a requirement that borrower pays 525 to lender on 2016-06-01.

a borrower defaults on a D3
     if the borrower fails to fulfil a requirement on a D0
     and a lender notifies the borrower on a D1 that the borrower fails to fulfil the requirement on D0
     and D3 is 2 days after D1
     and it is not the case that
         the borrower cures the failure of the requirement on a D2
         and D2 is equal to D3
             or D2 is before D3.

% eventuality to obligation level
first obligation is such that borrower pays 525 to lender on 2015-06-01.

% obligation to failure level
A requirement becomes failure on a date
    if the requirement is such that a borrower pays an amount to a lender on the date
    and it is not the case that
        the borrower pays the amount to the lender on the date.

% failure to cure level
A requirement is timely cured
    if the requirement becomes failure on a date
    and the requirement is such that  a borrower pays an amount to a lender on the date
    and the borrower pays the amount to the lender on a second date
    and the second date is on or before the date.

A date is on or before a second date
    if second is a X days after the date
    and X >= 0.

% X>=0? X>0?

% supporting tools

a number maps to a date
    if the number corresponds to date the date.

a date is a number X days away from a second date
    if the date is X days after second.

scenario test is:
     borrower fails to fulfil payment on 2015-06-01.
     lender notifies borrower on 2015-06-02 that borrower fails to fulfil payment on 2015-06-01.

scenario test2 is:
    borrower pays 525 to lender on 2015-06-02.

scenario test3 is:
    it is permitted that Bob designates by email at a 1 that presentation occurs at 2.
    bob performs email at 1.

query zero is:
    which event occurs at which time.

query one is:
     which person defaults on which time.

query two is:
    which requirement becomes failure on which date
    and which date maps to a good date.

query three is:
    which requirement is timely cured.

").

/** <examples>
?- answer("one with test").
?- answer("two with test2").
*/