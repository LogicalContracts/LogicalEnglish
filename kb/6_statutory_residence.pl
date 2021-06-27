:-module('https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040',[]).

mainGoal(complies_to_statutory_residence_test_at(_Individual, _Date), "Determine if a person is a UK resident for tax purposes").


uk_tax_year_for_date(Date,Year,Start,End) :-
    reasoner:uk_tax_year(Date,Year,Start,End). % explicit module qualifier needed

example('Chris Feb 12 - 2A',[
/*
Determine if Alex was a UK residence for tax purposes for the tax year 6 Apr 2018 – 5 Apr 2019
-	Alex is a UK citizen who lived in the UK until March 2018.
-	Alex left the UK to work full-time in Dubai from 1 Mar 2018 – 25 Mar 2019.
-	Alex took 20 days annual leave in this period (and no sick/parental leave etc.). He spent 15 days of his annual leave in the UK. 
-	Alex moved back to the UK on 26 Mar 2019, and did not find a job until 1 May 2019 (i.e. there was a gap in employment from 26 March – 1 May)
-	Assume that Alex took no ‘significant breaks’ (I can’t find a definition for this!) from his work in Dubai.
-	Assume that Alex worked 8 hours each work-day – 40 hrs per full week.
Expected result: non-resident by virtue of https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140 
   (third automatic overseas test)
NOTE: out of scope of original example, added a stub at 'https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11150'
   to allow testing this knowledge page
*/
    scenario([
        % a plus sign indicates this hypothetical extends, not redefines, existing rules and facts:
        ++ (complies_to_statutory_residence_test_at(alex, _Date) if before(Date,'20180406') ), % not quite what was stated, but close
        works_sufficient_hours_overseas(alex) at RDRM11150,
        no_significant_breaks_from_overseas_work(alex) at THIRD_OT,
        days_working_in_uk_more_than(alex,_,0) at THIRD_OT,
        days_spent_in_uk(alex,Start,End,DaysInUK) at myDb123,
        second_automatic_uk_test(_) at RDRM11130 if false,
        third_automatic_uk_test(_) at RDRM11370 if false,
        ties_test(_) at RDRM11510 if false
        ], not complies_to_statutory_residence_test_at(alex,A_DATE))
    ]) :- 
    A_DATE='20180406', uk_tax_year_for_date(A_DATE,_,Start,End),
    RDRM11150="https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11150",
    RDRM11130="https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11330",
    RDRM11370="https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11370",
    RDRM11510="https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11510",
    THIRD_OT='https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140',
    subtract_days(End,'20190326',Days), DaysInUK is 15 /*leave*/ +Days.

example('Chris Feb 12 - 2B',[
/*
Determine if Beatrice was a UK resident the tax year 6 Apr 2019 – 5 Apr 2020
-	Beatrice is a UK citizen with her home in the UK.
-	Beatrice worked as a pilot for an Airline company for the duration of this period.
-	95% of Beatrice’s work flights were cross-border flights between the UK and other countries.
-	Beatrice spent 200 days in the UK in the tax year from 6 Apr 2019 – 5 Apr 2020.
-	Beatrice worked overseas for 160 days in the same period.
-	The remaining days were spent on holiday outside the UK.
-	Assume that Beatrice worked 8 hours each work-day – 40 hrs per full week.
Expected result: resident, does not qualify for 3rd automatic (UK) test due to her having a ‘relevant job’ as 
    per https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11780
*/
    scenario([
    % NOTE: out of scope of original example; requires 'https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11370'
        ], complies_to_statutory_residence_test_at(beatrice,'20190406'))
    ]).

example('Chris Feb 12 - 2C1',[
    /*
Determine if Chris was a statutory resident for the UK tax year from 6 Apr 2019 – 5 Apr 2020.
Chris is an Australian citizen, and lived there for most of his adult life. Since July 2017, Chris has spent most of his time in Europe. 
Details of his travels for the last two years are below:
-	19 Mar 2019: Chris began living in Berlin on a German Working Holiday visa.
-	30 Nov 2019: Chris left Berlin and went back to Australia for some time.
-	Chris did not pay German income tax for the 2019 tax year in Germany on the advice of an accountant, who said he was considered 
    an Australian resident for tax purposes for that year.
-	13 Feb 2020: Issued a UK residence permit for a T5 Youth Mobility visa valid for 2 years (27 Feb 2020 – 27 Feb 2022).
-	27 Feb 2020: Left Australia and arrived in the UK.
Employment:
•	1 Jan 2019 – 1 Mar 2020: Worked part-time for Outotec (Australian branch) on a casual/hourly basis.
•	1 Jan 2019 – 1 Mar 2020: Worked part-time for Legal Technology Research (UK) on a casual/hourly basis.
•	1 Mar 2020 – present: Works full-time for Aora on a permanent basis.
Expected result: non-resident due to 2nd automatic test; see https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11130
*/
    scenario([
    % NOTE: out of scope of original example, requires 'https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11330'
        ],not complies_to_statutory_residence_test(chris,'20190406'))
    ]).
    
example('Chris Feb 12 - 2C2',[
    /*
Determination should be made for the tax year from 6 Apr 2020 – 5 Apr 2021.
Facts as per Example 2C1; assume that Chris remains in the UK for the remainder of 2021.
Expected result: resident
    */
    scenario(Facts, complies_to_statutory_residence_test(chris) on '20200406')
    % NOTE: out of scope of original example, as previous one
    ]) :- 
        example('Chris Feb 12 - 2C1',[scenario(Facts2C1,_)]), 
        Facts=[ + (days_spent_in_uk(chris,'20210406','20220405',Days) if subtract_days('20210406','20220405',Days) ) |Facts2C1]. 
        

en("the templates are:
    a date falls in the UK tax year a year number that starts at a starting number and ends at a ending number,
    an individual is resident in the UK for a tax year,
    an individual meets the automatic residence test for a year,
    an individual meets the sufficient ties test for a year,
    an individual meets an alternative of the automatic UK tests for a year,
    an individual complies to statutory residence test at a date,
    an individual satisfies first automatic uk test STEP 1 at a date,
    an individual does not meet any of three overseas tests STEP 2 at a date,
    an individual meets either the second or the third uk test STEP 3 at a date,
    an individual meets ties test STEP 4 at a date,
    an individual meets first automatic overseas test at a date,
    an individual meets second automatic overseas test at a date,
    an individual meets third automatic overseas test at a date according to other legislation,
    an individual meets third automatic overseas test at a date,
    an individual meets ties test at a date,
    an individual satisfies first automatic uk test at a date,
    an individual spent a number oof days in the UK starting at a starting day and ending at an ending day,
    an individual meets second automatic uk test at a date,
    an individual meets second automatic uk test at the date according to other legislation,
    an individual meets third automatic uk test at a date,
    an individual meets third automatic uk test at a date according to other legislation,
    an individual meets ties test at a date,
    an individual meets ties test at a date according to other legislation,
    a year is between the second year & a third year,
    the individual spent the total Days days in the UK starting at the first day and ending at the last day according to other legislation.

the knowledge base includes:

An individual is resident in the UK for a tax year
    if the individual meets the automatic residence test for the year
    or the individual meets the sufficient ties test for the year.

%An individual meets the automatic residence test for a year
%    if the individual meets an alternative of the automatic UK tests for the year
%    and the alternative is in [first, second, third, fourth]
%    and it is not the case that
%            the individual meets a different alternative of the automatic overseas tests for the year
%            and the different alternative is in [first, second, third, fourth, fifth].

%a date falls in the UK tax year a year number that starts at a starting number and ends at a ending number
%    if reasoner:uk_tax_year(the date,the year number,the starting number,the ending number).

an individual complies to statutory residence test at a date
    if the individual satisfies first automatic uk test STEP 1 at the date.

an individual complies to statutory residence test at a date
    if it is not the case that
        the individual does not meet any of three overseas tests STEP 2 at the date.

an individual complies to statutory residence test at a date
    if the individual does not meet any of three overseas tests STEP 2 at the date
    and the individual meets either the second or the third uk test STEP 3 at the date.

an individual complies to statutory residence test at a date
    if the individual does not meet any of three overseas tests STEP 2 at the date
    and the individual meets ties test STEP 4 at the date.

an individual does not meet any of three overseas tests STEP 2 at a date
    if it is not the case that
        the individual meets first automatic overseas test at the date
    and it is not the case that
        the individual meets second automatic overseas test at the date
    and it is not the case that
        the individual meets third automatic overseas test at the date.

an individual meets either the second or the third uk test STEP 3 at a date
    if the individual meets second automatic uk test at the date
    or the individual meets third automatic uk test at the date.

an individual meets ties test STEP 4 at a date
    if the individual meets ties test at the date.

an individual satisfies first automatic uk test at a date
    if the date falls in the UK tax year a year that starts at a starting day and ends at an ending day
    and the individual spent a number oof days in the UK starting at the starting day and ending at the ending day
    and the number oof days >= 183.

an individual meets second automatic uk test at a date
    if the individual meets second automatic uk test at the date according to other legislation.

an individual meets third automatic uk test at a date
    if the individual meets third automatic uk test at the date according to other legislation.

an individual meets ties test at a date
    if the individual meets ties test at the date according to other legislation.

%an individual meets first automatic overseas test at a date
%    if the date falls in the UK tax year a current year that starts at a day and ends at an other day
%    and a previous year is between the current year - 3 & the current year - 1
%    and a previous date falls in the UK tax year the previous year that starts at a previous starting day and ends at a previous ending day
%    and the individual spent a number oof days in the UK starting at the previous starting day and ending at the previous ending day
%    and the number oof days < 16
%    and the individual complies to statutory residence test at the previous date.

%an individual meets second automatic overseas test at a date
%    if the date falls in the UK tax year a current year that starts at a day and ends at an other day
%    and the individual spent a number oof days in the UK starting at the day and ending at the other day
%    and the number oof days < 46
%    and for all cases in which
%            a previous year is between the current year - 3 & the current year - 1
%            and a previous date falls in the UK tax year the previous year that starts at a previous start and ends at a previous end
%        it is the case that:
%            it is not the case that
%                the individual complies to statutory residence test at the previous date.

an individual meets third automatic overseas test at a date
    if the individual meets third automatic overseas test at the date according to other legislation.

% predefined
%an individual spent a total days days in the UK starting at a first day and ending at a last day
%    if the individual spent the total Days days in the UK starting at the first day and ending at the last day according to other legislation.").


/** <examples>
?- query_with_facts(complies_to_statutory_residence_test_at(Individual,'20180406') at 'https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040','Chris Feb 12 - 2A',Unknowns,Explanation,Result).
*/
