% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040',[]).

mainGoal(complies_to_statutory_residence_test(_Individual), "Determine if a person is a UK resident for tax purposes").

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
        ++ (complies_to_statutory_residence_test(alex) on Date if before(Date,'20180406') ), % not quite what was stated, but close
        works_sufficient_hours_overseas(alex) at RDRM11150,
        no_significant_breaks_from_overseas_work(alex) at THIRD_OT,
        days_working_in_uk_more_than(alex,_,0) at THIRD_OT,
        days_spent_in_uk(alex,Start,End,DaysInUK) at myDb123,
        second_automatic_uk_test(_) at RDRM11130 if false,
        third_automatic_uk_test(_) at RDRM11370 if false,
        ties_test(_) at RDRM11510 if false
        ], not complies_to_statutory_residence_test(alex) on A_DATE)
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
        ], complies_to_statutory_residence_test(beatrice) on '20190406')
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
        ],not complies_to_statutory_residence_test(chris) on '20190406')
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
        
% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"


complies_to_statutory_residence_test(Individual) on Date if % this could actually go into the if-then-else below; just following the text
    satisfies_first_automatic_uk_test_STEP1(Individual) on Date.

complies_to_statutory_residence_test(Individual) on Date if 
    not does_not_meet_any_of_three_overseas_tests_STEP2(Individual) on Date. 

complies_to_statutory_residence_test(Individual) on Date if 
    does_not_meet_any_of_three_overseas_tests_STEP2(Individual) on Date 
    and meets_either_the_second_or_the_third_uk_test_STEP3(Individual) on Date. 

complies_to_statutory_residence_test(Individual) on Date if
    does_not_meet_any_of_three_overseas_tests_STEP2(Individual) on Date 
    and meets_ties_test_STEP4(Individual) on Date. 

does_not_meet_any_of_three_overseas_tests_STEP2(Individual) on Date if
    not meets_first_automatic_overseas_test(Individual) on Date 
    and not meets_second_automatic_overseas_test(Individual) on Date 
    and not meets_third_automatic_overseas_test(Individual) on Date.

meets_either_the_second_or_the_third_uk_test_STEP3(Individual) on Date if
    (second_automatic_uk_test(I) on Date or third_automatic_uk_test(Individual) on Date). 

meets_ties_test_STEP4(Individual) on Date if 
    ties_test(Individual) on Date.

satisfies_first_automatic_uk_test(Individual) on Date if
    uk_tax_year_for_date(Date,_,Start,End) and days_spent_in_uk(Individual,Start,End,Duration) and Duration >= 183.

second_automatic_uk_test(Individual) on Date if
    second_automatic_uk_test(Individual) on Date at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11330".

third_automatic_uk_test(Individual) on Date if
    third_automatic_uk_test(Individual) on Date at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11370".

ties_test(Individual) on Date if
    ties_test(Individual) on Date at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11510".

meets_first_automatic_overseas_test(Individual) on Date if % cf. https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11120
    uk_tax_year_for_date(Date,ThisYear,_,_) and between(ThisYear-3,ThisYear-1,PreviousYear) 
    and uk_tax_year_for_date(PreviousDate,PreviousYear,PreviousStart,PreviousEnd) 
    and days_spent_in_uk(Individual,PreviousStart,PreviousEnd,Duration) and Duration <16
    and complies_to_statutory_residence_test(Individual) on PreviousDate. % HACK: this needs to be after the previous condition, to avoid going back in time ad eternum

meets_second_automatic_overseas_test(Individual) on Date if
    uk_tax_year_for_date(Date,ThisYear,Start,End)  
    and days_spent_in_uk(Individual,Start,End,Duration) and Duration <46
    % HACK: this needs to be after the previous condition, to avoid going back in time ad eternum
    and forall( (between(ThisYear-3,ThisYear-1,PreviousYear) and uk_tax_year_for_date(PreviousDate,PreviousYear,_PreviousStart,_PreviousEnd)), 
        not complies_to_statutory_residence_test(Individual) on PreviousDate). 

meets_third_automatic_overseas_test(Individual) on Date if
    meets_third_automatic_overseas_test(Individual) on Date at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140".

days_spent_in_uk(Individual,Start,End,TotalDays) if
    days_spent_in_uk(Individual,Start,End,TotalDays) at myDb123.

/** <examples>
?- query_with_facts(complies_to_statutory_residence_test(Individual) on '20180406' at 'https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040','Chris Feb 12 - 2A',Unknowns,Explanation,Result).
?- le(LogicalEnglish).
*/
    