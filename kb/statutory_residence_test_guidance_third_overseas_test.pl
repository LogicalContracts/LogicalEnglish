:- module('https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140',[]).

% Date is a day in the relevant UK tax year
third_automatic_overseas_test(I) on Date if 
    works_sufficient_hours_overseas(I) on Date 
        at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11150"
    and no_significant_breaks_from_overseas_work(I) on Date
    and days_working_in_uk_more_than(I,3,DaysWorkingUK) on Date and DaysWorkingUK<31
    and uk_tax_year(Date,_,Start,End) and days_spent_in_uk(I,Start,End,DaysInUK) at myDb123 and DaysInUK<91.

/** <examples>
?- le(LogicalEnglish).
*/