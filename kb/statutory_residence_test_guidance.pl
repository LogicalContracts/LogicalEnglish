% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
myURL("https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040").

% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.

% **** clause order may not be enough.... there seems to be a global if-then-else:
srt(true) if 
    individual(I) and first_automatic_uk_test(I).

srt(false) if 
    not srt(true) and individual(I) and (
        first_automatic_overseas_test(I) or second_automatic_overseas_test(I)
        or third_automatic_overseas_test(I) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140"
    ). 

srt(true) if 
    individual(I) and 
    second_automatic_uk_test(Individual) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11330".

srt(true) if 
    individual(I) and 
    third_automatic_uk_test(Individual) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11370".

srt(true) if 
    individual(I) and
    ties_test(I) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11510".


first_automatic_uk_test(Individual) if
    this_year(Year) and days_in_uk(I,Year,Duration) and Duration >= 183.

first_automatic_overseas_test(Individual) if % https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11120
    this_year(Y) and between(Y-3,Y-1,Previous) and srt(true) on PreviousYear
    and days_in_uk(I,Y,Duration) and Duration <16.

second_automatic_overseas_test(ndividual) if
    this_year(Y) and  forall(between(Y-3,Y-1,PreviousYear), not srt(true) on PreviousYear)
    and days_in_uk(I,Y,Duration) and Duration <46.

days_in_uk(Individual,Year,TotalDays) if
    days_in_uk(Individual,Year,TotalDays) at myDb123.