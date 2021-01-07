% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040',[]).

mainGoal(srt(Individual), "Determine if a person is a UK resident for tax purposes").

:- thread_local theIndividual/1.
srt(Individual) :-
    assert(theIndividual(Individual)),
    srt.

% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

function(individual(), I) if  % local function for convenience
    theIndividual(I).

srt if % this could actually go into the if-then-else below; just following the text
    first_automatic_uk_test.

srt if 
    if ( first_automatic_overseas_test or second_automatic_overseas_test(individual()) or third_automatic_overseas_test(individual()))
        then false
        else (second_automatic_uk_test or third_automatic_uk_test or ties_test).

first_automatic_uk_test if
    this_year(Year) and days_in_uk(individual(),Year,Duration) and Duration >= 183.

second_automatic_uk_test if
    second_automatic_uk_test(individual()) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11330".

third_automatic_uk_test if
    third_automatic_uk_test(individual()) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11370".

ties_test if
    ties_test(individual()) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11510".

first_automatic_overseas_test if % https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11120
    this_year(Y) and between(Y-3,Y-1,PreviousYear) and srt(true) on PreviousYear
    and days_in_uk(individual(),Y,Duration) and Duration <16.

second_automatic_overseas_test(Individual) if
    this_year(Y) and forall(between(Y-3,Y-1,PreviousYear), not srt(true) on PreviousYear)
    and days_in_uk(Individual,Y,Duration) and Duration <46.

third_automatic_overseas_test(Individual) if
    third_automatic_overseas_test(Individual) at "https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11140".

days_in_uk(Individual,Year,TotalDays) if
    days_in_uk(Individual,Year,TotalDays) at myDb123.