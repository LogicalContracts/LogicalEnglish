:- module(payg,[]).

en("the target language is: prolog. 
    
the templates are:
    the varied amount payable for *a quarter* by *an entity* is calculated as *an amount*.
    the estimated tax for *a year* for *an entity* is *an amount*. 
    the year-to-date fraction for *a quarter* is *a fraction*.
    the year-to-date instalment adjustment for *an entity* at *a quarter* is calculated as *an amount*.
    the new varied rate for *an entity* for *a year* is *an amount*.
    the PAYG instalment income for *an entity* for *a year* is *an amount*.
    the instalment income for *an entity* for *a quarter* of *a year* is *an amount*.
    EBITDA-based income for *an entity* for *a year* is *an amount*.
    ordinary income for *an entity* for *a year* is *an amount*.
    profit or loss for *an entity* for *a year* is *an amount*.
    tax for *an entity* for *a year* is *an amount*.
    depreciation for *an entity* for *a year* is *an amount*.
    amortisation EBITDA for *an entity* for *a year* is *an amount*.
    non-deductible expenses for *an entity* for *a year* is *an amount*.
    income with tax removed for *an entity* for *a year* of *an amount*.
    tax offsets for *an entity* for *a year* of *an amount*.
    net capital gains for *an entity* for *a year* of *an amount*.
    SB depreciation & capital works deduction for *an entity* for *a year* of *an amount*.
    unused prior year loss for *an entity* for *a year* of *an amount*.
    prior BAS period PAYGI calculation loss of *an amount*.
    Estimated annual net tax payable for *an entity* is *an amount*.
    Estimated taxable income for *an entity* is *an amount*.
    applicable tax rate for *an entity* is *a percentage*.
    non-refundable tax offsets for *an entity* is *an amount*.
    *an amount* was reported as an instalment on *a quarter* of the income year.
    *a value* was reported as a variation on *a quarter* of the income year.
    *an amount* is a type of business or investment income for *an entity* for *a quarter*.
    *an amount* is a type of excluded income for *an entity* for *a quarter*.
    tax was withheld from *an amount* because of no TFN or ABN for *an entity* for *a quarter*. 
    *a thing* is gross rent for *an entity* for *a quarter*.
    *a thing* is dividends for *an entity* for *a quarter*.
    *a thing* is royalties for *an entity* for *a quarter*.
    *a thing* is foreign pensions for *an entity* for *a quarter*.
    *a thing* is partnership income for *an entity* for *a quarter*.
    *a thing* is trust income for *an entity* for *a quarter*.
    *a thing* is foreign income for *an entity* for *a quarter*.
    *a thing* is interest for *an entity* for *a quarter*.
    *a thing* is gross sales for *an entity* for *a quarter*.
    *a thing* is gross fees for services for *an entity* for *a quarter*.
    *a thing* is withdrawals from farm management deposits for *an entity* for *a quarter*.
    *a thing* is fuel tax credits for *an entity* for *a quarter*.
    *a thing* is JobKeeper payments for *an entity* for *a quarter*.
    *an amount* is a capital gain for *an entity* for *a quarter*.
    *a thing* is a type of excluded income for *an entity* for *a quarter*.
    *a thing* is GST for *an entity* for *a quarter*.
    *a thing* is wine equalisation tax for *an entity* for *a quarter*.
    *a thing* is luxury car tax for *an entity* for *a quarter*.
    *a thing* is a type of excluded income for *an entity* for *a quarter*.
    *a thing* is salary and wages for *an entity* for *a quarter*.
    an amount was withheld from *a thing* under the PAYG withholding system.
    *a thing* is a type of excluded income for *an entity* for *a quarter*.
    *a thing* is a franking credit for *an entity* for *a quarter*.
    *a thing* is a deemed dividend for *an entity* for *a quarter*.
    *a thing* is an exempt income for *an entity* for *a quarter*.
    *a thing* is a payment under the National Rental Affordability Scheme for *an entity* for *a quarter*.
    *a thing* is a grant under the energy grants credits scheme for *an entity* for *a quarter*.
    *a thing* is a type of excluded income for *an entity* for *a quarter*.
    *a thing* is a capital gain for *an entity* for *a quarter*.
    *a year* is the current year.
    *an amount* is an ordinary income for *an entity* for *a quarter*.
    *an amount* is specially included as instalment income for *an entity* for *a quarter*.

the knowledge base payg includes:
 
the varied amount payable for a quarter Q by an entity E is calculated as a value A
    if the estimated tax for a year for E is an amount ET
    and the year-to-date fraction for Q is an fraction F
    and the year-to-date instalment adjustment for E at Q is calculated as an amount Y
    and A = ET * F - Y.


the year-to-date fraction for a quarter Q is a fraction F
    if Q is quarter 1 
        and F is 0.25
    or Q is quarter 2 
        and F is 0.5
    or Q is quarter 3 
        and F is 0.75
    or Q is quarter 4 
        and F is 1.0.


the year-to-date instalment adjustment for an entity at a quarter is calculated as a value V % check this
    if a value IR is the sum of each amount such that
        the amount was reported as an instalment on the quarter of the income year
    and a value IVC is the sum of each value such that
        the value was reported as a variation on the quarter of the income year
    and V = IR - IVC.


the new varied rate for an entity for a year is an amount VR
    if the estimated tax for the year for the entity is a value ET
    and the PAYG instalment income for the entity for the year is a value I
    and I>0
    and VR = (ET/I)*100.


% instalment income
% https://www.ato.gov.au/businesses-and-organisations/income-deductions-and-concessions/payg-instalments/calculate-your-payg-instalments/instalment-income

the PAYG instalment income for an entity for a year is a total amount 
    if the total amount is the sum of each partial such that
        the instalment income for the entity for a quarter of the year is the partial. 

% --- General Rule --- (as sketched by Gemini and Andrew)
% The primary rule states that business/investment income is included, unless it's specifically excluded.  

% Your instalment income is all the ordinary income you earned from your business and investment activities for the quarter (excluding GST).
the instalment income for an entity for a quarter of a year is a quarter amount 
    if the year is the current year
    and quarter amount is the sum of each partial such that
        partial is an ordinary income for the entity for the quarter.

an amount is an ordinary income for an entity for a quarter 
    if the amount is a type of business or investment income for the entity for the quarter
    and     it is not the case that
                the amount is a type of excluded income for the entity for the quarter
        or the amount is specially included as instalment income for an entity for a quarter. 

% An exception for income where tax was withheld due to no TFN/ABN. This is always included.
an amount is specially included as instalment income for an entity for a quarter
    if   tax was withheld from the amount because of no TFN or ABN for the entity for the quarter. 

a thing is a type of business or investment income for an entity for a quarter
    if the thing is gross rent for the entity for the quarter
    or the thing is dividends for the entity for the quarter
    or the thing is royalties for the entity for the quarter
    or the thing is foreign pensions for the entity for the quarter
    or the thing is partnership income for the entity for the quarter
    or the thing is trust income for the entity for the quarter
    or the thing is foreign income for the entity for the quarter
    or the thing is interest for the entity for the quarter
    or the thing is gross sales for the entity for the quarter
    or the thing is gross fees for services for the entity for the quarter
    or the thing is withdrawals from farm management deposits for the entity for the quarter
    or the thing is fuel tax credits for the entity for the quarter
    or the thing is JobKeeper payments for the entity for the quarter.

 % Capital gains are a special case, only included for super funds.
an income is specially included as instalment income for an entity for a quarter
    if the income is a capital gain for the entity for the quarter
    and the entity is a super fund.

% % --- Specific Exclusions (Types of Excluded Income) ---
a thing is a type of excluded income for an entity for a quarter
    if the thing is GST for the entity for the quarter
    or the thing is wine equalisation tax for the entity for the quarter
    or the thing is luxury car tax for the entity for the quarter.

a thing is a type of excluded income for an entity for a quarter
    if the thing is salary and wages for the entity for the quarter
    and an amount was withheld from the thing under the PAYG withholding system.

a thing is a type of excluded income for an entity for a quarter
    if the thing is a franking credit for the entity for the quarter
    or the thing is a deemed dividend for the entity for the quarter
    or the thing is an exempt income for the entity for the quarter
    or the thing is a payment under the National Rental Affordability Scheme for the entity for the quarter
    or the thing is a grant under the energy grants credits scheme for the entity for the quarter.
    
% General capital gains are excluded (the super fund rule above is the exception).
a thing is a type of excluded income for an entity for a quarter
    if the thing is a capital gain for the entity for the quarter
    and it is not the case that
        the entity is a super fund.


% to be verified PAYG instalment income
% PAYG instalment income for an entity for a year is an amount I
%     if EBITDA-based income for the entity for the year is I
%     or ordinary income for the entity for the year is I.


% EBITDA-based income for an entity for a year is an amount E
%     if profit or loss for the entity for the year is a value P
%     and non-deductible expenses for the entity for the year is a value ND
%     and a value L is the sum of each amount such that
%         income with tax removed for the entity for the year of the amount
%         or tax offsets for the entity for the year of the amount
%         or net capital gains for the entity for the year of the amount
%         or SB depreciation & capital works deduction for the entity for the year of the amount
%         or unused prior year loss for the entity for the year of the amount
%         or prior BAS period PAYGI calculation loss of the amount
%     and E = P + ND - L. 
% end of PAYG instalment income


the estimated tax for a year for an entity is an amount ET
    if Estimated annual net tax payable for the entity is ET
    and ET >= 0. 

Estimated annual net tax payable for an entity is a value ET
    if Estimated taxable income for the entity is a value ETI
    and applicable tax rate for the entity is a percentage TR
    and a value L is the sum of each amount such that
        non-refundable tax offsets for the entity is the amount
    and ET = ETI * TR - L.


scenario test is:
    2025 is the current year.
    profit or loss for Australian entity for 2025 is 40000.
    non-deductible expenses for Australian entity for 2025 is 5000.
    income with tax removed for Australian entity for 2025 of 3000.
    tax offsets for Australian entity for 2025 of 2000.
    net capital gains for Australian entity for 2025 of 1000.
    SB depreciation & capital works deduction for Australian entity for 2025 of 4000.
    unused prior year loss for Australian entity for 2025 of 5000.
    prior BAS period PAYGI calculation loss of 1000.
    Estimated annual net tax payable for Australian entity is 10000.
    Estimated taxable income for Australian entity is 60000.
    applicable tax rate for Australian entity is 0.25.
    non-refundable tax offsets for Australian entity is 2000.
    1000 was reported as an instalment on quarter 4 of the income year.
    2000 was reported as a variation on quarter 4 of the income year.

scenario harmander is:
    the estimated tax for 2025 for Harmander is 10125.
    ordinary income for Harmander for 2025 is 82480.

% https://www.ato.gov.au/businesses-and-organisations/income-deductions-and-concessions/payg-instalments/starting-payg-instalments
scenario example1 is:
    the estimated tax for 2025 for Rob is 7544. 
    the PAYG instalment income for Rob for 2025 is 49500. 

scenario example2 is:
    the estimated tax for 2025 for Danielle is 20437.
    the PAYG instalment income for Danielle for 2025 is 90000.

scenario mine is:
    the estimated tax for 2025 for me is 16388.
    the PAYG instalment income for me for 2025 is 80000.

scenario alex is:
    2025 is the current year.
    15000 is gross sales for Alex for quarter 1.
    5000 is salary and wages for Alex for quarter 1.
    an amount was withheld from 5000 under the PAYG withholding system.
    200 is interest for Alex for quarter 1.
    1500 is GST for Alex for quarter 1.

query normal is:
    the varied amount payable for quarter 4 by Australian entity is calculated as which amount.

query rate is:
    the new varied rate for which entity for 2025 is which amount.

query payg is:
    the PAYG instalment income for which entity for which year is which amount. 

"). 

/** <examples>
?- show prolog.
?- answer("normal with test").
?- answer("rate with test").
?- answer(normal, with(test), le(E), R). 
?- answer(rate, with(test), le(E), R). 
?- answer(rate, with(example1), le(E), R). 
?- answer(rate, with(example2), le(E), R). 
?- answer(rate, with(mine), le(E), R). 
?- answer(payg, with(alex), le(E), R).

*/