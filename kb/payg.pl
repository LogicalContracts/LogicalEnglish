:- module(payg,[]).

en("the target language is: prolog. 
    
the templates are:
    the estimated tax for *an entity* for *a year* is *an amount*. 
    the estimated annual net tax payable for *an entity* for *a year* is *an amount*.
    the estimated taxable income for *an entity* for *a year* is *an amount*.
    the applicable tax rate for *an entity* on *a year* is *a percentage*.
    *an entity* is under the aggregated turnover threshold in *a year*. 
    *an entity* is a base rate entity. 
    the tax offsets for *an entity* for *a year* is *an amount*.
    the estimated tax credits for *an entity* for *a year* are *an amount*.
    the varied amount payable for *a quarter* for *a year* by *an entity* is *an amount*.
    the year-to-date fraction for *a quarter* is *a fraction*.
    the year-to-date instalment adjustment for *an entity* for *a year* is *an amount*.
    the new varied rate for *an entity* for *a year* is *an amount*.
    the estimated PAYG instalment income for *an entity* for *a year* is *an amount*.
    *an amount* with *an ID* is an estimated ordinary income for *an entity* for *a year*.
    *an amount* with *an ID* is an estimated type of business or investment income for *an entity* for *a year*.
    *an amount* with *an ID* is specially included as estimated instalment income for *an entity* for *a year*.
    *an amount* with *an ID* is a type of estimated excluded income for *an entity* for *a year*.
    *an amount* with *an ID* was withheld as tax because of no TFN or ABN for *an entity* for *a year*.
    *an amount* with *an ID* was withheld from *an entity* under the PAYG withholding system.
    *an amount* with *an ID* was reported as an instalment on *a quarter* of *a year*.
    *an amount* with *an ID* was reported as a variation on *a quarter* of *a year*.
    the estimated gross rent for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated dividends for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated royalties for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated foreign pensions for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated partnership income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated trust income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated foreign income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated interest for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated gross sales for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated gross fees for services for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated withdrawals from farm management deposits for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated fuel tax credits for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated JobKeeper payments for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated capital gain for *an entity* for *a year* is *an amount* with *an ID*.
    *an entity* is a super fund.
    the estimated GST for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated wine equalisation tax for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated luxury car tax for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated salary and wages for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated franking credit for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated deemed dividend for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated exempt income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated payment under the National Rental Affordability Scheme for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated grant under the energy grants credits scheme for *an entity* for *a year* is *an amount* with *an ID*.
    *an entity* is the taxpayer.
    *a year* is a year under consideration.
    *a quarter* is previous or equal to *a quarter*.
    *a quarter* is previous to *a quarter*.
    the current quarter is *a quarter*.

the ontology is:
    quarter 1 is a quarter.
    quarter 2 is a quarter.
    quarter 3 is a quarter.
    quarter 4 is a quarter.
    Q1 is a quarter 1.
    Q2 is a quarter 2.
    Q3 is a quarter 3.
    Q4 is a quarter 4.

    a quarter X is previous or equal to a quarter Y 
        if X is previous to Y 
        or X is equal to Y.

    quarter 1 is previous to quarter 2.
    quarter 1 is previous to quarter 3.
    quarter 1 is previous to quarter 4.
    quarter 2 is previous to quarter 3.
    quarter 2 is previous to quarter 4.
    quarter 3 is previous to quarter 4.
    Q1 is previous to Q2.
    Q1 is previous to Q3.
    Q1 is previous to Q4.
    Q2 is previous to Q3.
    Q2 is previous to Q4.
    Q3 is previous to Q4.

the knowledge base payg includes:

% Estimated tax for the year
the estimated tax for an entity for a year is an amount ET
    if the estimated annual net tax payable for the entity for the year is ET
        and ET >= 0 
    or ET = 0
    	and it is not the case that
       		the estimated annual net tax payable for the entity for the year is an X.

% Estimated annual net tax payable for the year
the estimated annual net tax payable for an entity for a year is an amount ENT
    if the estimated taxable income for the entity for the year is an amount ETI
    and the applicable tax rate for the entity on the year is a percentage ATR
    and the tax offsets for the entity for the year is an amount TO
    and TO >= 0
    and the estimated tax credits for the entity for the year are an amount TC
    and TC >= 0
    and ENT = ETI * ATR - TO - TC. 

% Applicable tax rate (simplified)
% the entity is under the aggregated turnover threshold in the year -> Given!
the applicable tax rate for an entity on a year is a number ATR
    if         the entity is under the aggregated turnover threshold in the year 
            or the entity is a base rate entity
        and ATR is 0.25
    or      ATR is 0.30
    	and it is not the case that
    		the entity is under the aggregated turnover threshold in the year
        and it is not the case that
        	the entity is a base rate entity.

% Varied amount payable for the quarter
the varied amount payable for a quarter Q for an income year by an entity E is an amount VA
    if the current quarter is Q
    and the estimated tax for E for the income year is an amount ET
    and the year-to-date fraction for Q is a number F
    and the year-to-date instalment adjustment for E for the income year is an amount Y
    and VA = ET * F - Y.

% Year-to-date fraction for the quarter
the year-to-date fraction for a quarter Q is a number F
    if Q is quarter 1 
        and F is 0.25
    or Q is quarter 2 
        and F is 0.5
    or Q is quarter 3 
        and F is 0.75
    or Q is quarter 4 
        and F is 1.0.

% Year-to-date instalment adjustment for an entity for an income year
the year-to-date instalment adjustment for an entity E for an income year is an amount V
    if E is the taxpayer
    and the current quarter is a quarter C
    and the income year is a year under consideration
    and a number IR is the sum of each amount such that
            the amount with an ID was reported as an instalment on a quarter X of the income year
            and X is previous to C
    and a number IVC is the sum of each number such that
            the number with an other ID was reported as a variation on a quarter Y of the income year
            and Y is previous to C
    and V = IR - IVC.

% New varied rate for the year
the new varied rate for an entity for a year is a percentage VR
    if the estimated tax for the entity for the year is an amount ET
    and the year-to-date instalment adjustment for the entity for the year is an amount P
    and the estimated PAYG instalment income for the entity for the year is an amount PAYG
    and         PAYG > 0
            and VR = ((ET-P)/PAYG)*100
        or      PAYG = 0
            and VR = 0.

% instalment income (adapted to estimate PAYG instalment income with yearly basis)
the estimated PAYG instalment income for an entity for a year is a total amount 
    if the entity is the taxpayer
    and the year is a year under consideration
    and the total amount is the sum of each partial such that
        partial with an ID is an estimated ordinary income for the entity for the year. 

% --- General Rule --- (as sketched by Gemini and Andrew)
% The primary rule states that business/investment income is included, unless it's specifically excluded.          
an amount with an ID is an estimated ordinary income for an entity for a year 
    if the amount with the ID is an estimated type of business or investment income for the entity for the year
    and     it is not the case that
                the amount with the ID is a type of estimated excluded income for the entity for the year
        or the amount with the ID is specially included as estimated instalment income for the entity for the year. 

% An exception for income where tax was withheld due to no TFN/ABN. This is always included.
an amount with an ID is specially included as estimated instalment income for an entity for a year
    if   the amount with the ID was withheld as tax because of no TFN or ABN for the entity for the year. 

an amount E with an ID is an estimated type of business or investment income for an entity for a year
    if the estimated gross rent for the entity for the year is E with ID
    or the estimated dividends for the entity for the year is E with ID
    or the estimated royalties for the entity for the year is E with ID
    or the estimated foreign pensions for the entity for the year is E with ID
    or the estimated partnership income for the entity for the year is E with ID
    or the estimated trust income for the entity for the year is E with ID
    or the estimated foreign income for the entity for the year is E with ID
    or the estimated interest for the entity for the year is E with ID
    or the estimated gross sales for the entity for the year is E with ID
    or the estimated gross fees for services for the entity for the year is E with ID
    or the estimated withdrawals from farm management deposits for the entity for the year is E with ID
    or the estimated fuel tax credits for the entity for the year is E with ID
    or the estimated JobKeeper payments for the entity for the year is E with ID.

 % Capital gains are a special case, only included for super funds.
an amount I with an ID is specially included as estimated instalment income for an entity for a year
    if the estimated capital gain for the entity for the year is I with ID
    and the entity is a super fund.

% % --- Specific Exclusions (Types of Excluded Income) ---
an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated GST for the entity for the year is the amount with ID
    or the estimated wine equalisation tax for the entity for the year is the amount with ID
    or the estimated luxury car tax for the entity for the year is the amount with ID.

an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated salary and wages for the entity for the year is the amount with ID
    and the amount with ID was withheld from the entity under the PAYG withholding system.

an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated franking credit for the entity for the year is the amount with ID
    or the estimated deemed dividend for the entity for the year is the amount with ID
    or the estimated exempt income for the entity for the year is the amount with ID
    or the estimated payment under the National Rental Affordability Scheme for the entity for the year is the amount with ID
    or the estimated grant under the energy grants credits scheme for the entity for the year is the amount with ID.

% General capital gains are excluded (the super fund rule above is the exception).
an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated capital gain for the entity for the year is the amount with ID
    and it is not the case that
        the entity is a super fund.

scenario test_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO?
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 0. 
    the estimated tax credits for Australian entity for 2025 are 0.
    6250 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    6250 with idiAE202502 was reported as an instalment on quarter 2 of 2025.
    the estimated dividends for Australian entity for 2025 is 40000 with iddAE2025.
    the estimated royalties for Australian entity for 2025 is 35000 with idrAE2025.
    the estimated capital gain for Australian entity for 2025 is 10000 with idcAE2025. % excluded not a superfund
    the estimated GST for Australian entity for 2025 is 20000 with idgAE2025. % just excluded

query test is:
    the varied amount payable for which quarter for which year by Australian entity is which amount.

scenario ato_1_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity. 
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.

scenario ato_2_quarter_3 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 3.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    1000 with idiAE202502 was reported as a variation on quarter 2 of 2025.

scenario ato_3_quarter_4 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 4.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    1000 with idiAE202502 was reported as a variation on quarter 3 of 2025.
    3000 with idiAE202503 was reported as an instalment on quarter 2 of 2025.

scenario ato_4_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity. 
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    the estimated gross rent for Australian entity for 2025 is 80000 with idegAE2025.

query payable is:
    the varied amount payable for which quarter for which year by which entity is which amount.

% Varying by Rate
query new_rate is:
    the new varied rate for which entity for 2025 is which percentage.

query estimated_tax is:
    the estimated tax for which entity for which year is which amount.

query payg_income is:
    the estimated PAYG instalment income for which entity for which year is which amount.

"). 

/** <examples> To test the rules, these are possible combinations of queries and scenarios
?- show prolog.  % check translation into Prolog`
% basic tests
?- answer(test, with(test_quarter_2)). 
?- answer(test, with(test_quarter_2), le(E), R).
?- answer(payable, with(test_quarter_2), le(E), R).
?- answer(estimated_tax, with(test_quarter_2), le(E), R).
?- answer(payg_income, with(test_quarter_2), le(E), R).
?- answer(new_rate, with(test_quarter_2), le(E), R).
% specific tests starting with payable amount in the given quarter
?- answer(payable, with(ato_1_quarter_2), le(E), R). % use it to test ATO scenario
?- answer(payable, with(ato_2_quarter_3), le(E), R). % use it to test ATO scenario
?- answer(payable, with(ato_3_quarter_4), le(E), R). % use it to test ATO scenario
% calculating new rate
?- answer(new_rate, with(ato_4_quarter_2), le(E), R). % use it to test ATO scenario
*/