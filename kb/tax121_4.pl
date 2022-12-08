:- module('tax121+https://www.law.cornell.edu/uscode/text/26/121',[]).

en("the target language is: prolog. 
    
the templates are:
    
gross income of *a taxpayer* excludes gain from *an exchange* of *a property* at *a date*.
*an exchange* of *a property* occurs at *a date*.
*a period* of *a number* years ends at *a date*. 
*a property* has been owned by *a taxpayer* as residence for *a set* of periods.
*a property* has been used by *a taxpayer* as residence for *a set* of periods.
*a set* of periods aggregates to *a number* of years.
*a set* of periods is contained in *a long period*. 
*a set of periods* as *a taxpayer*'s principal residence aggregates to *a number of* years or more during *a bigger period*.
the amount of gain excluded for *a taxpayer* from *a sale or exchange* under subsection (a) shall not exceed *an amount*. 
special rules for joint returns apply to *a taxpayer* and *a property*. 
*a person* is married to *an other person*. 
neither *a taxpayer* nor *a spouse* are ineligible for the benefits of subsection (a) with respect to *an exchange* of *a property* by reason of paragraph 3. 
*a taxpayer* meets the ownership requirements of subsection (a) with respect to *an exchange* of *a property* at *a date*.
*a taxpayer* meets the use requirements of subsection (a) with respect to *an exchange* of *a property* at *a date*.
subsection (a) shall not apply to *a sale or exchange* of *a property* by *a person*.
*a date* is included in *a period*. 
*a property* has been owned by *a taxpayer* for periods aggregating *a number* years or more during *a period*.
*a property* has been used by *a taxpayer* as principal residence for periods aggregating *a number* years or more during *a period*.
*a set* of periods of *a taxpayer* owing *a property* aggregates to *a number* of years.
*a set* of periods of *a taxpayer* using *a property* as principal residence aggregates to *a number* of years.

the knowledge base tax121 includes:
    
% (a) Exclusion
% Gross income shall not include gain from the sale or exchange of property if, 
% during the 5-year period ending on the date of the sale or exchange, 
% such property has been owned and used by the taxpayer as 
% the taxpayer’s principal residence for periods aggregating 2 years or more.

gross income of a taxpayer excludes gain from a sale or exchange of a property at a date if
    the taxpayer meets the ownership requirements of subsection (a) with respect to the sale or exchange of the property at the date
    and the taxpayer meets the use requirements of subsection (a) with respect to the sale or exchange of the property at the date
    and it is not the case that
    	subsection (a) shall not apply to the sale or exchange of the property by the taxpayer.
    
a taxpayer meets the ownership requirements of subsection (a) with respect to a sale or exchange of a property at a date
    if the sale or exchange of the property occurs at the date
    and a period of 5 years ends at the date 
    and the property has been owned by the taxpayer for periods aggregating 2 years or more during the period.
    
a taxpayer meets the use requirements of subsection (a) with respect to a sale or exchange of a property at a date 
    if the sale or exchange of the property occurs at the date
    and a period of 5 years ends at the date 
    and the property has been used by the taxpayer as principal residence for periods aggregating 2 years or more during the period.   

a property has been owned by a taxpayer for periods aggregating a limit number years or more during a given period
    if a set of periods of the taxpayer owing the property aggregates to a total number of years
    and the total number >= the limit number
    and the set of periods is contained in the given period. 

a property has been used by a taxpayer as principal residence for periods aggregating a limit number years or more during a given period
    if a set of periods of the taxpayer using the property as principal residence aggregates to a total number of years
    and the total number >= the limit number
    and the set of periods is contained in the given period.

% (b) Limitations
% (1) In general
% The amount of gain excluded from gross income under subsection (a) 
% with respect to any sale or exchange shall not exceed $250,000.
%
% Special rules for joint returns

the amount of gain excluded for a taxpayer from a sale or exchange under subsection (a) shall not exceed 250000
    if gross income of the taxpayer excludes gain from the sale or exchange of a property at a date
    and it is not the case that
        special rules for joint returns apply to the taxpayer and the property. 

% A) $500,000 Limitation for certain joint returns
% Paragraph (1) shall be applied 
% by substituting “$500,000” for “$250,000” if—
% (i) either spouse meets the ownership requirements of subsection (a) with respect to such property;
% (ii) both spouses meet the use requirements of subsection (a) with respect to such property; and
% (iii) neither spouse is ineligible for the benefits of subsection (a) 
% with respect to such property by reason of paragraph (3).

the amount of gain excluded for a taxpayer from a sale or exchange under subsection (a) shall not exceed 500000
    if gross income of the taxpayer excludes gain from the sale or exchange of a property at a date
    and special rules for joint returns apply to the taxpayer and the property.

special rules for joint returns apply to a taxpayer and a property
    if the taxpayer is married to a spouse
    and the taxpayer meets the ownership requirements of subsection (a) with respect to a sale or exchange of the property at a date
    	or the spouse meets the ownership requirements of subsection (a) with respect to the sale or exchange of the property at the date
    and the taxpayer meets the use requirements of subsection (a) with respect to the sale or exchange of the property at the date
    and the spouse meets the use requirements of subsection (a) with respect to the sale or exchange of the property at the date
    and neither the taxpayer nor the spouse are ineligible for the benefits of subsection (a) with respect to the sale or exchange of the property by reason of paragraph 3.

neither a taxpayer nor a spouse are ineligible for the benefits of subsection (a) with respect to a sale or exchange of a property by reason of paragraph 3
    if subsection (a) shall not apply to the sale or exchange of the property by the taxpayer
    and subsection (a) shall not apply to the sale or exchange of the property by the spouse.
%
% (B) Other joint returns
% 
% If such spouses do not meet the requirements of subparagraph (A),
% the limitation under paragraph (1) shall be the sum of the limitations under paragraph (1) 
% to which each spouse would be entitled if such spouses had not been married. 
% For purposes of the preceding sentence, each spouse shall be treated as 
% owning the property during the period that either spouse owned the property.

% (3) Application to only 1 sale or exchange every 2 years
%
% Subsection (a) shall not apply to any sale or exchange by the taxpayer if, 
% during the 2-year period ending on the date of such sale or exchange, 
% there was any other sale or exchange by the taxpayer to which subsection (a) applied.

subsection (a) shall not apply to a sale or exchange of a property by a taxpayer
    if the sale or exchange of the property occurs at a date
    and an other sale or exchange of a second or the same property occurs at a second date
    and the other sale or exchange is different from the sale or exchange
    and a period of 2 years ends at the date
    and the second date is included in the period
    and gross income of the taxpayer excludes gain from the other sale or exchange of the second or the same property at the second date. 
      
scenario one is:
    the sale of the house occurs at 2022-06-20.
	the given period of 5 years ends at 2022-06-20.
    first set of periods of the taxpayer owing the house aggregates to 2 of years.
    first set of periods is contained in the given period. 
    second set of periods of the taxpayer using the house as principal residence aggregates to 3 of years.
    second set of periods is contained in the given period.

scenario two is:
    the sale of the house occurs at 2022-06-20.
	the given period of 5 years ends at 2022-06-20.
    the testing period of 2 years ends at 2022-06-20.
    the taxpayer is married to the spouse. 
    first set of periods of the taxpayer owing the house aggregates to 2 of years.
    first set of periods is contained in the given period. 
    second set of periods of the taxpayer using the house as principal residence aggregates to 3 of years.
    second set of periods is contained in the given period.
    third set of periods of the spouse using the house as principal residence aggregates to 4 of years.
    third set of periods is contained in the given period. 
    
scenario three is:
    the sale of the house occurs at 2022-06-20.
	the given period of 5 years ends at 2022-06-20.
    the testing period of 2 years ends at 2022-06-20.
    the taxpayer is married to the spouse. 
    first set of periods of the taxpayer owing the house aggregates to 2 of years.
    first set of periods is contained in the given period. 
    second set of periods of the taxpayer using the house as principal residence aggregates to 3 of years.
    second set of periods is contained in the given period.
    third set of periods of the spouse using the house as principal residence aggregates to 4 of years.
    third set of periods is contained in the given period. 
    some exchange of the second house occurs at 2021-01-01.
    2021-01-01 is included in the testing period. 
    the previous period of 5 years ends at 2021-01-01.
	the second given period of 5 years ends at 2021-01-01.
    the second testing period of 2 years ends at 2021-01-01.
    fourth set of periods of the taxpayer owing the second house aggregates to 2 of years. 
    fourth set of periods is contained in the previous period. 
    fifth set of periods of the taxpayer using the second house as principal residence aggregates to 2 of years.
    fifth set of periods is contained in the previous period. 
    sixth set of periods of the spouse using the second house as principal residence aggregates to 2 of years.
    sixth set of periods is contained in the previous period. 
 
query one is:
    gross income of which taxpayer excludes gain from which exchange of which property at which date. 

query two is: 
    the amount of gain excluded for the taxpayer from which sale under subsection (a) shall not exceed which amount. 

query three is:
    subsection (a) shall not apply to which sale or exchange of which property by which taxpayer.
").

/** <examples>
?- answer("query one with scenario one").
?- answer("query two with scenario one").
?- answer(one, with(one), le(E), R). 
?- answer(two, with(one), le(E), R). 
?- answer("query two with scenario two").
?- answer(two, with(two), le(E), R).
?- answer(three, with(three), le(E), R).
?- answer(two, with(three), le(E), R).
*/
