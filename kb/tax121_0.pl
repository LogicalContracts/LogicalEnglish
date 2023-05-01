:- module('tax121_0+https://www.law.cornell.edu/uscode/text/26/121',[]).

en("the target language is: prolog. 
    
the templates are:
    
gross income of *a taxpayer* excludes gain from *an exchange* of *a property*.
*an exchange* of *a property* occurs at *a date*.
*a period* of *a number* days ends at *a date*. 
*a property* has been owned by *a taxpayer* as residence for *a set* of periods.
*a property* has been used by *a taxpayer* as residence for *a set* of periods.
*a set* of periods aggregates to *a number* of years.
*a set* of periods is contained in *a bigger period*. 
*a complete set* combines *a first set* and *a second set*.
*a set of periods* as *a taxpayer*'s principal residence aggregates to *a number of* years or more during *a bigger period*.
the amount of gain excluded for *a taxpayer* from *a sale or exchange* under subsection a is *an amount*. 
special rules for joint returns apply to *a taxpayer* and *a property*. 
*a person* is married to *an other person*. 
neither *a taxpayer* nor *a spouse* are ineligible for the benefits of subsection a with respect to *a property* by reason of paragraph 3. 
*a taxpayer* meets the ownership requirements of subsection a with respect to *a property*.
*a taxpayer* meets the ownership requirements of subsection a with respect to *an exchange* of *a property* at *a date*.
*a spouse* meets the use requirements of subsection a with respect to *a property*.
*a taxpayer* meets the use requirements of subsection a with respect to *an exchange* of *a property* at *a date*.
subsection a shall not apply to any sale or exchange of *a property* by *a person*.
*a date* is included in *a period*. 

the knowledge base tax121_0 includes:
    
% (a) Exclusion
% Gross income shall not include gain from the sale or exchange of property if, 
% during the 5-year period ending on the date of the sale or exchange, 
% such property has been owned and used by the taxpayer as 
% the taxpayer’s principal residence for periods aggregating 2 years or more.

gross income of a taxpayer excludes gain from a sale or exchange of a property if
    the taxpayer meets the ownership requirements of subsection a with respect to the sale or exchange of the property at a date
    and the taxpayer meets the use requirements of subsection a with respect to the sale or exchange of the property at the date. 
    
a taxpayer meets the ownership requirements of subsection a with respect to a sale or exchange of a property at a date
    if the sale or exchange of the property occurs at the date
    and a 5-year period of 1825 days ends at the date 
    and the property has been owned by the taxpayer as residence for a property ownership set of periods
    and the property ownership set as the taxpayer's principal residence aggregates to 2 years or more during the 5-year period.
    
a taxpayer meets the use requirements of subsection a with respect to a sale or exchange of a property at a date 
    if the sale or exchange of the property occurs at the date
    and a 5-year period of 1825 days ends at the date 
    and the property has been used by the taxpayer as residence for a property usage set of periods
    and the property usage set as the taxpayer's principal residence aggregates to 2 years or more during the 5-year period.   

a set as a person's principal residence aggregates to a top number of years or more during a bigger period if
    the set of periods aggregates to a number of years
    and the number >= the top number of
    and the set of periods is contained in the bigger period. 

% (b) Limitations
% (1) In general
% The amount of gain excluded from gross income under subsection (a) 
% with respect to any sale or exchange shall not exceed $250,000.
%
% Special rules for joint returns

the amount of gain excluded for a taxpayer from a sale or exchange under subsection a is 250000
    if gross income of the taxpayer excludes gain from the sale or exchange of a property
    and it is not the case that
        special rules for joint returns apply to the taxpayer and the property. 

% A) $500,000 Limitation for certain joint returns
% Paragraph (1) shall be applied 
% by substituting “$500,000” for “$250,000” if—
% (i) either spouse meets the ownership requirements of subsection (a) with respect to such property;
% (ii) both spouses meet the use requirements of subsection (a) with respect to such property; and
% (iii) neither spouse is ineligible for the benefits of subsection (a) 
% with respect to such property by reason of paragraph (3).

the amount of gain excluded for a taxpayer from a sale or exchange under subsection a is 500000
    if gross income of a taxpayer excludes gain from the sale or exchange of a property
    and special rules for joint returns apply to the taxpayer and the property.

special rules for joint returns apply to a taxpayer and a property
    if the taxpayer is married to a spouse
    and the taxpayer meets the ownership requirements of subsection a with respect to the property
    	or the spouse meets the ownership requirements of subsection a with respect to the property
    and the taxpayer meets the use requirements of subsection a with respect to the property
    and the spouse meets the use requirements of subsection a with respect to the property
    and neither the taxpayer nor the spouse are ineligible for the benefits of subsection a with respect to the property by reason of paragraph 3.

a person meets the ownership requirements of subsection a with respect to a property
    if the person meets the ownership requirements of subsection a with respect to a sale or exchange of the property at a date.

a person meets the use requirements of subsection a with respect to a property
    if the person meets the use requirements of subsection a with respect to a sale or exchange of the property at a date. 

neither a taxpayer nor a spouse are ineligible for the benefits of subsection a with respect to a property by reason of paragraph 3
    if subsection a shall not apply to any sale or exchange of the property by the taxpayer
    and subsection a shall not apply to any sale or exchange of the property by the spouse.
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

subsection a shall not apply to any sale or exchange of a property by a taxpayer
    if the sale or exchange of the property occurs at a date
    and a 2-year period of a number of days ends at the date
    and an other sale or exchange of the property occurs at a second date
    and the second date is included in the 2-year period
    and gross income of the taxpayer excludes gain from the other sale or exchange of the property.
      
scenario one is:
    the sale of the house occurs at 2022-06-20.
	the big period of 1825 days ends at 2022-06-20.
    the house has been owned by the taxpayer as residence for the first part of periods. 
    the house has been used by the taxpayer as residence for the second part of periods.
    the first part of periods aggregates to 2 of years. 
    the second part of periods aggregates to 3 of years. 
	the first part of periods is contained in the big period.
	the second part of periods is contained in the big period.
  
query one is:
    gross income of which taxpayer excludes gain from which exchange of which property. 

query two is: 
    the amount of gain excluded for the taxpayer from which sale under subsection a is which amount. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query two with scenario one").
?- answer(one, with(one), le(E), R). 
?- answer(two, with(one), le(E), R). 
*/
