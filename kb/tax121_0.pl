:- module('tax121+https://www.law.cornell.edu/uscode/text/26/121',[]).

en("the target language is: prolog. 
    
the templates are:
    
gross income of *a taxpayer* excludes gain from *an exchange* of *a property*.
*an exchange* occurs at *a date*.
*a period* ends at *a date*. 
the size of *a period* is *a number* days. 
*a property* has been owned by *a taxpayer* as residence for *a set* of periods.
*a property* has been used by *a taxpayer* as residence for *a set* of periods.
*a set* of periods aggregates to *a number* of years.
*a set* of periods is contained in *a bigger period*. 
*a complete set* combines *a first set* and *a second set*.
*a set of periods* as *a taxpayer*'s principal residence aggregates to *a number of* years or more during *a bigger period*.

the knowledge base tax121 includes:
    
% (a) Exclusion
% Gross income shall not include gain from the sale or exchange of property if, 
% during the 5-year period ending on the date of the sale or exchange, 
% such property has been owned and used by the taxpayer as 
% the taxpayer’s principal residence for periods aggregating 2 years or more.

gross income of a taxpayer excludes gain from a sale or exchange of a property if
    the sale or exchange occurs at a date
    and a 5-year period ends at the date
    and the size of the 5-year period is 1825 days
    and the property has been owned by the taxpayer as residence for a first set of periods
    and the property has been used by the taxpayer as residence for a second set of periods
    and a complete set combines the first set and the second set
    and the complete set as the taxpayer's principal residence aggregates to 2 years or more during the 5-year period.  
   
a set as a person's principal residence aggregates to a top number of years or more during a bigger period if
    the set of periods aggregates to a number of years
    and the number >= the top number of
    and the set of periods is contained in the bigger period. 
      
scenario one is:
    the sale occurs at 1.
	the big period ends at 1.
    the size of the big period is 1825 days. 
    the house has been owned by the taxpayer as residence for the first part of periods. 
    the house has been used by the taxpayer as residence for the second part of periods.
    [1, 2] combines the first part and the second part. 
    [1, 2] of periods aggregates to 3 of years. 
	[1, 2] of periods is contained in the big period.
  
query one is:
    gross income of which taxpayer excludes gain from which exchange of which property. 

").

/** <examples>
?- answer("query one with scenario one").
?- answer(one, with(one), le(E), R). 
*/
