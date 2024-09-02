:- module(sbppscenario3,[]).

en("the target language is: prolog. 
    
the templates are:
    the indirect small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    the indirect small business participation percentage 1 of *an entity* in *an other entity* is *a percentage*. 
    the indirect small business participation percentage 2 of *an entity* in *an other entity* is *a percentage*.  
    the direct small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    *an entity* holds the legal and equitable interests in shares in *a company*.
    the percentage of the voting power of *an entity* in *a company* is *a percentage*.
    *an entity* jointly holds the legal and equitable interests in the shares of *a company* with *an other entity*.
    the percentage of any dividend *a company* may pay is *a percentage*.
    the percentage of any distribution of capital *a company* may make is *a percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage* and *a third percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage*.
    *an entity*  has entitlements to all the income and capital of *a trust*.
    the percentage of any distribution of income *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    the percentage of any distribution of capital *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    *a trustee* makes distribution of income during the relevant year.
    *a trustee* makes distribution of capital during the relevant year.
    the percentage of *an entity* capital profits for *an other entity* is *a number*. 
    the percentage of *an entity* revenue profits for *an other entity* is *a number*. 

the knowledge base sbppscenario3 includes:
 
% http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.75.html 
%   
the indirect small business participation percentage 1 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in an intermediate entity is a number MyD
    and a number L1 is the sum of each Pi such that
    	the direct small business participation percentage of the intermediate entity in the other entity is Pi
    and a number L2 is the sum of each Po such that
    	the indirect small business participation percentage 1 of the intermediate entity in the other entity is Po  
    and I = MyD * (L1 + L2). 

the indirect small business participation percentage of an entity in an other entity is a percentage I 
    if I is the sum of each Po such that
    	the indirect small business participation percentage 1 of the entity in the other entity is Po.

% 3
the direct small business participation percentage of an entity in a trust T is a percentage I
    if the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
        and T makes distribution of income during the relevant year
        and I = DI
    or  the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is a DC
        and T makes distribution of capital during the relevant year
        and I = DC.     

the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits for the trustee is X. 

the percentage of any distribution of capital a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity capital profits for the trustee is X.
                
scenario 3 is:
the percentage of Tom Edwards capital profits for BZJ Pty Ltd is 0.8.
BZJ Pty Ltd makes distribution of capital during the relevant year.
the percentage of Tom Edwards capital profits for XYZ Pty Ltd is 0.8.
XYZ Pty Ltd makes distribution of capital during the relevant year.
the percentage of BZJ Pty Ltd capital profits for Edward Family Trust is 0.5.
the percentage of XYZ Pty Ltd capital profits for Edward Family Trust is 0.5.
Edward Family Trust makes distribution of capital during the relevant year.
the percentage of Edward Family Trust capital profits for Tallow Unit Trust is 0.5.
Tallow Unit Trust makes distribution of capital during the relevant year. 
      
query indirect is:
	the indirect small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.
    
query direct is:
	the direct small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.    

").

/** <examples>
?- show prolog.
?- answer(indirect, with(3), le(E), R).
?- answer(direct, with(3), le(E), R).
?- answer("direct with scenario 3").
?- answer("indirect with scenario 3").
*/



