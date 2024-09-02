:- module(sbppscenario4,[]).

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
    the percentage of *an entity* capital profits from *an other entity* is *a number*. 
    the percentage of *an entity* revenue profits from *an other entity* is *a number*. 

the knowledge base sbppscenario4 includes:
 
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
        and I = DC
    or  DI is known
        and DC is known
        and I is the smallest among DI and DC.     

a percentage S is the smallest among a percentage P1 and a percentage P2
     if P1 =< P2
         and S = P1
     or P2 < P1
         and S = P2.    

the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits from the trustee is X. 

the percentage of any distribution of capital a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity capital profits from the trustee is X.
                
scenario 4 is:
the percentage of Tom Edwards capital profits from XYZ Pty Ltd is 1.
the percentage of Tom Edwards revenue profits from XYZ Pty Ltd is 1.    
XYZ Pty Ltd makes distribution of capital during the relevant year. 
%XYZ Pty Ltd makes distribution of income during the relevant year.
%
the percentage of XYZ Pty Ltd capital profits from Axel Family Trust is 0.2.
the percentage of Tom Edwards capital profits from Axel Family Trust is 0.8.
the percentage of XYZ Pty Ltd revenue profits from Axel Family Trust is 0.2.
the percentage of Tom Edwards revenue profits from Axel Family Trust is 0.8.
%Axel Family Trust makes distribution of income during the relevant year.
Axel Family Trust makes distribution of capital during the relevant year.
%
the percentage of Tom Edwards revenue profits from Faber Family Trust is 0.5.
the percentage of Tom Edwards capital profits from Faber Family Trust is 0.5.
the percentage of Mary Edwards revenue profits from Faber Family Trust is 0.5.
the percentage of Mary Edwards capital profits from Faber Family Trust is 0.5.
%Faber Family Trust makes distribution of income during the relevant year.
Faber Family Trust makes distribution of capital during the relevant year.
%
the percentage of Faber Family Trust capital profits from Edward Family Trust is 0.5.
the percentage of Faber Family Trust revenue profits from Edward Family Trust is 0.5.
the percentage of Axel Family Trust capital profits from Edward Family Trust is 0.5.
the percentage of Axel Family Trust revenue profits from Edward Family Trust is 0.5.
%Edward Family Trust makes distribution of income during the relevant year.
Edward Family Trust makes distribution of capital during the relevant year.
%
the percentage of Edward Family Trust capital profits from Tallow Unit Trust is 0.5.
the percentage of Edward Family Trust revenue profits from Tallow Unit Trust is 0.5.
the percentage of James Family Trust capital profits from Tallow Unit Trust is 0.25.
the percentage of James Family Trust revenue profits from Tallow Unit Trust is 0.25.
the percentage of Tom Family Trust capital profits from Tallow Unit Trust is 0.25.
the percentage of Tom Family Trust revenue profits from Tallow Unit Trust is 0.25.    
%Tallow Unit Trust makes distribution of income during the relevant year.
Tallow Unit Trust makes distribution of capital during the relevant year.     
      
query indirect is:
	the indirect small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.
    
query direct is:
	the direct small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.    

").

/** <examples>
?- show prolog.
?- answer(indirect, with(4), le(E), R).
?- answer(direct, with(4), le(E), R).
?- answer("direct with scenario 4").
?- answer("indirect with scenario 4").
*/


