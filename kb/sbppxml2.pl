:- module(sbppxml2,[]).

en("the target language is: prolog. 
    
the templates are:
    the indirect small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    the indirect small business participation percentage 1 of *an entity* in *an other entity* is *a percentage*. 
    the indirect small business participation percentage 2 of *an entity* in *an other entity* is *a percentage*.  
    the direct small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    *an entity* holds the legal and equitable interests in shares in *a company*.
    the percentage of *an entity* of the voting power in *a company* is *a percentage*.
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

the knowledge base sbppxml2 includes:
 
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

%  http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.70.html   


% 3
the direct small business participation percentage of an entity in a trust T is a percentage I
    if it is not the case that
    	the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income T may make to which the entity would be beneficially entitled is I
            and T makes distribution of income during the relevant year
            and it is not the case that
                T makes distribution of capital during the relevant year
        or  the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is I
            and T makes distribution of capital during the relevant year
            and it is not the case that
                T makes distribution of income during the relevant year
        or  the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
            and the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is a DC
            and I is the smallest among DI and DC.      

a percentage S is the smallest among a percentage P1 and a percentage P2
     if P1 =< P2
         and S = P1
     or P2 < P1
         and S = P2.    

a percentage S is the smallest among a percentage P1 and a percentage P2 and a percentage P3
     if P1 =< P2
         and P1 =< P3
         and S = P1
     or P2 =< P1
         and P2 =< P3
         and S = P2
     or P1 >= P2
        and P2 >= P3
        and S = P3. 

the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits from the trustee is X. 

the percentage of any distribution of capital a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity capital profits from the trustee is X.
                
scenario 2 is: 
% Capital
the percentage of Edward Family Trust capital profits from Tallow Unit Trust is 0.5.
the percentage of James Family Trust capital profits from Tallow Unit Trust is 0.25.
the percentage of Tom Family Turst capital profits from Tallow Unit Trust is 0.25.
% Income Revenue
the percentage of Edward Family Trust revenue profits from Tallow Unit Trust is 0.5.
the percentage of James Family Trust revenue profits from Tallow Unit Trust is 0.25.
the percentage of Tom Family Turst revenue profits from Tallow Unit Trust is 0.25.
% Voting
the percentage of Edward Family Trust of the voting power in Tallow Unit Trust is 0.5.
the percentage of James Family Trust of the voting power in Tallow Unit Trust is 0.25.
the percentage of Tom Family Turst of the voting power in Tallow Unit Trust is 0.25.
% distributions made
Tallow Unit Trust makes distribution of  capital  during the relevant year.
Tallow Unit Trust makes distribution of  income  during the relevant year.
% holds shares in
Edward Family Trust holds the legal and equitable interests in shares in Tallow Unit Trust.
James Family Trust holds the legal and equitable interests in shares in Tallow Unit Trust.
Tom Family Turst holds the legal and equitable interests in shares in Tallow Unit Trust.
% Capital
the percentage of Faber Family Trust capital profits from Edward Family Trust is 0.4.
the percentage of Axel Family Trust capital profits from Edward Family Trust is 0.6.
% Income Revenue
the percentage of Faber Family Trust revenue profits from Edward Family Trust is 0.6.
the percentage of Axel Family Trust revenue profits from Edward Family Trust is 0.4.
% Voting
the percentage of Faber Family Trust of the voting power in Edward Family Trust is 0.
the percentage of Axel Family Trust of the voting power in Edward Family Trust is 0.
% distributions made
Edward Family Trust makes distribution of  capital  during the relevant year.
Edward Family Trust makes distribution of  income  during the relevant year.
% holds shares in
Faber Family Trust holds the legal and equitable interests in shares in Edward Family Trust.
Axel Family Trust holds the legal and equitable interests in shares in Edward Family Trust.
% Capital
the percentage of Tom Edwards capital profits from Faber Family Trust is 0.5.
the percentage of Mary Edwards capital profits from Faber Family Trust is 0.5.
% Income Revenue
the percentage of Tom Edwards revenue profits from Faber Family Trust is 0.6666666666666666.
the percentage of Mary Edwards revenue profits from Faber Family Trust is 0.3333333333333333.
% Voting
the percentage of Tom Edwards of the voting power in Faber Family Trust is 0.
the percentage of Mary Edwards of the voting power in Faber Family Trust is 0.
% distributions made
Faber Family Trust makes distribution of  capital  during the relevant year.
Faber Family Trust makes distribution of  income  during the relevant year.
% holds shares in
Tom Edwards holds the legal and equitable interests in shares in Faber Family Trust.
Mary Edwards holds the legal and equitable interests in shares in Faber Family Trust.
% Capital
the percentage of Tom Edwards capital profits from Axel Family Trust is 0.6666666666666666.
the percentage of XYZ Pty Ltd capital profits from Axel Family Trust is 0.3333333333333333.
% Income Revenue
the percentage of Tom Edwards revenue profits from Axel Family Trust is 0.75.
the percentage of XYZ Pty Ltd revenue profits from Axel Family Trust is 0.25.
% Voting
the percentage of Tom Edwards of the voting power in Axel Family Trust is 0.
the percentage of XYZ Pty Ltd of the voting power in Axel Family Trust is 0.
% distributions made
Axel Family Trust makes distribution of  capital  during the relevant year.
Axel Family Trust makes distribution of  income  during the relevant year.
% holds shares in
Tom Edwards holds the legal and equitable interests in shares in Axel Family Trust.
XYZ Pty Ltd holds the legal and equitable interests in shares in Axel Family Trust.
% Capital
the percentage of Tom Edwards capital profits from XYZ Pty Ltd is 1.
% Income Revenue
the percentage of Tom Edwards revenue profits from XYZ Pty Ltd is 1.
% Voting
the percentage of Tom Edwards of the voting power in XYZ Pty Ltd is 0.
% distributions made
XYZ Pty Ltd makes distribution of  capital  during the relevant year.
XYZ Pty Ltd makes distribution of  income  during the relevant year.
% holds shares in
Tom Edwards holds the legal and equitable interests in shares in XYZ Pty Ltd.


query indirect is:
	the indirect small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.
    
query direct is:
	the direct small business participation percentage of Tom Edwards in Tallow Unit Trust is which number.    

").

/** <examples>
?- show prolog.
?- answer(indirect, with(2), le(E), R).
?- answer(direct, with(2), le(E), R).
?- answer("direct with scenario 2").
?- answer("indirect with scenario 2").
*/


