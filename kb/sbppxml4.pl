:- module(sbppxml4,[]).

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
    the relevant year is *a year*. 

the knowledge base sbppxml4 includes:
 
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
    	the indirect small business participation percentage 1 of the entity in the other entity is Po
    	or the direct small business participation percentage of the entity in the other entity is Po.

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
                
scenario 4 is:
% Capital statements
the percentage of Tom Hotskin capital profits from Hubble Unit Trust is 0.1.
the percentage of Hotskin Family Trust capital profits from Hubble Unit Trust is 0.9.
% Income Revenue statements
the percentage of Tom Hotskin revenue profits from Hubble Unit Trust is 0.5.
the percentage of Hotskin Family Trust revenue profits from Hubble Unit Trust is 0.5.
% Voting statements
the percentage of Tom Hotskin of the voting power in Hubble Unit Trust is 0.
the percentage of Hotskin Family Trust of the voting power in Hubble Unit Trust is 0.
% distributions made
Hubble Unit Trust makes distribution of  capital  during the relevant year.
Hubble Unit Trust makes distribution of  income  during the relevant year.
% holds shares in
Tom Hotskin holds the legal and equitable interests in shares in Hubble Unit Trust.
Hotskin Family Trust holds the legal and equitable interests in shares in Hubble Unit Trust.
% Section: 
the relevant year is 2024.
% Capital statements
% Income Revenue statements
% Voting statements
% distributions made
% holds shares in
XYZ Pty Ltd holds the legal and equitable interests in shares in Hotskin Family Trust.
Tom Hotskin holds the legal and equitable interests in shares in Hotskin Family Trust.
% Section: 
the relevant year is 2023.
% Capital statements
the percentage of XYZ Pty Ltd capital profits from Hotskin Family Trust is 0.85.
the percentage of Tom Hotskin capital profits from Hotskin Family Trust is 0.15.
% Income Revenue statements
the percentage of XYZ Pty Ltd revenue profits from Hotskin Family Trust is 0.5.
the percentage of Tom Hotskin revenue profits from Hotskin Family Trust is 0.5.
% Voting statements
the percentage of XYZ Pty Ltd of the voting power in Hotskin Family Trust is 0.
the percentage of Tom Hotskin of the voting power in Hotskin Family Trust is 0.
% distributions made
Hotskin Family Trust makes distribution of  capital  during the relevant year.
Hotskin Family Trust makes distribution of  income  during the relevant year.
% holds shares in
XYZ Pty Ltd holds the legal and equitable interests in shares in Hotskin Family Trust.
Tom Hotskin holds the legal and equitable interests in shares in Hotskin Family Trust.


query indirect is:
	the indirect small business participation percentage of Tom Hotskin in Hubble Unit Trust is which number.
    
query direct is:
	the direct small business participation percentage of Tom Hotskin in Hubble Unit Trust is which number.    

").

/** <examples>
?- show prolog.
?- answer(indirect, with(4), le(E), R).
?- answer(direct, with(4), le(E), R).
?- answer("direct with scenario 4").
?- answer("indirect with scenario 4").
*/


