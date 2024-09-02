:- module(sbpp5,[]).

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

the knowledge base sbpp includes:
 
% http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.75.html 
%   
the indirect small business participation percentage 1 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in an intermediate entity is a number MyD
    and a number L1 is the sum of each Pi such that
    	the direct small business participation percentage of the intermediate entity in the other entity is Pi
    and a number L2 is the sum of each Po such that
    	the indirect small business participation percentage 1 of the intermediate entity in the other entity is Po  
    and I = MyD * (L1 + L2). 

the indirect small business participation percentage 2 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in the other entity is I
    or I = 0. 

the indirect small business participation percentage of an entity in an other entity is a percentage I 
    if the indirect small business participation percentage 1 of the entity in the other entity is a percentage I1
    and the indirect small business participation percentage 2 of the entity in the other entity is a percentage I2
    and I = I1 + I2. 

    
%  http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.70.html   
% 1
the direct small business participation percentage of an entity in a company C is a percentage I    
    if  the entity holds the legal and equitable interests in shares in C
    and the percentage of the voting power of the entity in C is a VP
            and it is not the case that
                the entity jointly holds the legal and equitable interests in the shares of C with an other entity
            and I = VP
        or  the percentage of any dividend C may pay is a D
            and I = D
        or  the percentage of any distribution of capital C may make is a DC
            and I = DC
        or  VP is known
            and D is known
            and DC is known
            and I is the smallest among VP and D and DC. 
 
% 2
the direct small business participation percentage of an entity in a trust T is a percentage I
    if the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
            and I = DI
        or  the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is a DC
            and I = DC
        or  DI is known
            and DC is known
            and I is the smallest among DI and DC. 
 
% 3
the direct small business participation percentage of an entity in a trust T is a percentage I
    if it is not the case that
    	the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
            and T makes distribution of income during the relevant year
            and I = DI
        or  the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is a DC
            and T makes distribution of capital during the relevant year
            and I = DC
        or  DI is known
            and DC is known
            and I is the smallest among DI and DC.   
    
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

a percentage S is the smallest among a percentage P1 and a percentage P2
     if P1 =< P2
         and S = P1
     or P2 =< P1
         and S = P2.      
         
the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits for the trustee is X. 

the percentage of any distribution of capital a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity capital profits for the trustee is X.        
                

scenario test is:
    BZJ Pty Ltd makes distribution of income during the relevant year.
    BZJ Pty Ltd makes distribution of capital during the relevant year.
    the percentage of BZJ Pty Ltd capital profits for Tom Edwards is 0.8.
    the percentage of BZJ Pty Ltd revenue profits for Tom Edwards is 0.8.
    Edward FT makes distribution of income during the relevant year.
    Edward FT makes distribution of capital during the relevant year.   
    the percentage of Edward FT capital profits for BZJ Pty Ltd is 0.9.
    the percentage of Edward FT revenue profits for BZJ Pty Ltd is 0.9.
    TUT makes distribution of income during the relevant year.
    TUT makes distribution of capital during the relevant year.
    the percentage of TUT capital profits for Edward FT is 0.6. 
    the percentage of TUT revenue profits for Edward FT is 0.6.      

query indirect is:
	the indirect small business participation percentage of which entity in which entity O is which number.
    
query direct is:
	the direct small business participation percentage of which entity in  which entity O is which number.    

").

/** <examples>
?- answer("direct with scenario one").
?- answer("indirect with scenario one").
?- answer("direct with scenario two").
?- answer("indirect with scenario two").
?- answer(direct, with(TUT), le(E), R).
?- answer("direct with scenario TUT").
?- answer("indirect with scenario TUT").
?- show prolog.
?- answer(indirect, with(simple), le(E), R).
?- answer(direct, with(simple), le(E), R).
*/


