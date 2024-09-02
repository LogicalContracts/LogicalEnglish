:- module(sbpp2,[]).

en("the target language is: prolog. 
    
the templates are:
    *an entity* s indirect small business participation percentage in *an other entity* is *a percentage*. 
    *an entity* s direct small business participation percentage in *an other entity* is *a percentage*.
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

the knowledge base sbpp includes:
    
an entity s indirect small business participation percentage in an other entity is a percentage I 
    if the entity s direct small business participation percentage in an intermediate entity is a number MyD
    and a number IT is the sum of each other percentage such that
    	the intermediate entity s direct small business participation percentage in the other entity is a number DD
    	and intermediate entity s indirect small business participation percentage in the other entity is a number ID
    	and the other percentage = DD + ID
    and I = MyD * IT. 
    
%  http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.70.html   
% 1
an entity s direct small business participation percentage in a company C is a percentage I    
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
an entity s direct small business participation percentage in a trust T is a percentage I
    if the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income the trustee may make to which the entity would be beneficially entitled is a DI
            and I = DI
        or  the percentage of any distribution of capital the trustee may make to which the entity would be beneficially entitled is a DC
            and I = DC
        or  DI is known
            and DC is known
            and I is the smallest among DI and DC. 
 
% 3
an entity s direct small business participation percentage in a trust T is a percentage I
    if it is not the case that
    	the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income a trustee may make to which the entity would be beneficially entitled is a DI
            and the trustee makes distribution of income during the relevant year
            and I = DI
        or  the percentage of any distribution of capital a trustee may make to which the entity would be beneficially entitled is a DC
            and the trustee makes distribution of capital during the relevant year
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
    
scenario one is:
	a s direct small business participation percentage in b is 0.20. 
    b s direct small business participation percentage in c is 0.60. 
    
scenario two is:
    a holds the legal and equitable interests in shares in c.
    the percentage of the voting power of a in c is 0.20.
    
scenario three is:
    a holds the legal and equitable interests in shares in c.
    a jointly holds the legal and equitable interests in the shares of d with x.
    the percentage of the voting power of a in c is 0.20.
    the percentage of any dividend c may pay is 0.20.
    the percentage of any distribution of capital c may make is 0.25.
    
    
scenario tallow is:
    Edwards holds the legal and equitable interests in shares in tallow.
    the percentage of any dividend tallow may pay is 0.20.
    the percentage of any distribution of capital tallow may make is 0.50. 
    tallow has entitlements to all the income and capital of BZI.
    the percentage of any distribution of income BZI may make to which tallow would be beneficially entitled is 0.50.
    the percentage of any distribution of capital BZI may make to which tallow would be beneficially entitled is a 0.50. 
    BZI has entitlements to all the income and capital of Edwards.
    the percentage of any distribution of income BZI may make to which Edwards would be beneficially entitled is 0.50.
    the percentage of any distribution of capital BZI may make to which Edwards would be beneficially entitled is a 0.80.


query indirect is:
	which entity s indirect small business participation percentage in which other entity is which number.
    
query direct is:
	which entity s direct small business participation percentage in  which other entity is which number.    

").

/** <examples>
?- answer("direct with scenario one").
?- answer("indirect with scenario one").
?- answer("direct with scenario two").
?- answer("indirect with scenario two").
?- answer(direct, with(tallow), le(E), R).
?- answer("direct with scenario tallow").
?- show prolog.
*/

