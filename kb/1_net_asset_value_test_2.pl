:-module('1_net_asset_value_test_2+https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20',[]).

en("the target language is: prolog. 
    % inspired by https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
    % based on https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20 
    
the templates are:
    *a tax payer* satisfies maximum net asset value test. 
    the net value of the CGT assets of *an entity* is *an amount*.
    the assets after disregarding from the CGT assets of *an entity* are in *a set*.
    *a value* is obtained from the market values of *a set*.
    *a liability* is obtained from the liabilities of *a set*.
    *a value* is obtained from provisions in *a list*.
    *an amount* is the market value of *an asset*.
    *an amount* is a liability of *an asset*.
    *an amount* is a provision for *an item*.
    *a set* of *an entity* are in *a list*.
    the assets of *an entity* to be disregarded are filtered out from *a list* into *a new list*.
    *an asset* of *an entity* is to be disregarded. 
    *an asset* is in the assets of *an entity*.  
    *an entity* is connected with *an other entity*.
    *an entity* is an affiliate of *an other entity*. 
    *an entity* is an individual.
    *an asset* is being used solely for the personal use and enjoyment of *an entity* or *an entity*'s affiliate.
    *an asset* is a dwelling.
    *an asset* is an ownership interest in a dwelling.
    *an asset* is *an entity*'s main residence or an extension because of section 118 120.
    *an asset* is an amount included under subsection 2A.
    *an asset* is the market value of *a dwelling*.
    *an asset* is an ownership interest in *a dwelling*. 
    *an asset* is a right to or to any part of any allowance annuity.
    *an asset* is a capital amount payable out of a superannuation fund or an approved deposit fund.
    *an asset* is a policy of insurance on the life of an individual.


the knowledge base 1_net_asset_value_test_2 includes:

% The net value of the CGT assets of an entity is the amount (whether positive, negative or nil) 
% obtained by subtracting from the sum of the *market values of those assets the sum of:
% (a) the liabilities of the entity that are related to the assets; and
% (b) the following provisions made by the entity:
%     (i) provisions for annual leave; 
%     (ii) provisions for long service leave; 
%     (iii) provisions for unearned income; 
%     (iv) provisions for tax liabilities. 

the net value of the CGT assets of an entity is a final amount if
    the assets after disregarding from the CGT assets of the entity are in a remaining set
    and a total market value is obtained from the market values of the remaining set
    and a total liability is obtained from the liabilities of the remaining set
    and a provisions total is obtained from provisions in [i, ii, iii, iv]  
    and the final amount is the total market value - the total liability + the provisions total.  

a total market value is obtained from the market values of a remaining set
    if the total market value is the sum of each amount such that
        an asset is in the remaining set
        and the amount is the market value of the asset. 

a total liability is obtained from the liabilities of a remaining set
    if the total liability is the sum of each amount such that
        an asset is in the remaining set
        and the amount is a liability of the asset.

a total in provisions is obtained from provisions in a list 
    if the total in provisions is the sum of each amount such that
        an item is in the list
        and the amount is a provision for the item.

% 
% Assets to be disregarded
% 152-20(2)  

the assets after disregarding from the CGT assets of an entity are in a new set 
    if the CGT assets of the entity are in a list
    and the assets of the entity to be disregarded are filtered out from the list into the new set. 

the assets of an entity to be disregarded are filtered out from [] into [].
the assets of an entity to be disregarded are filtered out from an input set into an output set 
    if the input set has an asset as head before an input rest
    and the asset of the entity is to be disregarded 
    and the assets of the entity to be disregarded are filtered out from the input rest into the output set.
the assets of an entity to be disregarded are filtered out from an input set into an output set
    if the input set has an asset as head before an input rest
    and the output set has the asset as head before an output rest
    and it is not the case that
        the asset of the entity is to be disregarded 
    and the assets of the entity to be disregarded are filtered out from the input rest into the output rest.

% In working out the net value of the CGT assets of an entity:
% (a) disregard *shares, units or other interests (except debt) in another entity that 
% is *connected with the first-mentioned entity or with an *affiliate of the first-mentioned 
% entity, but include any liabilities related to any such shares, units or interests; and     
    
an asset of an entity is to be disregarded
    if the asset is in the assets of an other entity 
    and the other entity is connected with the entity
        or the other entity is connected with a third entity
           and the third entity is an affiliate of the entity. 
    
% b) if the entity is an individual, disregard:
%    (i) assets being used solely for the personal use and enjoyment of the individual, 
% or the individual's *affiliate (except a *dwelling, or an *ownership interest in a dwelling, 
% that is the individual's main residence, including any adjacent land to which the main 
% residence exemption can extend because of section 118-120 ); and 
%
%    (ii) except for an amount included under subsection (2A), the *market value of a dwelling,
% or an ownership interest in a dwelling, that is the individual's main residence 
% (including any relevant adjacent land); and 
%
%   (iii) a right to, or to any part of, any allowance, annuity 
% or capital amount payable out of a *superannuation fund or an *approved deposit fund; and 
%
%   (iv) a right to, or to any part of, an asset of a superannuation fund 
% or of an approved deposit fund; and 
%
%   (v) a policy of insurance on the life of an individual. 
    
an asset of an entity is to be disregarded
    if the entity is an individual
    and the asset is being used solely for the personal use and enjoyment of the entity or the entity's affiliate
    and it is not the case that
    	the asset is a dwelling 
        or the asset is an ownership interest in a dwelling
        or the asset is the entity's main residence or an extension because of section 118 120. 

an asset of an entity is to be disregarded
    if the entity is an individual
    and it is not the case that
        the asset is an amount included under subsection 2A
        or the asset is the market value of a dwelling
            and the dwelling is the entity's main residence or an extension because of section 118 120
        or the asset is an ownership interest in a dwelling
            and the dwelling is the entity's main residence or an extension because of section 118 120.

an asset of an entity is to be disregarded
    if the entity is an individual
    and the asset is a right to or to any part of any allowance annuity 
        or the asset is a capital amount payable out of a superannuation fund or an approved deposit fund.

an asset of an entity is to be disregarded
    if the entity is an individual
    and the asset is a policy of insurance on the life of an individual. 
  
% earlier rules (adapted to the above)

%A tax payer satisfies maximum net asset value test at a date
A tax payer satisfies maximum net asset value test
    if the net value of the CGT assets of the tax payer is a value
    and the value =< 6000000.

% Colin operates a newsagency as a sole trader.
% Colin's son, Simon, carries on his own florist business, which is unrelated to the newsagency business. 
% Simon owns the land and building from which the newsagency is conducted and leases it to Colin. 
% Simon also owns 100 percent of the shares in Simco Pty Ltd, which carries on a separate business. 
% Simon is connected with Simco Pty Ltd because he controls the company. 
% Simon regularly consults Colin for advice in his business affairs and acts according 
% to Colin’s wishes – therefore, Simon is Colin’s affiliate.

% To determine whether he satisfies the maximum net asset value test, 
% Colin includes the market value of the land and building owned by Simon 
% (because it is used in his newsagency business) but does not include Simon’s other assets 
% used in his florist business (because they are not used in the newsagency business). 
% Nor does Colin include Simco’s assets, because those assets are not used in his business 
% and Simco Pty Ltd is only connected because of his affiliate, Simon.

%     (i) provisions for annual leave; 
%     (ii) provisions for long service leave; 
%     (iii) provisions for unearned income; 
%     (iv) provisions for tax liabilities. 

scenario Colin is:
    the CGT assets of Colin are in [the land, the building].
    the land is in the assets of Simon.
    the building is in the assets of Simon.
    Simon is connected with Simco Pty Ltd.
    Simon is an affiliate of Colin.
    10000000 is the market value of the land.
    10000000 is the market value of the building.
    5000000 is a liability of the land.
    5000000 is a liability of the building.
    2500000 is a provision for i.
    2500000 is a provision for ii.
    2500000 is a provision for iii.
    2500000 is a provision for iv. 

query Colin is:
    which payer satisfies maximum net asset value test. 

query one is:
    the net value of the CGT assets of which entity is which amount.

"). 

/** <examples>
 * 
?- answer('Colin', with('Colin'), le(R), An). 
?- show prolog.
?- answer(one, with('Colin')).
*/
