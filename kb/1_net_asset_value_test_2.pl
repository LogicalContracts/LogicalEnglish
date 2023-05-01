:-module('1_net_asset_value_test_2+https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20',[]).

en("the target language is: prolog. 
    % inspired by https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
    % based on https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20 
    
the templates are:
    *a tax payer* satisfies maximum net asset value test. 
    the net value of the CGT assets of *an entity* is *an amount*.
    the assets after disregarding from the CGT assets of *an entity* are in *a set*.
    *a value* is obtained from the market values of *a set* of *an entity*.
    *a liability* is obtained from the liabilities of *a set* of *an entity*. 
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
    *an asset* must be included under subsection 2A.
    *an asset* is the market value of *a dwelling*.
    *an asset* is an ownership interest in *a dwelling*. 
    *an asset* is a right to or to any part of any allowance annuity.
    *an asset* is a capital amount payable out of a superannuation fund or an approved deposit fund.
    *an asset* is a policy of insurance on the life of an individual.
    *an asset* in *a set* of *an entity* has *an amount* as its market value.
    *an asset* in *a set* of *an entity* has *an amount* as liability.
    *a percentage* is reasonable as stated by paragraph 118-190 with respect to *an asset*.
    for *a fraction* out of *a number* times *a section* percent *an asset* was used for producing assessable income.
    *a percentage* for *an asset*'s assesable income. 
    *a first entity* controls *a second entity*.
    *an entity* owns *a number* percent of *an asset* in *a second entity*.
    *an asset* is a share.
    the commissioner is satisfied that *a situation*. 
    *an entity* is controlled by *a second entity*. 
    all shares carry the same voting rights. 
    the commissioner is satisfied that *a solution*.
    *an entity* is affiliate of *a second entity*. 
    *an affiliate* is an individual.
    *an affiliate* is a company.
    *an affiliate* is a trust.
    *an affiliate* is a partnership.
    *an affiliate* is a superannuation fund.
    *an affiliate* acts in accordance with directions from *an entity*.
    *an affiliate* acts in concert with *an entity*.

the knowledge base 1_net_asset_value_test_2 includes:

% Maximum net asset value test
% 
% SECTION 152-20   Meaning of net value of the CGT assets  
% View history reference
% Meaning of net value of the CGT assets
% 
% 152-20(1)   
%
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
    and a total market value is obtained from the market values of the remaining set of the entity
    and a total liability is obtained from the liabilities of the remaining set of the entity
    and a provisions total is obtained from provisions in [i, ii, iii, iv]  
    and the final amount is the total market value - the total liability + the provisions total.  

a total market value is obtained from the market values of a set of an entity
    if the total market value is the sum of each amount such that
        an asset in the set of the entity has the amount as its market value. 

a total liability is obtained from the liabilities of a set of an entity 
    if the total liability is the sum of each amount such that
        an asset in the set of the entity has the amount as liability.

a total in provisions is obtained from provisions in a list 
    if the total in provisions is the sum of each amount such that
        an item is in the list
        and the amount is a provision for the item.

an asset in a set of an entity has an amount as its market value 
    if the asset is in the set 
    and it is not the case that
    	the asset must be included under subsection 2A
    and the amount is the market value of the asset. 

an asset in a set of an entity has an amount as liability
    if the asset is in the set
    and it is not the case that
    	the asset must be included under subsection 2A
    and the amount is a liability of the asset.

%    Assets to be disregarded
%
%    152-20(2)  
%    In working out the net value of the CGT assets of an entity: 

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
        the asset must be included under subsection 2A
    and the asset is the market value of a residence
            and the residence is a dwelling
            and the residence is the entity's main residence or an extension because of section 118 120
        or the asset is an ownership interest in the residence
            and the residence is a dwelling
            and the residence is the entity's main residence or an extension because of section 118 120.

an asset of an entity is to be disregarded
    if the entity is an individual
    and the asset is a right to or to any part of any allowance annuity 
        or the asset is a capital amount payable out of a superannuation fund or an approved deposit fund.

an asset of an entity is to be disregarded
    if the entity is an individual
    and the asset is a policy of insurance on the life of an individual. 
  
% Individual's dwelling
%
% 152-20(2A)  
%    
% In working out the net value of the CGT assets of an individual, if:  
%     
%   (a) a *dwelling of the individual, an *ownership interest in such a dwelling or 
% any relevant adjacent land, was used, during all or part of the *ownership period of 
% the dwelling, by the individual to produce assessable income to a particular extent; and
%    
%   (b) the individual satisfied paragraph 118-190(1)(c) 
% (about interest deductibility) at least to some extent;
%   
% include such amount as is reasonable having regard to the extent to 
% which that paragraph was satisfied.
% Note:
%     
% The net value of the CGT assets of the individual will be reduced 
% by the same proportion of the individual's liabilities related to 
% the dwelling, ownership interest or adjacent land.

% https://www.ato.gov.au/law/view/document?LocID=%22PAC%2F19970038%2F118-190(1)%22

%  Partial exemption rules
%
% SECTION 118-190   Use of dwelling for producing assessable income  
%
% 118-190(1)  
% You get only a partial exemption for a *CGT event that happens 
% in relation to a *dwelling or your *ownership interest in it if:
%
% (a) apart from this section, because the dwelling was your main 
% residence or someone else's during a period:
%
%   (i) you would not make a *capital gain or *capital loss from the event; or 
%
%   (ii) you would make a lesser capital gain or loss than if this Subdivision had not applied; and 
%
% (b) the dwelling was used for the *purpose of producing assessable income 
% during all or a part of that period; and
% 
% (c) if you had incurred interest on money borrowed to *acquire the dwelling, 
% or your ownership interest in it, you could have deducted some or all of that interest. 

an asset must be included under subsection 2A
    if the asset is a dwelling
    and for a fraction out of a number times a section percent the asset was used for producing assessable income. 

an asset in a set of an entity has an amount as its market value 
    if the entity is an individual
    and the asset is a dwelling
    and a percentage is reasonable as stated by paragraph 118-190 with respect to the asset
    and a basic amount is the market value of the asset
    and the amount is the basic amount * the percentage / 100. 

an asset in a set of an entity has an amount as liability
    if the entity is an individual
    and the asset is a dwelling
    and a percentage is reasonable as stated by paragraph 118-190 with respect to the asset
    and a basic amount is a liability of the asset
    and the amount is the basic amount * the percentage / 100. 

an amount is reasonable as stated by paragraph 118-190 with respect to a given asset
    if  the amount is the sum of each percentage such that
        the percentage for the given asset's assesable income.

a percentage for a asset's assesable income
    if  for a fraction out of a number times a section percent the asset was used for producing assessable income
    and the percentage is the section * the fraction / the number. 

%A tax payer satisfies maximum net asset value test at a date
A tax payer satisfies maximum net asset value test
    if the net value of the CGT assets of the tax payer is a value
    and the value =< 6000000.

% https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Connected-entities/
% An entity is connected with another entity if:
% 
%    either entity controls the other entity, or
%    both entities are controlled by the same third entity.
an entity is connected with an other entity
    if the entity controls the other entity
    or the other entity controls the entity
    or a third entity controls the entity
        and the third entity controls the other entity. 

% An entity controls another entity if it or its affiliate (or all of them together):
%
%     owns, or has the right to acquire ownership of, interests in the other entity 
%     that give the right to receive at least 40% (the control percentage) of  
%         any distribution of income or capital by the other entity, or
%         if the other entity is a partnership, the net income of the partnership or
     
%     if the other entity is a company, owns, or has the right to acquire ownership of, 
% equity interests in the company that give at least 40% of the voting power in the company.


% If an entity's control percentage in another entity is at least 40% but less than 50%, 
% the Commissioner may determine that the first entity does not control the other entity if he 
% is satisfied that a third entity (not including any affiliates of the first entity) controls the other entity. 
a first entity controls a second entity
    if the first entity owns a number percent of the shares in the second entity
    and the first entity is different from the second entity
    and all shares carry the same voting rights 
    and the number >= 50
        or the number < 50
           and the number > 40
           and it is not the case that    
    			the commissioner is satisfied that
                    the second entity is controlled by a third entity
    			and the first entity is different from the third entity
           and it is not the case that
               the third entity is affiliate of the first entity. 

% adapted from previous affiliates LE document example:
an entity is affiliate of an affiliate 
    if the entity is different from the affiliate
    and the affiliate is an individual 
        or the affiliate is a company
    and it is not the case that 
        the affiliate is a trust
    and it is not the case that 
        the affiliate is a partnership
    and it is not the case that 
        the affiliate is a superannuation fund
    and the affiliate acts in accordance with directions from the entity
        or the affiliate acts in concert with the entity.

% https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
%
% Example 1
%
% Colin operates a newsagency as a sole trader.
%
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

%  https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
%
% Example 2
%
% Ben owns a house that has a market value of $750,000 just before applying the net assets test. 
% Ben has owned the house for 12 years:
%
%    for the first three years, 20% of it was used for producing assessable income
%    for the following two years, 40% was used for producing assessable income
%    for two years, it was used solely as a main residence
%    for the last five years, 10% was used for producing assessable income.
%
% Ben’s dwelling has had 15.8% income-producing use:
% 
%    3 ÷ 12 years × 20 = 5.0
%    2 ÷ 12 years × 40 = 6.7
%    2 ÷ 12 years × 0 = 0.0
%    5 ÷ 12 years × 10 = 4.1
%
% Ben will include $118,500 in his net assets ($750,000 × 15.8%).
%
% Ben has a liability of $500,000 attached to the house, therefore 15.8% ($79,000) 
% of the liability is also included in the calculation of net assets.

scenario Ben is:
    Ben is an individual.
    the CGT assets of Ben are in [the house].
    the house is in the assets of Ben.
    750000 is the market value of the house.
    500000 is a liability of the house.
    the house is a dwelling. 
    for 3 out of 12 times 20 percent the house was used for producing assessable income.
    for 2 out of 12 times 40 percent the house was used for producing assessable income.
    for 2 out of 12 times 0 percent the house was used for producing assessable income.
    for 5 out of 12 times 10 percent the house was used for producing assessable income.

% https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
% 
% Example 3
% 
% Cool Tool Pty Ltd is selling its business. The assets and liabilities of the company are as follows:
% 
% Assets:
%	
% Plant and machinery   $1,500,000
% Freehold premises     $3,500,000
%                       __________
% Total assets          $5,000,000
% 
% Liabilities:
% 	
% Mortgage (secured over the premises)  $2,000,000
% Provision for leave of employees        $500,000
% Provision for rebates                   $200,000
% Provision for possible damages payout   $100,000
%                                       __________
% Total liabilities                     $2,800,000
% 
% Net assets:                           $2,200,000
%
% The net value of the CGT assets of the company is calculated as follows:
% Assets:
%	
% Plant and machinery   $1,500,000
% Freehold premises     $3,500,000
%                       __________
% Total assets          $5,000,000
% 
% Liabilities:
% 	
% Mortgage (secured over the premises)  $2,000,000
% Provision for leave of employees        $500,000
%                                       __________
% Total liabilities                     $2,500,000
%
% Net assets:                           $2,500,000

scenario Cool Tool Pty Ltd is:
    the CGT assets of Cool Tool Pty Ltd are in [plant and machinery, freehold premises].
    plant and machinery is in the assets of Cool Tool Pty Ltd.
    freehold premises is in the assets of Cool Tool Pty Ltd.
    1500000 is the market value of plant and machinery. 
    3500000 is the market value of freehold premises.
    2000000 is a liability of freehold premises.
    500000 is a provision for i. 

%-- https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
%
% Example: Personal use assets (4)
%
% The market value of Lana’s CGT assets is:
% 
% Land used in business                 $50,000
% Business goodwill                    $200,000
% Trading stock	                       $100,000
% Plant                                 $50,000
% Boat (used solely for personal use)   $50,000
% Home                                 $600,000
%                                     _________
% Total                             $ 1,050,000
% 
% Lana borrowed $20,000 to buy the boat.
% 
% When working out the net value of her CGT assets, Lana does not include:
% 
%     the market value of her boat ($50,000)
%     the liability for the boat.
% 
% Lana uses 50 percent of her home exclusively for income-producing activity. 
% She includes 50 percent of the value of her home, representing the 
% income-producing percentage, and does not include the other 50 percent ($300,000).
% 
% Therefore, the net value of her CGT assets is:
% 
% $1,050,000 − $350,000 = $700,000

scenario Lana is:
    Lana is an individual.
    the CGT assets of Lana are in [land, goodwill, stock, plant, boat, home].
    land is in the assets of Lana.
    goodwill is in the assets of Lana.
    stock is in the assets of Lana.
    plant is in the assets of Lana.
    boat is in the assets of Lana.
    home is in the assets of Lana.
    50000 is the market value of land.
    200000 is the market value of goodwill.
    100000 is the market value of stock.
    50000 is the market value of plant.
    50000 is the market value of boat.
    600000 is the market value of home. 
    20000 is a liability of boat.
    for 12 out of 12 times 50 percent home was used for producing assessable income.
    home is a dwelling.
    boat is being used solely for the personal use and enjoyment of Lana or Lana's affiliate. 

%   Example 5
%
 % Olivia and Jill conduct a professional practice in partnership. 
 % As they each have a 50% interest in the partnership, they each control 
 % the partnership. Therefore, the partnership is connected with each partner, 
 % and Olivia and Jill are each connected with the partnership.
scenario Olivia and Jill is:
    Olivia controls the partnership.
    Jill controls the partnership. 


% Example 7
%
% Lachlan owns 48 percent of the shares in Ayoubi Art Supplies. He plays no part in the day-to-day or 
% strategic decision-making of the business. Daniel owns 42 percent of the shares in the company. 
% The remaining 10 percent of shares are beneficially owned by a third shareholder who does not take 
% part in the management of the business. All shares carry the same voting rights and Daniel makes 
% all day-to-day and strategic decisions for the company. Even though Lachlan owns 48 percent of 
% the shares in Ayoubi Art Supplies, he would not be taken to control the company if the 
% Commissioner was satisfied that the company is controlled by Daniel.   
scenario Ayoubi Art Supplies is:
    Lachlan is an individual.
    Daniel is an individual.
    Ayoubi Art Supplies is a company. 
    Lachlan owns 48 percent of the shares in Ayoubi Art Supplies. 
    Daniel owns 42 percent of the shares in Ayoubi Art Supplies.
    Ayoubi Art Supplies acts in accordance with directions from Daniel.
    all shares carry the same voting rights.     
    the shares is a share. 
    the commissioner is satisfied that
        Ayoubi Art Supplies is controlled by Daniel. 

query one is:
    which payer satisfies maximum net asset value test. 

query two is:
    the net value of the CGT assets of which entity is which amount.

query three is:
    which partner is connected with which other partner. 

query four is:
    which first entity controls which second entity.

"). 

/** <examples>
 * 
?- answer(one, with('Colin'), le(R), An). 
?- show prolog.
?- answer(two, with('Colin')).
?- answer(one, with('Ben'), le(R), An).
?- answer(two, with('Ben')).
?- answer(one, with('Cool Tool Pty Ltd'), le(R), An).
?- answer(two, with('Cool Tool Pty Ltd')).
?- answer(two, with('Lana')).
?- answer(one, with('Lana'), le(R), An).
?- answer(three, with('Olivia and Jill')).
?- answer(three, with('Olivia and Jill'), le(R), An).
?- answer(four, with('Ayoubi Art Supplies')).
?- answer(four, with('Ayoubi Art Supplies'), le(R), An).
*/
