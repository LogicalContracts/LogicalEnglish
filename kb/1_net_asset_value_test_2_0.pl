:-module('1_net_asset_value_test_2+https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20',[]).

en("the target language is: prolog. 
    % inspired by https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/
    % based on https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20 
    
    the templates are:
    *an asset* has *a type* of liability that costs *an amount*,
    *an amount* is a provision on *an asset*'s liabilities of some type that is in *a set*,
    *a first amount* is the market value of *an asset* at *a date*,
    *an asset* is an earnout cgt asset valued at *a value* according to other legislation,
    *an asset* is an earnout cgt asset valued at *a value*,
    *an asset* is a cgt asset,
    *a tax payer* satisfies maximum net asset value test at *a date*,
    *a person* has cgt assets net value of *a value* at *a date*,
    *a person* has relevant asset *an asset*,
    *a person* owns *an asset*,
    *a taxpayer* is connected to *a connection* according to other legislation,
    *a person* is connected to *a connection*,
    *a taxpayer* is affiliated with *an affiliate* at *a time* according to other legislation,
    *a taxpayer* is affiliated with *an affiliate* at *a time*,
    *a person* is affiliated with *an affiliate*,
    *an asset* is used in business of *a person*,
    *a person* has to exclude asset *an asset*,
    *a value* is the net value of *an asset* at *a date*,
    *an asset* belongs to *a connection*,
    *a person* is an individual,
    *an asset* is solely for personal use by *a person*,
    it is for home use for private purposes only including incidental income producing,
    *an asset* is part of rights to amounts or assets of super fund or approved deposit fund,
    *an asset* is life insurance for *a person*,
    *an asset* is used in business of *a taxpayer* according to other legislation,
    *a person* owns *an asset* according to other legislation,
    *an asset* is share in company of *a connection* according to other legislation,
    *an asset* is of interest in trust of *a connection* according to other legislation,
    *an asset* is a cgt asset according to other legislation,
    *a tax payer* satisfies maximum net asset value test, 
    the net value of the CGT assets of *an entity* is *an amount*,
    the assets to be disregarded from the CGT assets of *an entity* are in *a set*,
    *a value* is obtained from the market values of *a set*,
    *a liability* is obtained from the liabilities of *a set*,
    *a value* is obtained from provisions in *a list*,
    *an amount* is the market value of *an asset*,
    *an amount* is a liability of *an asset*,
    *an amount* is a provision for *an item*,
    *a set* of *an entity* are in *a list*,
    assets to be disregarded are filtered out from *a list* into *a new list*,
    *an asset* is to be disregarded. 


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
    the assets to be disregarded from the CGT assets of the entity are in a remaining set
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
% In working out the net value of the CGT assets of an entity:
% (a) disregard *shares, units or other interests (except debt) in another entity that 
% is *connected with the first-mentioned entity or with an *affiliate of the first-mentioned 
% entity, but include any liabilities related to any such shares, units or interests; and 

the assets to be disregarded from the CGT assets of an entity are in a new set 
    if the CGT assets of the entity are in a list
    and assets to be disregarded are filtered out from the list into the new set. 

assets to be disregarded are filtered out from [] into [].
assets to be disregarded are filtered out from an input set into an output set 
    if the input set has an asset as head before an input rest
    and the asset is to be disregarded 
    and assets to be disregarded are filtered out from the input rest into the output set.
assets to be disregarded are filtered out from an input set into an output set
    if the input set has an asset as head before an input rest
    and the output set has the asset as head before an output rest
    and it is not the case that
        the asset is to be disregarded 
    and assets to be disregarded are filtered out from the input rest into the output rest.

% earlier rules (adapted to the above)

%A tax payer satisfies maximum net asset value test at a date
A tax payer satisfies maximum net asset value test
    if the net value of the CGT assets of the tax payer is a value
    and the value =< 6000000.

% A person has cgt assets net value of a value at a date
%     if  the value is the sum of each asset net such that
%         the person has relevant asset an asset
%         and the asset is a cgt asset
%         and it is not the case that
%             the person has to exclude asset the asset
%             and the asset net is the net value of the asset at the date
%    .

% A person has relevant asset an asset
%     if the person owns the asset.

% A person has relevant asset an asset
%     if the person is connected to a connection
%     and the connection owns the asset.

% A person has relevant asset an asset
%     if the person is affiliated with an affiliate
%     and the affiliate owns the asset
%     and the asset is used in business of the person
%         or the person is connected to a connection
%             and the asset is used in business of the connection
%     .

% A person has relevant asset an asset
%     if the person is affiliated with an affiliate
%     and the affiliate is connected to an affiliate connection
%     and the affiliate connection owns the asset
%     and the asset is used in business of the person
%         or the person is connected to a connection
%             and the asset is used in business of the connection
% .

% A person has to exclude asset an asset
%     if the person is connected to a connection
%     or the person is affiliated with an affiliate
%         and the affiliate is connected to the connection
%         and the asset belongs to the connection.

% A person has to exclude asset an asset
%     if the person is an individual
%     and the asset is solely for personal use by the person
%         or the person is affiliated with an affiliate
%             and the asset is solely for personal use by the affiliate
%                 or it is for home use for private purposes only including incidental income producing
%                 or  the asset is part of rights to amounts or assets of super fund or approved deposit fund
%                 or  the asset is life insurance for the person
% .

% An amount is the net value of an asset at a date
%     if  a first amount is the market value of the asset at the date
%     and a second amount is the sum of each individual amount such that
%         the individual amount is a provision on the asset's liabilities of some type that is in [annual leave, long service leave, unearned income, tax liabilities, legally enforceable debts, legal or equitable obligations]
%         at the date
%     and the amount is the first amount - the second amount.

% An amount is a provision on an asset's liabilities of some type that is in a set
%   if the asset has a type of liability that costs the amount
%   and the type is in the set.

% A taxpayer owns an asset
%     if the taxpayer owns the asset according to other legislation.

% A taxpayer is connected to a connection
%     if the taxpayer is connected to the connection according to other legislation.

% An asset is used in business of a taxpayer
%     if the asset is used in business of the taxpayer according to other legislation.

% A taxpayer is affiliated with an affiliate at a time
%     if the taxpayer is affiliated with the affiliate at the time according to other legislation.

% An asset belongs to a connection
%     if the asset is share in company of the connection according to other legislation.

% An asset belongs to a connection
%     if  the asset is of interest in trust of the connection according to other legislation.

% An asset is a cgt asset
%     if the Asset is a cgt asset according to other legislation.

% %a TPN is an individual at a Date
% %    if myDB_entities : is_an_individual_on(the TPN,the Date).

% an asset is an earnout cgt asset valued at a value
%     if  the asset is an earnout cgt asset valued at the value according to other legislation.

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
