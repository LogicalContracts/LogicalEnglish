% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
myURL("https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/").

% goals / use cases: 
% - Determine if a given entity satisfies the maximum net value test for CGT assets

% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.

% note: referred from cgt_concessions_basic_conditions_sb.pl
satisfies_maximum_net_asset_value_test(TFN) on Date if 
    cgt_assets_net_value(TFN,Value) on Date and Value =< 6000000.

relevant_owner(You,You).
relevant_owner(You,Connection) if 
    connected_to(You,Connection).
relevant_owner(You,Affiliate) if 
    affiliate(You,Affiliate).
relevant_owner(You,AffiliateConnection) if 
    affiliate(You,Affiliate) and connected_to(Affiliate,AffiliateConnection).

relevant_asset_user(...) if ...

%net_value(Asset,Value) on Date

owns(TFN,Asset) if
    owns(TFN,Asset) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

connected_to(EntityTFN,ConnectedTFN) if
    connected_to(EntityTFN,ConnectedTFN) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

is_used_in_business_of(Asset,TFN) if
    is_used_in_business_of(Asset,TFN) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

affiliate(EntityTFN,AffiliateTFN) if 
    affiliate(EntityTFN,AffiliateTFN) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/".
