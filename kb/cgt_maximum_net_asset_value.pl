% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
myURL("https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/").

mainGoal(satisfies_maximum_net_asset_value_test(TFN), "Determine if a given entity satisfies the maximum net value test for CGT assets").


% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.

%TODO: flesh out the examples
example("Colin",[state([facts], (relevant_asset(...), ...))]).
example("Ben",[state([facts], cgt_assets_net_value(...))]).
example("Cool",[state([facts],...)])
example("Lana",[state([facts],...)])

% note: referred from cgt_concessions_basic_conditions_sb.pl
satisfies_maximum_net_asset_value_test(TFN) on Date if 
    cgt_assets_net_value(TFN,Value) on Date and Value =< 6000000.

relevant_asset(You,Asset) if 
    owns(You,Asset).
relevant_asset(You,Asset) if 
    connected_to(You,Connection) and owns(Connection,Asset).
relevant_asset(You,Asset) if 
    affiliate(You,Affiliate) and owns(Affiliate,Asset) and 
    (is_used_in_business_of(Asset,You) or connected_to(You,Connection) and is_used_in_business_of(Asset,Connection)).
relevant_asset(You,Asset) if 
    affiliate(You,Affiliate) and connected_to(Affiliate,AffiliateConnection) and owns(AffiliateConnection,Asset) and
    (is_used_in_business_of(Asset,You) or connected_to(You,Connection) and is_used_in_business_of(Asset,Connection)).

asset_to_exclude(You,Asset) if 
    (connected_to(You,Connection) or affiliate(You,Affiliate) and connected_to(Affiliate,Connection))
    and is_interest_in(Asset,Connection).
asset_to_exclude(You,Asset) if
    is_individual(You) and (
        solely_for_personal_use_by(Asset,You) or affiliate(You,Affiliate) and solely_for_personal_use_by(Asset,Affiliate)
        or own_home_used_for_private_purposes_only_or_incidental_income_producing
        %TODO: used part of your home to produce assessable income
        or rights_to_ammounts_or_assets_of_super_fund_or_approved_deposit_fund(Asset)
        or is_life_insurance_for(Asset,You)
        ).

%TODO: Effect of look-through earnout rights

net_value(Asset,Value) on Date if
    if is_earnout_cgt_asset(Asset,Value) then true 
    else (
        market_value(Asset,Market) on Date and 
        aggregate( sum(Liability), (
            liability(Asset,Type,Liability) and 
            Type in [
                annual_leave,long_service_leave,unearned_income,tax_liabilities, % From "Meaning of 'net value'"; contradicts exclusions in "Liabilities to include" !!!
                legally_enforceable_debts, legal_or_equitable_obligations % from "Liabilities to include"; also refers https://www.ato.gov.au/law/view/document?DocID=TXD/TD200714/NAT/ATO/00001
                ] 
            ), Liabilities) on Date
    ).

% "Partner in a partnership": seems just an example

cgt_assets_net_value(You,Value) on Date if 
    aggregate(sum(AssetNet), (
        relevant_asset(You,Asset) and is_cgt_asset(Asset) and not asset_to_exclude(You,Asset) and net_value(Asset,AssetNet) on Date
        ), Value).


% proxy predicates to other knowledge pages:

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

is_interest_in(Asset,Connection) if 
    is_share_in_company(Asset)
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
is_interest_in(Asset,Connection) if 
    is_interest_in_trust(Asset)
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
% Any other cases?  is_interest_in(Asset,Connection) if ...

is_cgt_asset(Asset) if
    is_cgt_asset(Asset) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/".

is_individual(TPN) if 
    is_individual(TPN) at myDB_entities.

is_earnout_cgt_asset(Asset,Value) if 
    is_earnout_cgt_asset(Asset,Value) 
        at "https://www.ato.gov.au/General/Capital-gains-tax/In-detail/Business-assets/Earnout-arrangements-and-CGT/".