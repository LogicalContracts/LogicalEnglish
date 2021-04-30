% Written by Miguel Calejo for LodgeIT, Australia, and AORA, UK; copyright LodgeIT+AORA (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/',[]).

mainGoal(satisfies_maximum_net_asset_value_test(_TFN), "Determine if a given entity satisfies the maximum net value test for CGT assets").


% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

%TODO: flesh out the examples
example('Colin',[scenario([facts], (has_revelant_asset(xxx), whatever))]).
example('Ben',[scenario([facts], has_cgt_assets_net_value_of(123,5000))]).
example('Cool',[scenario([facts],p)]).
example('Lana',[scenario([facts],q)]).
example('Andrew email Feb 5 2021',[
    /* Andrew has net CGT assets 4,000,000, has affiliate with net assets 1,000,000, has connected entity with net CGT assets of 2,000,000 */
    scenario([
        owns(andrew,cgt_asset_1) at myDB1,
        is_cgt_asset(cgt_asset_1) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/",
        is_earnout_cgt_asset_with_value(cgt_asset_1,4000000) at EARNOUT,
        % is_share_in_company(cgt_asset_1,entity) at myDB1 if false,
        % has_to_exclude_asset(andrew,_) if false, %http://localhost:3050/p/tests.pl#tabbed-tab-0 Andrew doesn't want any asset excluded!
        has_affiliated_with(andrew,affiliate1),
        ++ owns(affiliate1,cgt_asset_2), ++ s_net_value_is(cgt_asset_2,1000000),
        is_connected_to(andrew,entity) at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/",
        owns(entity,asset3) at myDB1,
        is_cgt_asset(asset3) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/",
        is_earnout_cgt_asset_with_value(asset3,2000000) at EARNOUT,
        is_used_in_business_of(_,_) at myDB2 if false
        ], not satisfies_maximum_net_asset_value_test(andrew))
    ]) :- EARNOUT="https://www.ato.gov.au/General/Capital-gains-tax/In-detail/Business-assets/Earnout-arrangements-and-CGT/".

% note: referred from cgt_concessions_basic_conditions_sb.pl
satisfies_maximum_net_asset_value_test(TaxPayer) on Date if 
    has_cgt_assets_net_value_of(TaxPayer,Value) on Date and Value =< 6000000.

has_cgt_assets_net_value_of(Person,Value) on Date if 
    %aggregate/3 fails for empty list, so this is what we need to sum:
    aggregate_all(sum(AssetNet), (
        has_revelant_asset(Person,Asset) and is_cgt_asset(Asset) and not has_to_exclude_asset(Person,Asset) and s_net_value_is(Asset,AssetNet) on Date
        ), Value).
    
has_revelant_asset(Person,Asset) if 
    owns(Person,Asset).
has_revelant_asset(Person,Asset) if 
    is_connected_to(Person,Connection) and owns(Connection,Asset).
has_revelant_asset(Person,Asset) if 
    has_affiliated_with(Person,Affiliate) and owns(Affiliate,Asset) and 
    (is_used_in_business_of(Asset,Person) or is_connected_to(Person,Connection) and is_used_in_business_of(Asset,Connection)).
has_revelant_asset(Person,Asset) if 
    has_affiliated_with(Person,Affiliate) and is_connected_to(Affiliate,AffiliateConnection) and owns(AffiliateConnection,Asset) and
    (is_used_in_business_of(Asset,Person) or is_connected_to(Person,Connection) and is_used_in_business_of(Asset,Connection)).

has_to_exclude_asset(Person,Asset) if 
    (is_connected_to(Person,Connection) or has_affiliated_with(Person,Affiliate) and is_connected_to(Affiliate,Connection))
    and is_of_interest_in(Asset,Connection).
has_to_exclude_asset(Person,Asset) if
    is_an_individual(Person) and (
        is_solely_for_personal_use_by(Asset,Person) or has_affiliated_with(Person,Affiliate) and is_solely_for_personal_use_by(Asset,Affiliate)
        or own_home_used_for_private_purposes_only_or_incidental_income_producing
        %TODO: used part of your home to produce assessable income
        or is_part_of_rights_to_amounts_or_assets_of_super_fund_or_approved_deposit_fund(Asset)
        or is_life_insurance_for(Asset,Person)
        ).

%TODO: Effect of look-through earnout rights

s_net_value_is(Asset,Value) on Date if
    if is_earnout_cgt_asset_with_value(Asset,Value) then true 
    else (
        s_market_value_is(Asset,MarketValue) on Date and 
        aggregate_all( sum(Liability), (
            s_type_and_liability_are(Asset,Type,Liability) and 
            % From "Meaning of 'net value'"; contradicts exclusions in "Liabilities to include" !!!
            % from "Liabilities to include"; also refers https://www.ato.gov.au/law/view/document?DocID=TXD/TD200714/NAT/ATO/00001
            Type in [annual_leave,long_service_leave,unearned_income,tax_liabilities, legally_enforceable_debts, legal_or_equitable_obligations] 
            ), Liabilities) on Date 
        and Value is MarketValue-Liabilities
    ).

% "Partner in a partnership": seems just an example



% proxy predicates to other knowledge pages:

owns(TFN,Asset) if
    owns(TFN,Asset) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

is_connected_to(EntityTFN,ConnectedTFN) if
    is_connected_to(EntityTFN,ConnectedTFN) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

is_used_in_business_of(Asset,TFN) if
    is_used_in_business_of(Asset,TFN) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

has_affiliated_with(EntityTFN,AffiliateTFN) on T if 
    has_affiliated_with(EntityTFN,AffiliateTFN) on T
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/".

is_of_interest_in(Asset,Connection) if 
    is_share_in_company(Asset,Connection)
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
is_of_interest_in(Asset,Connection) if 
    is_of_interest_in_trust(Asset,Connection)
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
% Any other cases?  is_of_interest_in(Asset,Connection) if ...

is_cgt_asset(Asset) if
    is_cgt_asset(Asset) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/".

% example of a stub to an external Prolog predicate on module myDB_entities
% could insetad have use_module(myDB_entities) and avoid the qualified call to is_an_individual_on/2
is_an_individual(TPN) on Date because 'according to myDB_entities' :- 
    myDB_entities:is_an_individual_on(TPN,Date).

is_earnout_cgt_asset_with_value(Asset,Value) if 
    is_earnout_cgt_asset_with_value(Asset,Value) 
        at "https://www.ato.gov.au/General/Capital-gains-tax/In-detail/Business-assets/Earnout-arrangements-and-CGT/".

/** <examples>
?- query_with_facts(satisfies_maximum_net_asset_value_test(TaxPayer),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
?- query_with_facts(satisfies_maximum_net_asset_value_test(andrew),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
?- le(LogicalEnglish).
*/
    