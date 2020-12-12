% Written by Miguel Calejo for NobleAccounting & Associates, Australia; copyright NobleAccounting 2020
% Encoding of https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the cgt event time, "now"
:- use_module(syntax).
:- discontiguous (if)/2.

% external info; these may eventually be existing databases
partner_in_partnership(Tfn123,Partnership789) if
    partner_in_partnership(Tfn123,Partnership789) at myDB1. % cf. https://www.ato.gov.au/Business/Starting-your-own-business/Before-you-get-started/Choosing-your-business-structure/Partnership/
membership_interest_in_partnership(MemberTFN,PartnerShipTFN,RightOrInterest) if
    membership_interest_in_partnership(MemberTFN,PartnerShipTFN,RightOrInterest) at myDB1.
asset_is_right_or_interest_to_amount_of_income_or_capital_of_partnership(Asset,Partnership) if
    asset_is_right_or_interest_to_amount_of_income_or_capital_of_partnership(Asset,Partnership) at myDB2. %TODO: grok this!
asset_is_right_or_interest_to_AMOUNT_CALCULATED_BY_REF_TO_PARTNER_ENTITLEMENT_to_amount_of_income_or_capital_of_partnership(Asset,Partnership) if
    asset_is_right_or_interest_to_AMOUNT_CALCULATED_BY_REF_TO_PARTNER_ENTITLEMENT_to_amount_of_income_or_capital_of_partnership(Asset,Partnership) at myDB2.
is_share_in_company(Asset) if 
    is_share_in_company(Asset) at myDB1.
is_interest_in_trust(Asset) if 
    is_interest_in_trust(Asset) at myDB1.
does_not_carry_on_business_except_as_a_partner(TFN) if 
    does_not_carry_on_business_except_as_a_partner(TFN) at myDB2.
is_used_in_business_of(Asset,TFN) if 
    is_used_in_business_of(Asset,TFN) at myDB2.
owns(TFN,Asset) if 
    owns(TFN,Asset) at myDB1.

% external rulesets/knowledge sources;
has_aggregated_turnover(TFN,Turnover) if has_aggregated_turnover(TFN,Turnover) 
    at "https://www.ato.gov.au/business/small-business-entity-concessions/eligibility/aggregation/".
additional_conditions(Asset) if additional_conditions(Asset)
    at "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Extra-conditions-if-the-CGT-asset-is-a-share-or-trust-interest/".
active_asset_test(Asset) if active_asset_test(Asset) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/active-asset-test/". 
satisfies_maximum_net_asset_value_test(TFN) if satisfies_maximum_net_asset_value_test(TFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/".
is_a_small_business_entity(TFN) if is_a_small_business_entity(TFN) at
    "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
affiliate(EntityTFN,AffiliateTFN) if affiliate(EntityTFN,AffiliateTFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/".
connected_to(EntityTFN,ConnectedTFN) if connected_to(EntityTFN,ConnectedTFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/".
is_an_interest_in_partnership_asset(Asset,PartnershipTFN) if is_an_interest_in_partnership_asset(Asset,PartnershipTFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/partner-in-a-partnership---using-the-small-business-entity-test/?anchor=Partnership_assets".


%%%% Some local rules defining core "actors" of the page from the event structure

% Tax File Number abstracts from entity type
you(TFN) if cgt_event(_,_,_,TFN). % "you" is convenient because it ties to the original regulation text
% asset with richer structure? fluid nature, e.g. number of shares? let's abstract for now
theAsset(ID) if cgt_event(ID,_When,_Type,_Owner).
now(Moment) if cgt_event(_,Moment,_,_).

%%%% The juice of that web page, aligned with its sentences within reason; what it actually means/regulates

% Provenance of the knowledge we're encoding in this knowledge source/module
myURL("https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/").

% goals / use cases: 
% - Determine if a cgt_event fullfills the basic conditions for CGT concessions

basicConditions if 
    step1 and step2 and step3 and step4.

step1 if    % first, ilustrate a rule style where KB URLs are inline:
    you(Y) 
    and is_a_small_business_entity(Y)  
    and has_aggregated_turnover(Y,T) 
    and  T< 2000000.
step1 if    % now rules referring to the glue predicates above:
    you(Y) and does_not_carry_on_business_except_as_a_partner(Y) and 
    theAsset(A) and is_used_in_business_of(A,E) and is_a_small_business_entity(E) and
    (affiliate(Y,E) or connected_to(E,Y)). 
    % see https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/passively-held-assets/
step1 if
    you(Y) and partner_in_partnership(Y,P) and 
    is_a_small_business_entity(P) and theAsset(A) and
    % See https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/partner-in-a-partnership---using-the-small-business-entity-test/?anchor=Partnership_assets#Partnership_assets
    ( is_an_interest_in_partnership_asset(A,P) 
        or 
        owns(Y,A) and not is_an_interest_in_partnership_asset(A,P) and is_used_in_business_of(A,P) ).
step1 if
    you(Y) and satisfies_maximum_net_asset_value_test(Y). % 
    % actually the following would intrude within the realm of the referred page:
    % you(Y) and cgt(_,EventMoment,_,Y) and immediately_before(TestMoment,EventMoment) and satisfies_maximum_net_asset_value_test(Y) on TestMoment.
    % see https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/

step2 if 
    theAsset(A) and active_asset_test(A). 

step3 if
    theAsset(A),
    (if (is_share_in_company(A) or is_interest_in_trust(A)) must additional_conditions(A)). 

step4 if
    cgt_event(A,When,Type,Y) and
    ( if 
        member(Type, [creation,transfer,variation,ending]) and
        ( asset_is_right_or_interest_to_amount_of_income_or_capital_of_partnership(A,P) or 
            asset_is_right_or_interest_to_AMOUNT_CALCULATED_BY_REF_TO_PARTNER_ENTITLEMENT_to_amount_of_income_or_capital_of_partnership(A,P))
        then
        (if Type==ending then immediately_before(MIWhen,When) else immediately_before(When,MIWhen)) and
        membership_interest_in_partnership(Y,P,A) on MIWhen
    ) 
    and after(When ,'20180508T19:30'). 
step4 if 
    cgt_event(_Type,_RightOrInterest,_Y,When) and not after(When ,'20180508T19:30').


% optional declarations to omit some predicates from explanations:
irrelevant_explanation(you(_)).
irrelevant_explanation(theAsset(_)).
irrelevant_explanation(now(_)).


%%%% EXAMPLE DATA

% Asset ID, Date, Type, Owner TFN
cgt_event(asset1,'2019-12-31',ending,tfn1). % this date is paramount to ALL predicates! TFN of OWNER?

% dummy at(Predicate,KnowledgeSource) implementation with examples; this will later miggrate to other files
partner_in_partnership(tfn1,partnershipTFN5) at myDB1. 
membership_interest_in_partnership(tfn2,partnershipTFN6,roriAsset1) at myDB1.
asset_is_right_or_interest_to_amount_of_income_or_capital_of_partnership(roriAsset1,partnershipTFN6) at myDB2. 
asset_is_right_or_interest_to_AMOUNT_CALCULATED_BY_REF_TO_PARTNER_ENTITLEMENT_to_amount_of_income_or_capital_of_partnership(roriAsset2,partnershipTFN6) at myDB2.
is_share_in_company(asset6) at myDB1.
is_interest_in_trust(asset7) at myDB1.
does_not_carry_on_business_except_as_a_partner(tfn1) at myDB2.
is_used_in_business_of(asset1,partnershipTFN5) at myDB2.
owns(tfn1,asset1) at myDB1.

%has_aggregated_turnover(tfn1,1000000) at _.
additional_conditions(asset1) at _.
active_asset_test(asset1) at _.
satisfies_maximum_net_asset_value_test(tfn1) at _.
%is_a_small_business_entity(tfn1) at _.
affiliate(tfn1,affiliateTFN2) at _.
connected_to(tfn1,connectionTFN3) at _.
is_an_interest_in_partnership_asset(asset1,partnershipTFN4) at _.

/** <examples>
?- query(basicConditions,Unknowns).
*/