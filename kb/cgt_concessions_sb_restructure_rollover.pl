% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:- module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover',[]).


mainGoal(rollover_applies(_ID,_Asset,_When,_Transferor,_TransfereesList), "Determine if a asset transfer event can be treated as a restructure rollover").

is_asserted(Information) :-
    assert(Information). 

:- thread_local transfer_event/5.
rollover_applies(ID,Asset,When,Transferor,TransfereesList) :-
    is_asserted(transfer_event(ID,Asset,When,Transferor,TransfereesList)),
    rollover_applies.


% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"


% example(Title,Sequence).  Sequence is a list of scenario(FactChanges,TrueConclusion)
% An example ilustrates and tests:
%TODO: replace pseudo code text by predicates
example( 'Ultimate ownership unchanged', [
    % initial facts and condition:
    scenario(['penny runs a business B','B has assets A'], is_ultimately_owned_by(A,'Penny',1)),
    % new facts and condition:
    scenario(['penny has trust T', transfer_event(ID,A,When,B,[T])], is_ultimately_owned_by(A,'Penny'))
    ]).
example( 'Changed share of ownership', [
    % initial facts and condition:
    scenario(['Amy, Joanna and Remy run a delivery business B as equal partners','B has assets A'], 
        true),
    % new facts and condition:
    scenario(['Amy, Joanna and Remy establish company C, different shares', transfer_event(ID,A,When,B,[C])], 
        not rollover_applies),
    % alternative facts and another condition:
    scenario(['Amy, Joanna and Remy establish company C, different shares', 'Amy, Joanna and Remy establish company C, equal shares'], 
        rollover_applies)
    ]).
example( 'Andrew email Feb 4 2021', [
    /* Company transfers its assets to partners in a partnership on 1 July 2020. Company has turnover of 5,300,000. 
    Company is a Small Business Entity i.e. the transferor & the partners in a partnership i.e. transferee is also a small business entity.)
    Assets are - Goodwill, Trading stock, Plant & equipment, revenue assets
    */
    scenario([
        owns(company1,company1_goodwill) on T at BASICS if T @=< BEFORE,
        owns(company1,company1_trading_stock) on T at BASICS if T @=< BEFORE,
        owns(company1,company1_plant_and_equipment) on T at BASICS if T @=< BEFORE,
        owns(company1,company1_revenue_asset) on T at BASICS if T @=< BEFORE,
        %TODO: add time here, and below...?
        is_a_partner_in_partnership_with(andrew,company1) at BASICS, is_a_partner_in_partnership_with(miguel,company1) at BASICS, 
        has_an_aggregated_turnover_of(company1,5300000) at AGGREGATION,
        has_an_aggregated_turnover_of(andrew,1000000) at AGGREGATION,
        has_an_aggregated_turnover_of(miguel,500000) at AGGREGATION,
        is_a_small_business_entity(company1) at SBE, is_a_small_business_entity(andrew) at SBE, is_a_small_business_entity(miguel) at SBE,
        transfer_event(EVENT,company1_goodwill,WHEN,company1,[andrew,miguel]),
        % with the rule as it is below, no point in injecting more than one event:
        %transfer_event(EVENT2,company1_trading_stock,WHEN,company1,[andrew,miguel]),
        %transfer_event(EVENT3,company1_plant_and_equipment,WHEN,company1,[andrew,miguel]),
        %transfer_event(EVENT4,company1_revenue_asset,WHEN,company1,[andrew,miguel]),
        owns(company1,company1_goodwill) on T at BASICS if T @>= WHEN,
        owns(company1,company1_trading_stock) on T at BASICS if T @>= WHEN,
        owns(company1,company1_plant_and_equipment) on T at BASICS if T @>= WHEN,
        owns(company1,company1_revenue_asset) on T at BASICS if T @>= WHEN,
        part_of_genuine_restructure(EVENT) at "https://www.ato.gov.au/law/view/document?DocID=COG/LCG20163/NAT/ATO/00001&PiT=99991231235958",
        is_used_in_business_of(company1_goodwill,company1) at BASICS,
        is_of_asset_type(company1_goodwill,trading_stock) at myDb17,
       '\'s_family_trust_is_in'(_,_) if false
        | MoreFacts
        ], rollover_applies(EVENT))
    ]) :- 
        % for mere convenience, Prolog code to setup some data and make the above less cluttered:
        EVENT=123,
        WHEN='20200701', is_immediately_before(BEFORE,WHEN),
        SBE= "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/",
        BASICS="https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/",
        AGGREGATION="https://www.ato.gov.au/business/small-business-entity-concessions/eligibility/aggregation/",
        findall(is_a_small_business_entity(E) at SBE, E in [company1,andrew,miguel], MoreFacts).

% mere linguistics...? :
is_a(cgt_asset,asset).
is_a(trading_stock,asset).
is_a(revenue_asset,asset).
is_a(depreciating_asset,asset).
is_a(loan_to_shareholder,asset). % from later in the text

is_a_party_in(TaxPayer,EventID) if 
    transfer_event(EventID,_Asset,_When,TaxPayer,_Tes).
is_a_party_in(TaxPayer,EventID) if 
    transfer_event(EventID,_Asset,_When,_Tor,Transferees) and TaxPayer in Transferees.

rollover_applies(Event) if
    transfer_event(Event,Asset,When,_Tor,_Tes) and is_after(When,'20160701') 
    % we could simply use [Tor|Tes] below, but perhaps this reads more nicely:
    and forall( is_a_party_in(Party,Event), has_an_aggregated_turnover_of(Party,Turnover) and Turnover < 10000000 and is_an_eligible_party(Party) )
    and part_of_genuine_restructure(Event)
        at "https://www.ato.gov.au/law/view/document?DocID=COG/LCG20163/NAT/ATO/00001&PiT=99991231235958"
    and is_immediately_before(Before,When) 
    and setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on Before, PreviousOwners)
    and setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on When, NewOwners) % assuming that When is the first moment after the transfer
    and ( 
        NewOwners = PreviousOwners 
        or 
        % there is a family trust to which all owners belong:
       '\'s_family_trust_is_in'(FT,GroupMembers) and s_family_trust_election_ocurred(FT)
        and forall(Owner/_ in PreviousOwners, Owner in GroupMembers) 
        and forall(Owner/_ in NewOwners, Owner in GroupMembers)
    )
    and is_an_eligible_asset(Asset).


is_ultimately_owned_by(Asset,Owner,1) on T if % full ownership
    owns(Owner,Asset) on T
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
%TODO: extend predicates for partial ownership, and indirection ( using is_connected_to ?)

is_an_eligible_party(P) if 
    is_a_small_business_entity(P) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
is_an_eligible_party(P) if 
    has_affiliated_with(P,Affiliate) at
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/"
    and is_a_small_business_entity(Affiliate) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
is_an_eligible_party(P) if 
    is_connected_to(P,Taxpayer) at
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/" 
    and is_a_small_business_entity(Taxpayer) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
is_an_eligible_party(P) if
    is_a_partner_in_partnership_with(P,Partnership) at % our first knowledge page:
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/"
    and is_a_small_business_entity(Partnership) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".

is_an_eligible_asset(A) if
    is_an_active_asset(A) and is_of_asset_type(A,Type) and
    % not A=loan_to_shareholder and irrelevant?
    Type in [cgt_event,depreciating_asset,trading_stock,revenue_asset].

is_an_active_asset(Asset) if 
    is_used_in_business_of(Asset,_SomeTaxpayer) % our first knowledge page:
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

is_of_asset_type(Asset,Type) if 
    is_a(Type,asset) and is_of_asset_type(Asset,Type) at myDb17.

has_an_aggregated_turnover_of(Party,Turnover) if 
    has_an_aggregated_turnover_of(Party,Turnover) at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

% "Tax implications" seem to boil down to:
is_a_rollover_cost(Cost) if
    transfer_event(_ID,Asset,When,_TaxPayer,_Transferees) and is_immediately_before(Before,When) 
    and costs(Asset,Cost) on Before.

%TODO; discuss with Andrew
% "Other implications" seem inocuous, addressable elsewhere, or ignorable (e.g. no market balue consideration), except:
% https://www.ato.gov.au/general/tax-and-corporate-australia/a-strong-domestic-tax-regime/#Generalantiavoidancerule can we encode this elsewhere?
% membership interests calculation:???
% integrity rule?

/** <examples>
?- query_with_facts(rollover_applies(Event),'Andrew email Feb 4 2021',Unknowns,Explanation,Result).
?- le(LogicalEnglish).
*/
