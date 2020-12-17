% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:- module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover',[]).


mainGoal(rollover_applies(ID,Asset,When,TransferorTFN,TransfereesTFNList), "Determine if a asset transfer event can be treated as a restructure rollover").

:- thread_local transfer_event/5.
rollover_applies(ID,Asset,When,TransferorTFN,TransfereesTFNList) :-
    assert(transfer_event(ID,Asset,When,TransferorTFN,TransfereesTFNList)),
    rollover_applies.


% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.


% example(Title,Sequence).  Sequence is a list of state(FactChanges,TrueConclusion)
% An example ilustrates and tests:
%TODO: replace pseudo code text by predicates
example( "Ultimate ownership unchanged", [
    % initial facts and condition:
    state(["penny runs a business B","B has assets A"], ultimate_owner(A,'Penny')),
    % new facts and condition:
    state(["penny has trust T", transfer_event(ID,A,When,B,[T])], ultimate_owner(A,'Penny'))
    ]).
example( "Changed share of ownership", [
    % initial facts and condition:
    state(["Amy, Joanna and Remy run a delivery business B as equal partners","B has assets A"], 
        true),
    % new facts and condition:
    state(["Amy, Joanna and Remy establish company C, different shares", transfer_event(ID,A,When,B,[C])], 
        not rollover_applies),
    % alternative facts and another condition:
    state([- "Amy, Joanna and Remy establish company C, different shares", "Amy, Joanna and Remy establish company C, equal shares"], 
        rollover_applies)
    ]).

% mere linguistics...? :
isa(cgt_event,asset).
isa(trading_stock,asset).
isa(revenue_asset,asset).
isa(depreciating_asset,asset).
isa(loan_to_shareholder,asset). % from later in the text

you(TaxPayer) if 
    transfer_event(ID,Asset,When,TaxPayer,Tes).
you(TaxPayer) if 
    transfer_event(ID,Asset,When,Tor,Transferees) and TaxPayer in Transferees.

rollover_applies if
    you(Y) and has_aggregated_turnover(Y,Turnover) 
        at "https://www.ato.gov.au/business/small-business-entity-concessions/eligibility/aggregation/" 
    and Turnover < 10000000 % is this redundant with elligible_party?
    and transfer_event(ID,Asset,When,Tor,Tes) and after(When,'20160701') 
    and forall( Party in [Tor|Tes], elligible_party(Party))
    and part_of_genuine_restructure(ID)
        at "https://www.ato.gov.au/law/view/document?DocID=COG/LCG20163/NAT/ATO/00001&PiT=99991231235958"
    and immediately_before(Before,When) and setof(Owner/Share, ultimate_owner(Asset,Owner,Share) on Before, PreviousOwners)
    and setof(Owner/Share, ultimate_owner(Asset,Owner,Share) on When, NewOwners) % assuming that When is the moment after the transfer
    and ( 
        NewOwners = PreviousOwners or 
        family_trust(FT,GroupMembers) and forall(Owner/_ in PreviousOwners, Owner in GroupMembers) and forall(Owner/_ in NewOwners, Owner in GroupMembers)
    )
    and elligible_asset(Asset).


ultimate_owner(Asset,Owner,1) :- % full ownership
    owns(TFN,Asset) 
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".
%TODO: extend predicates for partial ownership, and indirection ( using connected_to ?)

elligible_party(P) if 
    is_a_small_business_entity(P) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) if 
    affiliate(P,AffiliateTFN) at
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/"
    and is_a_small_business_entity(AffiliateTFN) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) if 
    connected_to(P,ConnectedTFN) at
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/" 
    and is_a_small_business_entity(ConnectedTFN) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) if
    partner_in_partnership(P,Partnership) at % our first knowledge page:
        "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/"
    and is_a_small_business_entity(Partnership) at
        "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".

elligible_asset(A) if
    active_asset(A) and asset_type(A,Type) and
    % not A=loan_to_shareholder and   irrelevant?
    Type in [cgt_event,depreciating_asset,trading_stock,revenue_asset].

active_asset(A) if 
    is_used_in_business_of(Asset,SomeTFN) % our first knowledge page:
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

asset_type(Asset,Type) if 
    isa(Type,asset) and asset_type(Asset,Type) at myDb17.

% "Tax implications" seem to boil down to:
rollover_cost(Cost) if
    transfer_event(ID,Asset,When,TaxPayer,Tes) and immediately_before(Before,When) 
    and asset_value(Asset,Cost) on Before.

%TODO; discuss with Andrew
% "Other implications" seem inocuous, addressable elsewhere, or ignorable (e.g. no market balue consideration), except:
% https://www.ato.gov.au/general/tax-and-corporate-australia/a-strong-domestic-tax-regime/#Generalantiavoidancerule can we encode this elsewhere?
% membership interests calculation:???
% integrity rule?