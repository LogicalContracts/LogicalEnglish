% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
myURL("https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover").

% Assumptions: 
%   all values are in AUD
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.

%transfer_event(ID,Asset,When,Transferor,Transferees)

isa(cgt_event,asset).
isa(trading_stock,asset).
isa(revenue_asset,asset).
isa(depreciating_asset,asset).

% you(TaxPayer) :- ...?

rollover_applies :-
    you(Y) and has_aggregated_turnover(Y,Turnover) 
    at "https://www.ato.gov.au/business/small-business-entity-concessions/eligibility/aggregation/" 
    and Turnover < 10000000,
    and transfer_event(ID,Asset,When,Tor,Tes)
    and forall( Party in [Tor|Tes], elligible_party(Party))
    and part_of_genuine_restructure(ID) at 
    "https://www.ato.gov.au/law/view/document?DocID=COG/LCG20163/NAT/ATO/00001&PiT=99991231235958"
    and immediately_before(Before,When) and setof(Owner/Share, ultimate_owner(Asset,Owner,Share) on Before, PreviousOwners)
    and setof(Owner/Share, ultimate_owner(Asset,Owner,Share) on When, NewOwners), 
    NewOwners = PreviousOwners.
    % HERE!

% ultimate_owner(Asset,Owner,Share) :- ??

elligible_party(P) :- 
    is_a_small_business_entity(TFN) at
    "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) :- 
    affiliate(P,AffiliateTFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/"
    and is_a_small_business_entity(AffiliateTFN) at
    "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) :- 
    connected_to(P,ConnectedTFN) at
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/" 
    and is_a_small_business_entity(ConnectedTFN) at
    "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
elligible_party(P) :- 
    partner_in_partnership(P,Partnership) at % our first knowledge page...
    "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions"
    and is_a_small_business_entity(Partnership) at
    "https://www.ato.gov.au/General/Capital-gains-tax/Small-business-CGT-concessions/Basic-conditions-for-the-small-business-CGT-concessions/Small-business-entity/".
