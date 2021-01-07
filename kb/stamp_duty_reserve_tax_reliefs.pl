% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/guidance/stamp-duty-reserve-tax-reliefs-and-exemptions',[]).

mainGoal(exempt_transfer(FromTaxPayer,ToTaxPayer,SecurityIdentifier,When),"Determine if an electronic transaction is exempt from SDRT").

:- thread_local shares_transfer/4.
exempt_transfer(FromTaxPayer,ToTaxPayer,SecurityIdentifier,When) :-
    assert(shares_transfer(FromTaxPayer,ToTaxPayer,SecurityIdentifier,When)), % should retractall, but assuming this is a new, transient thread
    exempt_transfer.


% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

% shares_transfer(FromTaxPayer,ToTaxPayer,Identifier,When)
shares_transfer(From,To) if  % for mere convenience below
    shares_transfer(From,To,_ID,_When).

you(TaxPayer) if 
    shares_transfer(_,TaxPayer).

function(you(), Y) if 
    you(Y). % functional notation for convenience

now(Now) if shares_transfer(_,_,_,Now).

exempt_transfer if
    shares_received_as_gift
    or shares_inherited_in_will % should use perhaps (??): beneficiary_in_will_of(you(),Dead) at myDB666 and shares_transfer(Dead,you())
    or married_or_in_civil_partnership(you(),P) and shares_transfer(P,you()) % assumes transfer can occur anytime during marriage
    or shares_transferred_on_divorce_or_dissolution
    or 'shares_in_trust_that__?' % is this the case for a change of trustee, or transfer from one trust to another??
    or shares_trading_on_growth_market_but_unlisted_on_recognized 
    or settlement_to_shareholders_on_business_wound_up.

% Unlike suggested by Chris on Dec 2, this is NOT quite reflecting the full STSM041260... just the market lists:
shares_trading_on_growth_market_but_unlisted_on_recognized if
    shares_transfer(_From,_To,ID,_When) and trading_in_market(ID,Market)
    and growth_market(Market) at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330"
    and forall( 
        recognized_stock_exchange(Exchange) 
            at "https://www.gov.uk/government/publications/recognised-stock-exchanges-definition-legislation-and-tables/recognised-stock-exchanges-definition-legislation-and-tables-of-recognised-exchanges", 
        not trading_in_market(ID,Exchange) ).

shares_inherited_in_will if
    question("Did you receive the shares as a gift, without paying anything (either money or some other consideration)").

married_or_in_civil_partnership(P1,P2) if
    now(When) and married_or_in_civil_partnership(P1,P2,When) at myDB_456.

trading_in_market(SecurityID,MarketID) if
    now(When) and trading_in_market(SecurityID,MarketID,When) at myDB_789.

%TODO: later (cf. Chris email Dec 2, 2020): Transfers that qualify for Stamp Duty Reserve Tax RELIEF 