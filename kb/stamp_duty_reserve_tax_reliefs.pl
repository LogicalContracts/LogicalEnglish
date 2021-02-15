% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/guidance/stamp-duty-reserve-tax-reliefs-and-exemptions',[]).

mainGoal(exempt_transfer(_FromTaxPayer,_ToTaxPayer,_SecurityIdentifier,_When),"Determine if an electronic transaction is exempt from SDRT").

example("Chris Feb 12 - 1A",[
    /* 
    Alice is the daughter of John.
    Alice has been married to Adam since 1 Jan 2011.
    Alice inherited some shares left to her in John’s will when he passed away on 1 Jan 2021.
    The shares were transferred from John’s estate to Alice on 1 Feb 2021.
    The shares were not in a trust.
    The shares left to Alice were listed on the FTSE100.
    Expected result: exempt due to shares being inherited through a will.
    */
    scenario([
        married_or_in_civil_partnership(alice,adam,When) at myDB_456 if When@>= '20110101',
        shares_transfer(john,alice,sharesID,'20210201'),
        shares_inherited_in_will,
        shares_in_trust_that__ if false
        ], exempt_transfer)
    ]).
example("Chris Feb 12 - 1B1",[
        /*
        Ben is the step-brother of Adam.
        Ben received some shares from Adam that in a paid transfer (i.e. not a gift) on 10 Dec 2020.
        Ben was not a trustee of the trust on 10 Dec 2020, but became one on 1 Jan 2021.
        The shares were admitted to trading on Merkur Market (Norway), but not listed on any market.
        Expected result: exempt due to shares trading on a recognised growth market.
        */
        scenario([
            married_or_in_civil_partnership(ben,adam,When) at myDB_456 if false,
            shares_transfer(adam,ben,sharesID,'20201010'),
            shares_received_as_gift if false,
            shares_in_trust_that__ if false,
            shares_trading_on_growth_market_but_unlisted_on_recognized % quick and dirty
             ], exempt_transfer)
    ]).
example("Chris Feb 12 - 1B2",[
    /*
    Ben is the step-brother of Adam.
    Ben received some shares from Adam that in a paid transfer (i.e. not a gift) on 10 Dec 2017.
    Ben was not a trustee of the trust on 10 Dec 2017, but became one on 1 Jan 2018.
    The shares were admitted to trading on Merkur Market (Norway), but not listed on any market.
    Expected result: not exempt. Ben was not a co-trustee of Adam when the transfer was made; 
        market was not recognised as a high growth market at time of transfer. 
    */
    scenario([
        married_or_in_civil_partnership(ben,adam,When) at myDB_456 if false,
        shares_transfer(adam,ben,sharesID,'20171210'),
        shares_received_as_gift if false,
        shares_inherited_in_will if false,
        shares_transferred_on_divorce_or_dissolution if false,
        settlement_to_shareholders_on_business_wound_up if false,
        shares_in_trust_that__ on T if T@>='20180101',
        trading_in_market(sharesID,merkurMarket,_Anytime) at myDB_789,
        growth_market(merkurMarket) on When at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330" if When@>='20180401' % dummy date
         ], not exempt_transfer)
]).
example("Chris Feb 12 - 1C",[
    /*
    Cathy bought some shares from a broker, which were transferred to her on 1 Jan 2021.
    The shares were for stock of The International Bank for Reconstruction and Development
    Expected result: exempt, see https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041040 
    */
    scenario([
        shares_transfer(broker,cathy,'International Bank for Reconstruction and Development','20210101')
         ], exempt_transfer)
]).
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
    or shares_transferred_on_divorce_or_dissolution % it would be nice to know that stepbrothers cannot be married, ergo not divorced...
    or shares_in_trust_that__ on T % is this the case for a change of trustee, or transfer from one trust to another??
    or shares_trading_on_growth_market_but_unlisted_on_recognized 
    or settlement_to_shareholders_on_business_wound_up
    or shares_transfer(_From,_To,SecurityID,_When) and exemption(SecurityID) at 'https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041040'.

% Unlike suggested by Chris on Dec 2, this is NOT quite reflecting the full STSM041260... just the market lists:
shares_trading_on_growth_market_but_unlisted_on_recognized if
    shares_transfer(_From,_To,ID,When) and trading_in_market(ID,Market)
    and growth_market(merkurMarket) on When at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330"
    and forall( 
        recognized_stock_exchange(Exchange) on When
            at "https://www.gov.uk/government/publications/recognised-stock-exchanges-definition-legislation-and-tables/recognised-stock-exchanges-definition-legislation-and-tables-of-recognised-exchanges", 
        not trading_in_market(ID,Exchange) on When).

shares_received_as_gift if
    question("Did you receive the shares as a gift, without paying anything (either money or some other consideration)").

married_or_in_civil_partnership(P1,P2) if
    % assumes myDB_456 checks also the symmetrical relationship
    now(When) and married_or_in_civil_partnership(P1,P2,When) at myDB_456.

trading_in_market(SecurityID,MarketID) if
    now(When) and trading_in_market(SecurityID,MarketID,When) at myDB_789.

%TODO: later (cf. Chris email Dec 2, 2020): Transfers that qualify for Stamp Duty Reserve Tax RELIEF 

/** <examples>
?- query_with_facts(exempt_transfer,"Chris Feb 12 - 1A",Unknowns,Explanation,Result).
?- (Unknowns=[];Unknowns=_), query_with_facts(exempt_transfer,"Chris Feb 12 - 1B1",Unknowns,Explanation,Result).
*/