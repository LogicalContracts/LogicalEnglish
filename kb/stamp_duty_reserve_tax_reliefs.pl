% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/guidance/stamp-duty-reserve-tax-reliefs-and-exemptions',[]).

mainGoal(exempt_transfer(FromTaxPayer,ToTaxPayer,SecurityIdentifier,Time),"Determine if an electronic transaction is exempt from SDRT").

example('Chris Feb 12 - 1A',[
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
        married_or_in_civil_partnership_at(alice,adam,Time) at myDB_456 if Time@>= '20110101',
        shares_transfer(john,alice,sharesID,'20210201'),
        corresponds_to_shares_inherited_in_a_will([john, alice, sharesID, '20210201']),
        it_is_about_shares_in_a_trust if false
        ], is_a_transfer_exempt([john, alice, sharesID, '20210201']))
    ]).
example('Chris Feb 12 - 1B1',[
        /*
        Ben is the step-brother of Adam.
        Ben received some shares from Adam that in a paid transfer (i.e. not a gift) on 10 Dec 2020.
        Ben was not a trustee of the trust on 10 Dec 2020, but became one on 1 Jan 2021.
        The shares were admitted to trading on Merkur Market (Norway), but not listed on any market.
        Expected result: exempt due to shares trading on a recognised growth market.
        */
        scenario([
            married_or_in_civil_partnership_at(ben,adam,Time) at myDB_456 if false,
            shares_transfer(adam,ben,sharesID,'20201010'),
            corresponds_to_shares_received_as_gift(it) if false,
            it_is_about_shares_in_a_trust if false,
            it_is_about_shares_trading_on_growth_market_but_unlisted_or_recognized % quick and dirty
             ], is_a_transfer_exempt([adam, ben, sharesID, '20201010']))
    ]).
example('Chris Feb 12 - 1B2',[
    /*
    Ben is the step-brother of Adam.
    Ben received some shares from Adam that in a paid transfer (i.e. not a gift) on 10 Dec 2017.
    Ben was not a trustee of the trust on 10 Dec 2017, but became one on 1 Jan 2018.
    The shares were admitted to trading on Merkur Market (Norway), but not listed on any market.
    Expected result: not exempt. Ben was not a co-trustee of Adam when the transfer was made; 
        market was not recognised as a high growth market at time of transfer. 
    */
    scenario([
        married_or_in_civil_partnership_at(ben,adam,Time) at myDB_456 if false,
        shares_transfer(adam,ben,sharesID,'20171210'),
        shares_received_as_gift if false,
        corresponds_to_shares_inherited_in_a_will([adam, ben, sharesID, '20171210']) if false,
        corresponds_to_shares_transferred_on_divorce_or_dissolution([adam, ben, sharesID, '20171210']) if false,
        it_corresponds_to_a_settlement_to_shareholders_on_a_business_wound_up if false,
        it_is_about_shares_in_a_trust on T if T@>='20180101',
        trading_in_market(sharesID,merkurMarket,_Anytime) at myDB_789,
        is_the_grown_market(merkurMarket) on Time at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330" if Time@>='20180401' % dummy date
         ], not is_a_transfer_exempt([adam, ben, sharesID, '20171210']))
]).
example('Chris Feb 12 - 1C',[
    /*
    Cathy bought some shares from a broker, which were transferred to her on 1 Jan 2021.
    The shares were for stock of The International Bank for Reconstruction and Development
    Expected result: exempt, see https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041040 
    */
    scenario([
        shares_transfer(broker,cathy,'International Bank for Reconstruction and Development','20210101')
         ], is_a_transfer_exempt([broker, cathy, 'International Bank for Reconstruction and Development', '20210101']))
]).

:- thread_local shares_transfer/4.
exempt_transfer(TaxPayerOne,TaxPayerTwo,SecurityIdentifier,Time) :-
    assert(shares_transfer(TaxPayerOne,TaxPayerTwo,SecurityIdentifier,Time)), % should retractall, but assuming this is a new, transient thread
    is_a_transfer_exempt([TaxPayerOne, TaxPayerTwo, SecurityIdentifier, Time]).


% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

% shares_transfer(FromTaxPayer,ToTaxPayer,Identifier,Time)
transfers_shares_to(Sender,Recipient) if  % for mere convenience below
    shares_transfer(Sender,Recipient,ID,Time).

%is_you(TaxPayer) if 
%    transfers_shares_to(_,TaxPayer).

%function(you(), Y) if 
%    is_you(Y). % functional notation for convenience

is_the_recipient_of(Recipient, Transfer) if
    Transfer = [Sender, Recipient, SecurityID, Time].

is_now(Time) if shares_transfer(Sender,Recipient,SecurityID,Time).

is_a_transfer_exempt(Transfer) if
    corresponds_to_shares_received_as_gift(Transfer)
    or corresponds_to_shares_inherited_in_a_will(Transfer) % should use perhaps (??): beneficiary_in_will_of(you(),Dead) at myDB666 and transfers_shares_to(Dead,you())
    or is_the_recipient_of(FirstPerson, Transfer) and is_married_or_in_civil_partnership_with(FirstPerson, SecondPerson) and transfers_shares_to(SecondPerson,FirstPerson) % assumes transfer can occur anytime during marriage
    or corresponds_to_shares_transferred_on_divorce_or_dissolution(Transfer) % it would be nice to know that stepbrothers cannot be married, ergo not divorced...
    or it_is_about_shares_in_a_trust on Time % is this the case for a change of trustee, or transfer from one trust to another??
    or it_is_about_shares_trading_on_growth_market_but_unlisted_or_recognized 
    or it_corresponds_to_a_settlement_to_shareholders_on_a_business_wound_up
    or shares_transfer(From,To,SecurityID,Time) and has_an_exemption(SecurityID) at 'https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041040'.

% Unlike suggested by Chris on Dec 2, this is NOT quite reflecting the full STSM041260... just the market lists:
it_is_about_shares_trading_on_growth_market_but_unlisted_or_recognized if
    shares_transfer(From,To,ID,Time) and is_trading_in_market(ID,Market)
    and is_the_grown_market(merkurMarket) on Time at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330"
    and forall( 
        is_a_recognized_stock_exchange(Exchange) on Time
            at "https://www.gov.uk/government/publications/recognised-stock-exchanges-definition-legislation-and-tables/recognised-stock-exchanges-definition-legislation-and-tables-of-recognised-exchanges", 
        not is_trading_in_market(ID,Exchange) on Time).

question( corresponds_to_shares_received_as_gift(it), "Did you receive the shares as a gift, without paying anything (either money or some other consideration)").

is_married_or_in_civil_partnership_with(FirstPerson,SecondPerson) if
    % assumes myDB_456 checks also the symmetrical relationship
    is_now(Time) and married_or_in_civil_partnership_at(FirstPerson,SecondPerson,Time) at myDB_456.

is_trading_in_market(SecurityID,MarketID) if
    is_now(Time) and trading_in_market(SecurityID,MarketID,Time) at myDB_789.

%TODO: later (cf. Chris email Dec 2, 2020): Transfers that qualify for Stamp Duty Reserve Tax RELIEF 

/** <examples>
?- query_with_facts(exempt_transfer,'Chris Feb 12 - 1A',Unknowns,Explanation,Result), render_questions(Unknowns,Questions).
?- (Unknowns=[];Unknowns=_), query_with_facts(exempt_transfer,'Chris Feb 12 - 1B1',Unknowns,Explanation,Result).
?- le(LogicalEnglish).
*/