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

en("the templates are:
    merkurMarket is the name of a market at a time according to other legislation,
    a transfer from a tax payer one to a tax payer two with a security identifier at a time is exempt,
    a description is a transfer exempt,
    a sender transfers shares to a recipient,
    a sender transfers shares to a recipient at a time with some id as a code,
    a recipient is the recipient of a transfer,
    a time is now,
    a transfer corresponds to shares received as gift,
    a Transfer corresponds to shares inherited in a will,
    a person is the recipient of a transfer,
    a transfer corresponds to shares received as gift,
    a transfer corresponds to shares inherited in a will,
    a person is the recipient of a transfer,
    a first person is married or in civil partnership with a second person,
    a first person is married or in civil partnership at a time with a second person according to other legislation,
    a second person transfers shares to a first person,
    a transfer corresponds to shares transferred on divorce or dissolution,
    it is about shares in a trust at a time,
    a transfer is about shares trading on a growth market but unlisted or recognized,
    it corresponds to a settlement to shareholders on a business wound up,
    a security ID has an exemption according to other legislation,
    a transfer ID is trading in a market at a time,
    a transfer ID is trading in a market,
    a market name is the name of a market at a time according to other legislation,
    an exchange is a recognized stock exchange at a time according to other legislation,
    a person identified by an ID is trading in a market at a time according to other legislation,
    an event is exempt from SDRT,
    an event occurs at a time,
    an event is a paperless transfer of a security by a transferor to a transferee,
    an event is a gift to a transferee,
    an event corresponds to the transfer of a security in a will by a transferor to a transferee,
    an event is associated with some event of marriage-civil partnership between a transferor and a transferee,
    an event is associated with some event of divorce-dissolution of civil partnership between the transferor and the transferee,
    an security is in a trust at a time,
    a person is a trustee of a trust,
    a security is trading in a market at a time,
    a security is listed on a stock exchange at a time,
    an event is associated with some event of liquidation settlement to a transferee,
    a security is trading in some recognised growth market at a time,
    a market is an RGM market at the time as defined at an URL,
    a security is listed on some recognised stock exchange at a time,
    a stock exchange is a recognised stock exchange at a time as defined at an URL.

the knowledge base includes:

A transfer from a tax payer one to a tax payer two with a security identifier at a time is exempt
    if this information shares_transfer(the tax payer one,the tax payer two,the security identifier,the time) has been recorded
    and [the tax payer one,the tax payer two,the security identifier,the time] is a transfer exempt.

A sender transfers shares to a recipient
    if the sender transfers shares to the recipient at a time with some id as a code.

A recipient is the recipient of a transfer
    if the transfer = [a sender,the recipient,a security ID,a time].

A time is now
    if a sender transfers shares to a recipient at the time with some id as a security ID .

an event is exempt from SDRT
    if the event occurs at a time
    and the event is a paperless transfer of a security by a transferor to a transferee
    and the event is a gift to the transferee
        or the event corresponds to the transfer of the security in a will by the transferor to the transferee
        or the event is associated with some event of marriage-civil partnership between the transferor and the transferee
        or the event is associated with some event of divorce-dissolution of civil partnership between the transferor and the transferee
        or the security is in a trust at the time
            and the transferor is a trustee of the trust
            and the transferee is a trustee of the trust
        or the security is trading in some recognised growth market at the time
            and it is not the case that
                the security is listed on some recognised stock exchange at the time
        or the event is associated with some event of liquidation settlement to the transferee.

the security is trading in some recognised growth market at a time
    if the security is trading in a market at the time
    and the market is an RGM market at the time as defined at "https://www.gov.uk/hmrc-internal-manuals/stamp-taxes-shares-manual/stsm041330".

the security is listed on some recognised stock exchange at a time
    if the security is listed on a stock exchange at the time
    and the stock exchange is a recognised stock exchange at the time as defined at "https://www.gov.uk/government/publications/recognised-stock-exchanges-definition-legislation-and-tables/recognised-stock-exchanges-definition-legislation-and-tables-of-recognised-exchanges".


A transfer is a transfer exempt
    if the transfer corresponds to shares received as gift
    or the transfer corresponds to shares inherited in a will
    or a first person is the recipient of the transfer
        and the first person is married or in civil partnership with a second person
        and the second person transfers shares to the first person
    or the transfer corresponds to shares transferred on divorce or dissolution
    or it is about shares in a trust at a time
    or the transfer is about shares trading on a growth market but unlisted or recognized
    or it corresponds to a settlement to shareholders on a business wound up
    or the transfer = [a sender,a recipient,a security ID,a time]
        and the security ID has an exemption according to other legislation.

A transfer is about shares trading on growth market but unlisted or recognized
    if the transfer = [a sender,a recipient,an ID,a time]
    and the ID is trading in a grown market
    and merkurMarket is the name of the grown market at the time according to other legislation
    and for all cases in which
            an exchange is a recognized stock exchange at the time according to other legislation
        it is the case that:
            it is not the case that
                the ID is trading in the exchange at the time.

A first person is married or in civil partnership with a second person
    if  a time is now
    and the first person is married or in civil partnership at the time with the second person according to other legislation.

A security ID is trading in a market
    if a time is now
    and a person identified by the security ID is trading in the market at the time according to other legislation."). 


/** <examples>
?- query_with_facts(is_a_transfer_exempt,'Chris Feb 12 - 1A',Unknowns,Explanation,Result), render_questions(Unknowns,Questions).
?- (Unknowns=[];Unknowns=_), query_with_facts(exempt_transfer,'Chris Feb 12 - 1B1',Unknowns,Explanation,Result).
*/