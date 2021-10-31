:-module('one+https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/',[]).

mainGoal(satisfies_maximum_net_asset_value_test_at(_Taxpayer, _Date), "Determine if a given entity satisfies the maximum net value test for CGT assets").

example('Andrew email Feb 5 2021',[
    /* Andrew has net CGT assets 4,000,000, has affiliate with net assets 1,000,000, has connected entity with net CGT assets of 2,000,000 */
    scenario([
        owns(andrew,cgt_asset_1) at myDB1,
        is_a_cgt_asset(cgt_asset_1) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/",
        is_an_earnout_cgt_asset_valued_at(cgt_asset_1,4000000) at EARNOUT,
        % is_share_in_company(cgt_asset_1,entity) at myDB1 if false,
        % has_to_exclude_asset(andrew,_) if false, %http://localhost:3050/p/tests.pl#tabbed-tab-0 Andrew doesn't want any asset excluded!
        is_affiliated_with(andrew,affiliate1),
        ++ owns(affiliate1,cgt_asset_2), ++ '\'s_net_value_is'(cgt_asset_2,1000000),
        ++ is_the_market_value_of_at(10000000, cgt_asset_2, EARNOUT), 
        ++ has_of_liability_that_costs(cgt_asset_2, annual_leave, 2000000), 
        is_connected_to(andrew,entity) at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/connected-entities/",
        owns(entity,asset3) at myDB1,
        is_a_cgt_asset(asset3) at "https://www.ato.gov.au/General/Capital-gains-tax/CGT-assets-and-exemptions/",
        is_an_earnout_cgt_asset_valued_at(asset3,2000000) at EARNOUT,
        is_used_in_business_of(_,_) at myDB2 if false
        ], not satisfies_maximum_net_asset_value_test(andrew, '20210205'))
    ]) :- EARNOUT="https://www.ato.gov.au/General/Capital-gains-tax/In-detail/Business-assets/Earnout-arrangements-and-CGT/".

en("the templates are:
    *an asset* has *a type* of liability that costs *an amount*,
    *an amount* is a provision on *an asset*'s liabilities of some type that is in *a set*,
    *a first amount* is the market value of *an asset* at *a date*,
    *an asset* is an earnout cgt asset valued at *a value* according to other legislation,
    *an asset* is an earnout cgt asset valued at *a value*,
    *an asset* is a cgt asset,
    *a tax payer* satisfies maximum net asset value test at *a date*,
    *a person* has cgt assets net value of *a value* at *a date*,
    *a person* has relevant asset *an asset*,
    *a person* owns *an asset*,
    *a taxpayer* is connected to *a connection* according to other legislation,
    *a person* is connected to *a connection*,
    *a taxpayer* is affiliated with *an affiliate* at *a time* according to other legislation,
    *a taxpayer* is affiliated with *an affiliate* at *a time*,
    *a person* is affiliated with *an affiliate*,
    *an asset* is used in business of *a person*,
    *a person* has to exclude asset *an asset*,
    *a value* is the net value of *an asset* at *a date*,
    *an asset* belongs to *a connection*,
    *a person* is an individual,
    *an asset* is solely for personal use by *a person*,
    it is for home use for private purposes only including incidental income producing,
    *an asset* is part of rights to amounts or assets of super fund or approved deposit fund,
    *an asset* is life insurance for *a person*,
    *an asset* is used in business of *a taxpayer* according to other legislation,
    *a person* owns *an asset* according to other legislation,
    *an asset* is share in company of *a connection* according to other legislation,
    *an asset* is of interest in trust of *a connection* according to other legislation,
    *an asset* is a cgt asset according to other legislation.


the knowledge base includes:
A tax payer satisfies maximum net asset value test at a date
    if the tax payer has cgt assets net value of a value at the date
    and the value =< 6000000.

A person has cgt assets net value of a value at a date
    if  the value is the sum of each asset net such that
        the person has relevant asset an asset
        and the asset is a cgt asset
        and it is not the case that
            the person has to exclude asset the asset
            and the asset net is the net value of the asset at the date
   .

A person has relevant asset an asset
    if the person owns the asset.

A person has relevant asset an asset
    if the person is connected to a connection
    and the connection owns the asset.

A person has relevant asset an asset
    if the person is affiliated with an affiliate
    and the affiliate owns the asset
    and the asset is used in business of the person
        or the person is connected to a connection
            and the asset is used in business of the connection
    .

A person has relevant asset an asset
    if the person is affiliated with an affiliate
    and the affiliate is connected to an affiliate connection
    and the affiliate connection owns the asset
    and the asset is used in business of the person
        or the person is connected to a connection
            and the asset is used in business of the connection
.

A person has to exclude asset an asset
    if the person is connected to a connection
    or the person is affiliated with an affiliate
        and the affiliate is connected to the connection
        and the asset belongs to the connection.

A person has to exclude asset an asset
    if the person is an individual
    and the asset is solely for personal use by the person
        or the person is affiliated with an affiliate
            and the asset is solely for personal use by the affiliate
                or it is for home use for private purposes only including incidental income producing
                or  the asset is part of rights to amounts or assets of super fund or approved deposit fund
                or  the asset is life insurance for the person
.

An amount is the net value of an asset at a date
    if  a first amount is the market value of the asset at the date
    and a second amount is the sum of each individual amount such that
        the individual amount is a provision on the asset's liabilities of some type that is in [annual leave, long service leave, unearned income, tax liabilities, legally enforceable debts, legal or equitable obligations]
        at the date
    and the amount is the first amount - the second amount.

An amount is a provision on an asset's liabilities of some type that is in a set
  if the asset has a type of liability that costs the amount
  and the type is in the set.

A taxpayer owns an asset
    if the taxpayer owns the asset according to other legislation.

A taxpayer is connected to a connection
    if the taxpayer is connected to the connection according to other legislation.

An asset is used in business of a taxpayer
    if the asset is used in business of the taxpayer according to other legislation.

A taxpayer is affiliated with an affiliate at a time
    if the taxpayer is affiliated with the affiliate at the time according to other legislation.

An asset belongs to a connection
    if the asset is share in company of the connection according to other legislation.

An asset belongs to a connection
    if  the asset is of interest in trust of the connection according to other legislation.

An asset is a cgt asset
    if the Asset is a cgt asset according to other legislation.

%a TPN is an individual at a Date
%    if myDB_entities : is_an_individual_on(the TPN,the Date).

an asset is an earnout cgt asset valued at a value
    if  the asset is an earnout cgt asset valued at the value according to other legislation."). 

/** <examples>
?- query_with_facts(satisfies_maximum_net_asset_value_test_at(TaxPayer, Date),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
?- query_with_facts(satisfies_maximum_net_asset_value_test_at(andrew, Date),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
?- query_with_facts(satisfies_maximum_net_asset_value_test_at(TaxPayer, '20210205'),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
*/

