:- module('1_net_asset_value_test_3',[]).

en("the target language is: prolog. 
    
the templates are:
    *an asset* has *a type* of liability that costs *an amount*,
    *an amount* is a provision on *an asset* s liabilities of some type that is in *a set*,
    *a first amount* is the market value of *an asset* at *a date*,
    *an asset* is an earnout cgt asset valued at *a value* according to other legislation,
    *an asset* is an earnout cgt asset valued at *a value*,
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
    *an asset* is solely for personal use by *a person*,
    it is for home use for private purposes only including incidental income producing,
    *an asset* is part of rights to amounts or assets of super fund or approved deposit fund,
    *an asset* is life insurance for *a person*,
    *an asset* is used in business of *a taxpayer* according to other legislation,
    *a person* owns *an asset* according to other legislation,
    *an asset* is share in company of *a connection* according to other legislation,
    *an asset* is of interest in trust of *a connection* according to other legislation,
    *an asset* is a cgt asset according to other legislation.

the ontology is:
An asset is a cgt asset
    if the asset is a cgt asset according to other legislation. 

the knowledge base 1_net_asset_value_test_3 includes:
    
A tax payer satisfies maximum net asset value test at a date
    if the tax payer has cgt assets net value of a value at the date
    and the value =< 6000000.

A person has cgt assets net value of a value at a date
    if  the value is the sum of each partial such that
        the person has relevant asset an asset
        and the asset is a cgt asset
        and it is not the case that
            the person has to exclude asset the asset
        and the partial is the net value of the asset at the date.

A person has relevant asset an asset
    if the person owns the asset.

A person has relevant asset an asset
    if the person is connected to a connection
    and the connection owns the asset.

A person has relevant asset an asset
    if the person is affiliated with an affiliate
    and the affiliate owns the assethas_cgt_assets_net_value_of_at
    and the asset is used in business of the person
        or the person is connected to a connection C
            and the asset is used in business of C
    .

A person has relevant asset an asset
    if the person is affiliated with an affiliate
    and the affiliate is connected to an affiliate connection
    and the affiliate connection owns the asset
    and the asset is used in business of the person
        or the person is connected to a connection C
            and the asset is used in business of C
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
        the individual amount is a provision on the asset s liabilities of some type that is in [annual leave, long service leave, unearned income, tax liabilities, legally enforceable debts, legal or equitable obligations]
        at the date
    and the amount = the first amount - the second amount.

An amount is a provision on an asset s liabilities of some type that is in a set
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

%a TPN is an individual at a Date
%    if myDB_entities : is_an_individual_on(the TPN,the Date).

an asset is an earnout cgt asset valued at a value
    if  the asset is an earnout cgt asset valued at the value according to other legislation.
    
scenario feb 5 2021 is:
    4000000 is the net value of cgt asset 1 at 2021-02-05. 
    andrew owns cgt asset 1.
    cgt asset 1 is a cgt asset according to other legislation. 
    cgt asset 1 is an earnout cgt asset valued at 4000000.
    andrew is affiliated with affiliate1.
    affiliate1 owns cgt asset 2.
    10000000 is the net value of cgt asset 2 at EARNOUT.
    cgt asset 2 has annual_leave of liability that costs 2000000.
    andrew is connected to entity according to other legislation.
    entity owns asset3.
    asset3 is a cgt asset according to other legislation. 
    asset3 is an earnout cgt asset valued at 2000000.
    
query andrew is:
    andrew satisfies maximum net asset value test at which date. 
    
query taxpayer is:
    which taxpayer satisfies maximum net asset value test at which date.
    
query date is:
    which taxpayer satisfies maximum net asset value test at 2021-02-05.
    
query asset is:
    which asset is a cgt asset. 
	

%?- query_with_facts(satisfies_maximum_net_asset_value_test_at(TaxPayer, Date),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
%?- query_with_facts(satisfies_maximum_net_asset_value_test_at(andrew, Date),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).
%?- query_with_facts(satisfies_maximum_net_asset_value_test_at(TaxPayer, '20210205'),'Andrew email Feb 5 2021',Unknowns,Explanation,Result).	

").

/** <examples>
?- show prolog. 
?- answer("andrew with scenario feb 5 2021").
?- answer("taxpayer with scenario feb 5 2021").
?- answer(andrew, with('feb 5 2021'), le(E), R).
?- answer(date, with('feb 5 2021'), le(E), R).
?- answer(asset, with('feb 5 2021'), le(E), R).
*/




