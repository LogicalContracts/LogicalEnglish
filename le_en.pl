% A cured database of Document Examples for Logical English

:- module(le_en, [en/3]).

%1
en('1_cgt_assets_and_exemptions_3', 'This example covers CGT assets and exemptions.', "the target language is: prolog. 
    
the templates are:

  *an asset* costed *an amount* to acquire.
  *a thing* meets the definition of plant and equipment under Division 43 of the Income Tax Act.
  *an asset* is used soley for taxable income generating purposes. 
  *an asset* is used purely for personal use purposes. 
  *an award* was secured for valour or brave conduct.
  *an asset* was bought.
  *an asset* was secured in exchange for *an other asset*.
  *an asset* has been secured through a game or a competition. 
  *an asset* corresponds to compensation or damages.
  *an asset* was received for an injury at *a place* of *a taxpayer*. 
  *an asset* was received for an injury at *a place* of *a taxpayer* s relative.
  *an asset* was received under an Unlawful Termination Assistance Scheme.
  *an asset* was received under an alternative Dispute Resolution Assistance Scheme.
  *an asset* was received under an M4/M5 Cashback Scheme. 
  *an asset* was received under a scheme established under legislation by an Australian Government agency or a foreign government agency.
  *an asset* was received under a scheme established under legislation by a local government body.
  *an asset* was received under a scheme established under legislation by a foreign government agency. 
  *an asset* is an interest in *a fund*. 
  *a fund* has *a number* of members.
  *an asset* is being transferred because of a relationship breakdown. 
  *an asset* was created in *an agreement*. 
  *an asset* was ended in *an agreement*. 
  *an asset* arose from the maturity of *a policy*. 
  *an instrument* is an annuity instrument for *an asset*. 
  *an asset* of *a taxpayer* is a CGT exempt asset.
  *an asset* is for surrender of *a policy*. 
  *a taxpayer* is the original beneficial owner of *a policy*. 
  *an asset* is held in a pooled development fund. 
  *an asset* is held in certain venture capital entity.
  *an asset* is held in an early stage venture capital limited partnership. 
  *an asset* was gifted through a will to a deductible gift recipient beneficiary.
  *an asset* belongs to *a person*. 
  *a person* was a resident of Norfolk Island at *a date*. 
  *an asset* was acquired at *a date*.
  *a thing* is a share or a unit or similar investments.
  *a thing* is a rental property plant or equipment.
  *an asset* is used for the purchase of items for personal consumption. 
  *an arrangement* for *an asset* is *a state*. 
  
the ontology is:

% A CGT asset is:
% (a) any kind of property; or
% (b) a legal or equitable right that is not property.

a thing is a CGT asset
  if the thing is a property
  or    the thing is a legal right
        or the thing is an equitable right
    and it is not the case that
        the thing is a property. 

% List of assets - List of CGT assets and exemptions - https://www.ato.gov.au/Individuals/Capital-gains-tax/List-of-cgt-assets-and-exemptions/

% Real estate assets
% Shares, units & similar investments
% Personal use assets
% Collectables assets
% Intangible assets
% Foreign currency assets
% Depreciating assets
% Award & payout assets
a thing is a property 
  if the thing is a real state asset
  or the thing is a share or a unit or similar investments
  or the thing is a personal use asset
  or the thing is a collectable asset
  or the thing is an intangible asset
  or the thing is a foreign currency asset
  or the thing is a depreciating asset
  or the thing is an award or payout asset. 

% Real estate assets list
% Main residence
% Granny flat
% Vacant land
% Business premises
% Rental properties
% Holiday houses
% Hobby farms
% Farms
a thing is a real state asset
  if the thing is a main residence
  or the thing is a granny flat
  or the thing is a vacant land
  or the thing is a business premise
  or the thing is a holiday house
  or the thing is a hobby farm
  or the thing is a farm. 

% Main residence is a CGT exempt asset if it is used purely for personal use.
an asset is a CGT exempt asset
  if the asset is used purely for personal use purposes
  and the asset is a main residence.

% Granny flat is a CGT exempt asset if an eligible granny flat arrangement is created, varied or terminated.
an asset is a CGT exempt asset
  if the asset is a granny flat
  and an arrangement for the asset is created
      or the arrangement for the asset is varied 
      or the arrangement for the asset is terminated
  and the arrangement is a granny flat arrangement. 

% Shares, units & similar investments assets list
% Shares
% Units
% Convertible notes
% Rights to aquire shares or units
% Options to acquire shares or units
% Stapled securities
% Cryptocurrencies
a thing is a share or a unit or similar investments
  if the thing is a share
  or the thing is a unit
  or the thing is a convertible note
  or the thing is a right to acquire shares or units
  or the thing is an option to acquire shares or units
  or the thing is a stapled security
  or the thing is a cryptocurrency. 

% Cryptocurrency is a CGT exempt asset if it is used for the purchase of items for personal consumption.
an asset is a CGT exempt asset
  if the asset is a cryptocurrency
  and the asset is used for the purchase of items for personal consumption.

% Personal use assets list
% boats
% furniture
% electrical goods
% household items
% option or right to acquire a personal use asset
% An IOU / loan to family member
% An IOU / loan to family friend
% An IOU related to a personal use asset
a thing is a personal use asset
  if the thing is a boat
  or the thing is a furniture
  or the thing is an electrical good
  or the thing is a household item
  or the thing is an option or right to acquire a personal use asset
  or the thing is an IOU as a loan to a family member
  or the thing is an IOU as a loan to a family friend
  or the thing is an IOU related to a personal use asset. 

% A personal use asset is not a CGT exempt asset if the asset cost the person $10,000 or more to acquire.
an asset is a CGT exempt asset
  if the asset is a personal use asset
  and the asset costed an amount to acquire 
  and the amount < 10000.  

% Collectables assets list
% artwork
% jewellery
% antiques
% rare coins 
% rare medallions
% rare stamps
% rare folios
% rare manuscripts
% rare books
a thing is a collectable asset
  if the thing is an artwork
  or the thing is a jewellery
  or the thing is an antique
  or the thing is a rare coin
  or the thing is a rare medallion
  or the thing is a rare stamp
  or the thing is a rare folio
  or the thing is a manuscript
  or the thing is a book. 

% If an entity makes a capital loss on disposal of a collectable then that loss can only be offset against a capital gain on the disposal of a collectable.

% Intangibles assets list
% Leases
% goodwill
% licences
% contractual rights
a thing is a intangible asset
  if the thing is a lease
  or the thing is a goodwill
  or the thing is a license
  or the thing is a contractual right. 

% Depreciating assets list
% business equipment
% plant and equipment found in a rental property
a thing is a depreciating asset
  if the thing is a business equipment
  or the thing is a rental property plant or equipment.

% Plant and equipment is rental property plant and equipment if it is a removable asset in a rental property
%   and it meets the definition of plant and equipment under Division 43 of the Income Tax Act.
a thing is a rental property plant or equipment
  if the thing is a removable asset in a rental property
  and the thing meets the definition of plant and equipment under Division 43 of the Income Tax Act.

% A depreciating asset is CGT exempt asset if it is used soley for taxable income generating purposes.
an asset is a CGT exempt asset
  if the asset is a depreciating asset
  and the asset is used soley for taxable income generating purposes.

% A motor vehicle is a CGT exempt asset if it is used purely for personal use purposes.
an asset is a CGT exempt asset
  if the asset is a motor vehicle
  and the asset is used purely for personal use purposes.

% A motor cycle is a CGT exempt asset if it is used purely for personal use purposes.
an asset is a CGT exempt asset
  if the asset is a motor cycle
  and the asset is used purely for personal use purposes.

% An award is a CGT exempt asset if it was secured for valour or brave conduct.
% An award is not a CGT exempt asset if it was bought or if it was secured in exchange for another asset. 
an asset is a CGT exempt asset
  if the asset is an award
  and the asset was secured for valour or brave conduct
  and it is not the case that
      the asset was bought
      or the asset was secured in exchange for another asset. 

% Gambling winnings are a CGT exempt asset if they are secured through a game or a competition.
an asset is a CGT exempt asset
  if the asset is a gambling winning
  and the asset has been secured through a game or a competition. 

% Compensation or damages is a CGT exempt asset if received for an injury you or your relative suffered at work.
% Compensation or damages is a CGT exempt asset if received for an injury you or your relative suffered anywhere.
% Compensation or damages is a CGT exempt asset if received under an Unlawful Termination Assistance Scheme.
% Compensation or damages is a CGT exempt asset if received under an alternative Dispute Resolution Assistance Scheme.
% Compensation or damages is a CGT exempt asset if received under an M4/M5 Cashback Scheme.
% Compensation or damages is a CGT exempt asset if received under a scheme established under legislation by an Australian Government agency,  
% or a foreign government agency.
an asset is a CGT exempt asset
  if the asset corresponds to compensation or damages
  and the asset was received for an injury at a place of a taxpayer
      or the asset was received for an injury at place of the taxpayer s relative
      or the asset was received under an Unlawful Termination Assistance Scheme
      or the asset was received under an alternative Dispute Resolution Assistance Scheme
      or the asset was received under an M4/M5 Cashback Scheme
      or the asset was received under a scheme established under legislation by an Australian Government agency or a foreign government agency.

% Receipt of compensation or damages is a CGT exempt asset if received under a scheme established under legislation by a local government body.
% Receipt of compensation or damages is a CGT exempt asset if received under a scheme established under legislation by a foreign government agency.
an asset is a CGT exempt asset
  if the asset is a receipt for compensation or damages
  and the asset was received under a scheme established under legislation by a local government body
      or the asset was received under a scheme established under legislation by a foreign government agency. 

% An interest in one small super fund (a complying fund that has less than 5 members) is a CGT exempt asset if 
% the rights are transferred to another because of a relationship breakdown between spouses or former spouses.
an asset is a CGT exempt asset
  if the asset is an interest in a super fund
  and the super fund has a number of members
  and the number < 5
  and the asset is being transferred because of a relationship breakdown. 

% Rights are a CGT exempt asset if created or ended in a superannuation agreement.
an asset is a CGT exempt asset
  if the asset is a right
  and the asset was created in an agreement
      or the asset was ended in the agreement
  and the agreement is a superannuation agreement. 

% A payout under a general insurance policy is a CGT exempt asset if it arises from the maturity of the policy.
% A payout under a life insurance policy is a CGT exempt asset if it arises from the maturity of the policy.
an asset is a CGT exempt asset
  if the asset is a payout
  and the asset arose from the maturity of a policy
  and the policy is a general insurance policy for the asset
      or the policy is a life insurance policy for the asset.

% A payout under an annuity instrument is a CGT exempt asset if it arises from the maturity of the instrument.
an asset is a CGT exempt asset
  if the asset is a payout
  and the asset arose from the maturity of an instrument
  and the instrument is an annuity instrument for the asset. 

% A payment for surrender of an insurance policy is a CGT exempt asset where you are the original beneficial owner of the policy.
an asset of a taxpayer is a CGT exempt asset
  if the asset is a payment
  and the asset is for surrender of an insurance policy
  and the taxpayer is the original beneficial owner of the policy. 

% A share is a CGT exempt asset if held in a pooled development fund.
% A share is a CGT exempt asset if held in certain venture capital entity.
% A share is a CGT exempt asset if held in an early stage venture capital limited partnership.
an asset is a CGT exempt asset
  if the asset is a share 
  and the asset is held in a pooled development fund
      or the asset is held in certain venture capital entity
      or the asset is held in an early stage venture capital limited partnership. 

% A  gift is a CGT exempt asset if gifted through a will to a deductible gift recipient beneficiary.
an asset is a CGT exempt asset
  if the asset is a gift
  and asset was gifted through a will to a deductible gift recipient beneficiary.

% An asset is a CGT exempt asset if held by a person who was a resident of Norfolk Island before 24 October 2015
%   and the asset was acquired on Norfolk Island before 24 October 2015.
an asset is a CGT exempt asset
  if the asset belongs to a person
  and the person was a resident of Norfolk Island at a date
  and the date is before 2015-10-24.

% An asset is a CGT exempt asset if the asset was acquired on or before 19 September 1985.
an asset is a CGT exempt asset
  if the asset was acquired at a date
  and the date is before or equal to 1985-09-19. 
        
scenario Colin is:
    asset one belongs to Colin.
    Colin was a resident of Norfolk Island at 2000-01-01.
    asset two was acquired at 1985-09-19.  

% Check if your assets are subject to CGT, exempt, or pre-date CGT.
query one is:
  which thing is a CGT asset.

query two is:
  which asset is a CGT exempt asset. 

query three is:
  which asset is a pre-date CGT asset. 
"). 
% end of en('1_cgt_assets_and_exemptions_3', ...

%2
en('1_net_asset_value_test_3', 'This example covers the maximum net asset value test.', "the target language is: prolog. 
    
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
    if  the person has relevant asset an asset X
    and a value Y is the net value of X at the date
    and the value is the sum of each partial such that
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
    and the affiliate owns the asset
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
	
"). 
% end of en('1_net_asset_value_test_3', ...

%3
en('3_rollover_3', 'This example covers the small business restructure rollover.', "the target language is: prolog. 
    
    the templates are:
    *A trust* s election ocurred,
    *A trust* is the trust of a group,
    *An event* rollover applies,
    *An owner* / *a share*,
    *An Event ID* rollover of the transfer of *an Asset* from *a Transferor* to *a Transferees List* at *a time* applies,
    *an event* of transfering *an asset* from *a payer* to *a recipient* at *a date* occurs,
    the small business restructure rollover applies to *an event*,
    *an event* occurs at *a date*,
    *an event* is a transfer of *an asset* by *a transferor* to *a transferee*,
    *an event* is part of genuine restructuring according to other legislation,
    *an event* meets the ultimate ownership test at *a time*,
    *an event* is undertaken as part of *a trust*,
    *a trust* relates to *a family group*,
    *an individual* is a member of *a group*,
    *an individual* is an ultimate owner of *an asset* at *a time*,
    *a trust* is a family trust,
    *a tax payer* is in *a transferee*,
    *a time* is after *a second time*,
    *a party* is a party of *an event*,
    *an amount* is the aggregated turnover of *a party*,
    *a party* is an eligible party,
    *a previous time* is immediately before *a time*,
    *an asset* is ultimately owned by *a share* with *an owner* at *a time*,
    *an entity* has *a share* in *an asset* at *a time*,
    *an owner* owns *an asset* at *a time* according to other legislation,
    *a party* is a small business entity according to other legislation,
    *a party* has affiliated with *an affiliate* according to other legislation,
    *a party* is connected to *a taxpayer* according to other legislation,
    *a party* is a partner in partnership with *a partnership* according to other legislation,
    *an asset* is used in business of *a taxpayer* according to other legislation,
    *an asset* is of type *a type* according to other legislation,
    *a party* has an aggregated turnover of *an amount*,
    *a party* has an aggregated turnover of *an amount* according to other legislation,
    *a cost* is a rollover cost,
    *an asset* costs *a cost* at *a time*,
    *an owner* / *a thing* is in *a set*,
    *an asset* is an eligible asset.
    
the ontology is:
    cgt asset is an asset.
    trading stock is an asset.
    revenue asset is an asset.
    depreciating asset is an asset.
    loan to shareholder is an asset. 
    an asset is a type
        if  the type is a asset
        and the asset is of type the type according to other legislation.
    an asset is an active asset
        if the asset is used in business of a taxpayer according to other legislation.

the knowledge base 3_rollover_3 includes:
%An Event ID rollover of the transfer of an Asset from a Transferor to a Transferees List at a Time applies
%    if  this information transfer_event(the Event ID,the Asset,the Time,the Transferor,the Transferees List) has been recorded
%    and the Event ID rollover applies.

A tax payer is a party of an event
   if the event of transfering an asset from the tax payer to a recipient at a time occurs.

A tax payer is a party of an event
    if the event of transfering an asset from a somebody to a transferee at a time occurs
    and the tax payer is in the transferee.
    
the small business restructure rollover applies to an event
        if the event occurs at a time
        and the event is a transfer of an asset by a transferor to a transferee
        and for all cases in which
                a party is a party of the event
                and an amount is the aggregated turnover of the party
            it is the case that
                the party is an eligible party
                and the amount < 10000000.

an event meets the ultimate ownership test at a time
        if the event is a transfer of an asset by a transferor to a transferee
        and a previous time is immediately before the time
        and for all cases in which
                the asset is ultimately owned by an entity with an ultimate owner at the time
                and the entity has a share in the asset at the time
            it is the case that
                the entity is an ultimate owner of the asset at the previous time
                and the entity has the share in the asset at the previous time.

an event meets the ultimate ownership test at a time
        if the event is a transfer of an asset by a transferor to a transferee
        and the event is undertaken as part of a trust
        and the trust is a family trust
        and the trust relates to a family group
        and a previous time is immediately before the time
        and for all cases in which
                an individual is an ultimate owner of the asset at the time
            it is the case that
                the individual is a member of the family group
        and for all cases in which
                the asset is ultimately owned by an second individual with an ultimate owner at the time
            it is the case that
                the second individual is a member of the family group.

An asset is ultimately owned by 1 with an owner at a time
    if  the owner owns the asset at the time according to other legislation.

A party is an eligible party
    if the party is a small business entity according to other legislation.

A party is an eligible party
    if the party has affiliated with an affiliate according to other legislation
    and the affiliate is a small business entity according to other legislation.

A party is an eligible party
    if the party is connected to a taxpayer according to other legislation
    and the taxpayer is a small business entity according to other legislation.

A party is an eligible party
    if the party is a partner in partnership with a partnership according to other legislation
    and the partnership is a small business entity according to other legislation.

An asset is an eligible asset
    if  the asset is an active asset
    and the asset is a a type
    and the type is in [cgt event,depreciating asset,trading stock,revenue asset].

A party has an aggregated turnover of an amount
    if the party has an aggregated turnover of the amount according to other legislation.

A cost is a rollover cost
    if  an event occurs at a time
    and the event is a transfer of an asset by a transferor to a transferee
    and a previous time is immediately before the time
    and the asset costs the cost at the previous time.
    

query one is:
    the small business restructure rollover applies to which event.
    
query two is:
    which tax payer is a party of which event.
        
    
scenario Testing is:
    event 123 occurs at 2016-07-01.
    event 123 is a transfer of company1 goodwill by a company1 to miguel.
    company1 goodwill is an active asset. 
    company1 goodwill is a trading stock. 
    event 123 is part of genuine restructuring according to other legislation.
    event 123 of transfering company1 goodwill from company1 to miguel at 2016-07-01 occurs.
    miguel has an aggregated turnover of 400000 according to other legislation.
    company1 has an aggregated turnover of 5300000 according to other legislation.

scenario Andrew email Feb 4 2021 is:
    company1 owns company1 goodwill at a time according to other legislation.
    company1 owns company1 trading_stock at a time according to other legislation.
    company1 owns company1 plant and equipment at a time according to other legislation.
    company1 owns company1 revenue asset at a time according to other legislation.
    miguel is a partner in partnership with company1 according to other legislation.
    company1 has an aggregated turnover of 5300000 according to other legislation.
    andrew has an aggregated turnover of 1000000 according to other legislation.
    miguel has an aggregated turnover of 500000 according to other legislation.
    company1 is a small business entity according to other legislation.
    andrew is a small business entity according to other legislation.
    miguel is a small business entity according to other legislation.
    event 123 of transfering company1 goodwill from company1 to [andrew,miguel] at 2020-07-01 occurs.
    company1 owns company1 goodwill at a time according to other legislation.
    company1 owns company1 trading_stock at a time according to other legislation.
    company1 owns company1 plant and equipment at a time according to other legislation.
    company1 owns company1 revenue asset at a time according to other legislation.
    event 123 is part of genuine restructuring according to other legislation.
    company1 goodwill is used in business of company1 according to other legislation.
    company1 goodwill is of type trading stock.
    
    
scenario Andrew email Feb 4 2021 version 2 is:
    miguel is a partner in partnership with company1 according to other legislation.
    company1 has an aggregated turnover of 5300000 according to other legislation.
    andrew has an aggregated turnover of 1000000 according to other legislation.
    miguel has an aggregated turnover of 500000 according to other legislation.
    company1 is a small business entity according to other legislation.
    andrew is a small business entity according to other legislation.
    miguel is a small business entity according to other legislation.
    event 123 of transfering company1 goodwill from company1 to [andrew,miguel] at 2020-07-01 occurs.
    
"). 
% end of en('3_rollover_3', ...

%4
en('4_affiliates_3', 'This example covers affiliates entities for tax purposes.', "the target language is: prolog.

the predicates are:
    *a term* must not be a variable,
    *a term* must be nonvar,
    *an entity* has affiliated with *an affiliate* at *a date*,
    *a date* is not earlier than *a second date*,
% commenting out some templates for now, cf. Jacinto's email Nov 25 2025 to Miguel:
%    *an affiliate* is an individual or is a company,
%    *an affiliate* is a trust,
%    *an affiliate* is a partnership,
%    *an affiliate* is a superannuation fund,
    *an affiliate* acts in accordance with directions from *an entity*,
    *an affiliate* acts in concert with *an entity*,
    *an affiliate* is affiliated per older legislation with *an entity*,
    *an entity* is a trust according to other legislation,
%    *an entity* is a partnership,
    *an entity* is a partnership according to other legislation,
    *an entity* is a superannuation fund according to other legislation.
    
the ontology is:

an entity is a trust
    if the entity is a trust according to other legislation.

an entity is a partnership
    if the entity is a partnership according to other legislation.

an entity is a superannuation fund
    if the entity is a superannuation fund according to other legislation.

the knowledge base 4_affiliates_3 includes:

an entity has affiliated with an affiliate at a date
    if the date is not earlier than 2009-01-01
    and the affiliate is an individual 
        or the affiliate is a company
    and it is not the case that
        the affiliate is a trust
    and it is not the case that
        the affiliate is a partnership
    and it is not the case that
        the affiliate is a superannuation fund
    and the affiliate acts in accordance with directions from the entity
        or  the affiliate acts in concert with the entity.

an entity has affiliated with an affiliate at a date
    if  the date is known
    and the date is before 2009-01-01
    and the affiliate is affiliated per older legislation with the entity. 
    
a date P is not earlier than a date A
    if P is A
    or 	P is known
    	and A is known
    	and A is before P. 
    
scenario test is:
    his company is a company. 
    his company acts in accordance with directions from andrew.
    his company is affiliated per older legislation with andrew.
    
query one is:
    andrew has affiliated with his company at 2020-01-02.

query two is:
    who affiliated with which entity at which date. 

query three is:
    what entity affiliated with which company at which date. 

query four is:
    when did which entity affiliate with which other entity.

query five is:
    on what date did which entity affiliate with which company. 

query six is:
    is there an affiliation between which entity and which company. 

query seven is:
    when did the affiliation between which entity and which company begin.
    "). 
% end of en('4_affiliates_3', ...

%5
en(augmentedsem, "This example covers augmented semantics in ontology handling", "the target language is: prolog. 

the templates are:
*a label* is a label for *a description*.
the dataset ID is *a dataset*. 
the dataset *a dataset* is described at *a location*. 
the dataset *a dataset* can be downloaded from *a url*.   
the provider of *a dataset* is *a provider*.
the platform of *a dataset* is *a platform*.
the method of delivery of *a dataset* is *a method*. 
the product version of *a dataset* included here is *a date*. 

the ontology is:
    
the dataset ID is FDIC Insured Banks. 
%the dataset FDIC Insured Banks is described at https://hub.arcgis.com/datasets/geoplatform::fdic-insured-banks/about . 
%the dataset FDIC Insured Banks can be downloaded from https://hub.arcgis.com/datasets/geoplatform::fdic-insured-banks/ .   
the provider of FDIC Insured Banks is GeoPlatform ArcGIS Online.
the platform of FDIC Insured Banks is ArcGIS.
the method of delivery of FDIC Insured Banks is http. 
the product version of FDIC Insured Banks included here is 2018-06-29:00:00. 

%Column Label to Meaning Mappings:
X is a label for Latitude. 
Y is a label for Longitude. 
OBJECTID is a label for ID. 
ACQDATE is a label for Acquisition Date. 
ADDRESS is a label for Branch Address. 
ADDRESS2 is a label for Street Address Line 2. 
BKCLASS is a label for Institution Class. 
CBSA is a label for Core Based Statistical Areas (Branch). 
CBSA_DIV is a label for Metropolitan Divisions Name (Branch). 
CBSA_DIV_FLG is a label for Metropolitan Divisions Flag (Branch). 
CBSA_DIV_NO is a label for Metropolitan Divisions Number (Branch). 
CBSA_METRO is a label for Metropolitan Division Number (Branch). 
CBSA_METRO_FLG is a label for Metropolitan Division Flag (Branch). 
CBSA_METRO_NAME is a label for Metropolitan Division Name (Branch). 
CBSA_MICRO_FLG is a label for Micropolitan Division Flag (Branch). 
CBSA_NO is a label for Core Based Statistical Area Name (Branch). 
CERT is a label for Institution FDIC Certificate #. CITY is a label for Branch City. 
COUNTY is a label for Branch County. 
CSA is a label for Combined Statistical Area Name (Branch). 
CSA_FLG is a label for Combined Statistical Area Flag (Branch). 
CSA_NO is a label for Combined Statistical Area Number (Branch). 
ESTYMD is a label for Branch Established Date. 
FI_UNINUM is a label for FDIC UNINUM of the Owner Institution. 
ID is a label for ID. LATITUDE is a label for Latitude. 
LONGITUDE is a label for Longitude. 
MAINOFF is a label for Main Office. 
MDI_STATUS_CODE is a label for Minority Status Code. 
MDI_STATUS_DESC is a label for Minority Status Description. 
NAME is a label for Institution Name. 
OFFNAME is a label for Office Name. 
OFFNUM is a label for Branch Number. 
RUNDATE is a label for Run Date. 
SERVTYPE is a label for Service Type Code. 
SERVTYPE_DESC is a label for Service Type Description. 
STALP is a label for Branch State Abbreviation. 
STCNTY is a label for State and County Number. 
STNAME is a label for Branch State. 
UNINUM is a label for Unique Identification Number for a Branch Office. 
ZIP is a label for Branch Zip Code.

an object is of a type
    if a label is a label for the type
    and the object is of the label.  % connection to the dataset 
    
%Ontological Mappings:
%Spatial/Geographic Mappings (GeoSPARQL/GeoNames):

    X is a geo:lat (latitude coordinate). % as in http://www.opengis.net/ont/geosparql#lat
    Y is a geo:long (longitude coordinate). % as in http://www.opengis.net/ont/geosparql#long
    LATITUDE is a geo:lat (latitude coordinate). % as in http://www.opengis.net/ont/geosparql#lat
    LONGITUDE is a geo:long (longitude coordinate). % as in http://www.opengis.net/ont/geosparql#long
    OBJECTID is a geo:SpatialObject. % as in http://www.opengis.net/ont/geosparql#SpatialObject
    ADDRESS is a locn:Address. % as in http://www.w3.org/ns/locn#Address
    ADDRESS2 is a locn:addressArea. % as in http://www.w3.org/ns/locn#addressArea
    CITY is a gn:populatedPlace. % as in http://www.geonames.org/ontology#populatedPlace
    COUNTY is a gn:administrativeDivision. % as in http://www.geonames.org/ontology#administrativeDivision
    STNAME is a gn:administrativeDivision. % as in http://www.geonames.org/ontology#administrativeDivision
    STALP is a gn:countryCode. % as in http://www.geonames.org/ontology#countryCode
    ZIP is a locn:postCode. % as in http://www.w3.org/ns/locn#postCode

%Financial/Business Mappings (FIBO):
    BKCLASS is a fibo-be-le-lp:BusinessEntity. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/LegalPersons/BusinessEntity
    NAME is a fibo-fnd-org-fm:FormalOrganization. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/FormalOrganization
    CERT is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    FI_UNINUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    UNINUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    OFFNAME is a fibo-be-le-fbo:Branch. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/FormalBusinessOrganizations/Branch
    OFFNUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    MAINOFF is a fibo-be-le-fbo:HeadOffice. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/FormalBusinessOrganizations/HeadOffice
    SERVTYPE is a fibo-fbc-fct-fse:FinancialService. % as in https://spec.edmcouncil.org/fibo/ontology/FBC/FunctionalEntities/FinancialServicesEntities/FinancialService
    SERVTYPE_DESC is a fibo-fbc-fct-fse:FinancialServiceDescription. % as in https://spec.edmcouncil.org/fibo/ontology/FBC/FunctionalEntities/FinancialServicesEntities/FinancialServiceDescription

%Temporal Mappings (W3C Time Ontology):
    ACQDATE is a time:Instant. % as in http://www.w3.org/2006/time#Instant
    ESTYMD is a time:Instant. % as in http://www.w3.org/2006/time#Instant
    RUNDATE is a time:Instant. % as in http://www.w3.org/2006/time#Instant

%Statistical/Administrative Mappings (SDMX, Dublin Core):
    CBSA is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    CBSA_DIV is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    CSA is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    STCNTY is a sdmx-concept:administrativeClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#administrativeClassification

%General Data Mappings (DCAT, Dublin Core):
    ID is a dct:identifier. % as in http://purl.org/dc/terms/identifier
    CBSA_DIV_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CBSA_METRO_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CBSA_MICRO_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CSA_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    MDI_STATUS_CODE is a skos:Concept. % as in http://www.w3.org/2004/02/skos/core#Concept
    MDI_STATUS_DESC is a skos:prefLabel. % as in http://www.w3.org/2004/02/skos/core#prefLabel

%Demographic/Social Mappings (FOAF, Schema.org):
    MDI_STATUS_CODE is a schema:demographicGroup. % as in http://schema.org/demographicGroup
    MDI_STATUS_DESC is a schema:description. % as in http://schema.org/description

query thing is:
  which thing is a which type.

query label is:
  which label is a label for Institution Class. 

query description is:
  CBSA is a label for which description. 

query dataset is:
	the dataset ID is which one.
    
query about is:
    the dataset FDIC Insured Banks is described at which text.
  
query download is:
    the dataset FDIC Insured Banks can be downloaded from which url.
   
query provider is:
    the provider of FDIC Insured Banks is which provider.
    
query platform is:
    the platform of FDIC Insured Banks is which platform.

query method is:
    the method of delivery of FDIC Insured Banks is which method. 
    
query version is:
    the product version of FDIC Insured Banks included here is which version. 
").
% end of en(augmentedsem, ...

%6
en('cgt_assets', 'This example covers CGT assets as an improvement on 1_cgt_assets_and_exemptions_3', "the target language is: prolog. 
    
the templates are:

  *an asset* costed *an amount* to acquire.
  *a thing* meets the definition of plant and equipment under Division 43 of the Income Tax Act.
  *an asset* is used soley for taxable income generating purposes. 
  *an asset* is used purely for personal use purposes. 
  *an award* was secured for valour or brave conduct.
  *an asset* was bought.
  *an asset* was secured in exchange for *an other asset*.
  *an asset* has been secured through a game or a competition. 
  *an asset* corresponds to compensation or damages.
  *an asset* was received for an injury at *a place* of *a taxpayer*. 
  *an asset* was received for an injury at *a place* of *a taxpayer* s relative.
  *an asset* was received under an Unlawful Termination Assistance Scheme.
  *an asset* was received under an alternative Dispute Resolution Assistance Scheme.
  *an asset* was received under an M4/M5 Cashback Scheme. 
  *an asset* was received under a scheme established under legislation by an Australian Government agency or a foreign government agency.
  *an asset* was received under a scheme established under legislation by a local government body.
  *an asset* was received under a scheme established under legislation by a foreign government agency. 
  *an asset* is an interest in *a fund*. 
  *a fund* has *a number* of members.
  *an asset* is being transferred because of a relationship breakdown. 
  *an asset* was created in *an agreement*. 
  *an asset* was ended in *an agreement*. 
  *an asset* arose from the maturity of *a policy*. 
  *an instrument* is an annuity instrument for *an asset*. 
  *an asset* of *a taxpayer* is a CGT exempt asset.
  *an asset* is for surrender of *a policy*. 
  *a taxpayer* is the original beneficial owner of *a policy*. 
  *an asset* is held in a pooled development fund. 
  *an asset* is held in certain venture capital entity.
  *an asset* is held in an early stage venture capital limited partnership. 
  *an asset* was gifted through a will to a deductible gift recipient beneficiary.
  *an asset* belongs to *a person*. 
  *a person* was a resident of Norfolk Island at *a date*. 
  *an asset* was acquired at *a date*.
  *a thing* is a share or a unit or similar investments.
  *a thing* is a rental property plant or equipment.
  *an asset* is used for the purchase of items for personal consumption. 
  *an arrangement* for *an asset* is *a state*. 
  
the ontology is:

% A CGT asset is:
% (a) any kind of property; or
% (b) a legal or equitable right that is not property.

a thing is a CGT asset
  if the thing is a property
  or    the thing is a legal right
        or the thing is an equitable right
    and it is not the case that
        the thing is a property. 

% List of assets - List of CGT assets and exemptions - https://www.ato.gov.au/Individuals/Capital-gains-tax/List-of-cgt-assets-and-exemptions/

% Real estate assets
% Shares, units & similar investments
% Personal use assets
% Collectables assets
% Intangible assets
% Foreign currency assets
% Depreciating assets
% Award & payout assets
a thing is a property 
  if the thing is a real state asset
  or the thing is a share or a unit or similar investments
  or the thing is a personal use asset
  or the thing is a collectable asset
  or the thing is an intangible asset
  or the thing is a foreign currency asset
  or the thing is a depreciating asset
  or the thing is an award or payout asset. 

% Real estate assets list
% Main residence
% Granny flat
% Vacant land
% Business premises
% Rental properties
% Holiday houses
% Hobby farms
% Farms
a thing is a real state asset
  if the thing is a main residence
  or the thing is a granny flat
  or the thing is a vacant land
  or the thing is a business premise
  or the thing is a holiday house
  or the thing is a hobby farm
  or the thing is a farm. 

% Main residence is a CGT exempt asset if it is used purely for personal use.
an asset is a CGT exempt asset
  if the asset is used purely for personal use purposes
  and the asset is a main residence.

% Granny flat is a CGT exempt asset if an eligible granny flat arrangement is created, varied or terminated.
an asset is a CGT exempt asset
  if the asset is a granny flat
  and an arrangement for the asset is created
      or the arrangement for the asset is varied 
      or the arrangement for the asset is terminated
  and the arrangement is a granny flat arrangement. 

% Shares, units & similar investments assets list
% Shares
% Units
% Convertible notes
% Rights to aquire shares or units
% Options to acquire shares or units
% Stapled securities
% Cryptocurrencies
a thing is a share or a unit or similar investments
  if the thing is a share
  or the thing is a unit
  or the thing is a convertible note
  or the thing is a right to acquire shares or units
  or the thing is an option to acquire shares or units
  or the thing is a stapled security
  or the thing is a cryptocurrency. 

% Cryptocurrency is a CGT exempt asset if it is used for the purchase of items for personal consumption.
an asset is a CGT exempt asset
  if the asset is a cryptocurrency
  and the asset is used for the purchase of items for personal consumption.

% Personal use assets list
% boats
% furniture
% electrical goods
% household items
% option or right to acquire a personal use asset
% An IOU / loan to family member
% An IOU / loan to family friend
% An IOU related to a personal use asset
a thing is a personal use asset
  if the thing is a boat
  or the thing is a furniture
  or the thing is an electrical good
  or the thing is a household item
  or the thing is an option or right to acquire a personal use asset
  or the thing is an IOU as a loan to a family member
  or the thing is an IOU as a loan to a family friend
  or the thing is an IOU related to a personal use asset. 

% A personal use asset is not a CGT exempt asset if the asset cost the person $10,000 or more to acquire.
an asset is a CGT exempt asset
  if the asset is a personal use asset
  and the asset costed an amount to acquire 
  and the amount < 10000.  

% Collectables assets list
% artwork
% jewellery
% antiques
% rare coins 
% rare medallions
% rare stamps
% rare folios
% rare manuscripts
% rare books
a thing is a collectable asset
  if the thing is an artwork
  or the thing is a jewellery
  or the thing is an antique
  or the thing is a rare coin
  or the thing is a rare medallion
  or the thing is a rare stamp
  or the thing is a rare folio
  or the thing is a manuscript
  or the thing is a book. 

% If an entity makes a capital loss on disposal of a collectable then that loss can only be offset against a capital gain on the disposal of a collectable.

% Intangibles assets list
% Leases
% goodwill
% licences
% contractual rights
a thing is a intangible asset
  if the thing is a lease
  or the thing is a goodwill
  or the thing is a license
  or the thing is a contractual right. 

% Depreciating assets list
% business equipment
% plant and equipment found in a rental property
a thing is a depreciating asset
  if the thing is a business equipment
  or the thing is a rental property plant or equipment.

% Plant and equipment is rental property plant and equipment if it is a removable asset in a rental property
%   and it meets the definition of plant and equipment under Division 43 of the Income Tax Act.
a thing is a rental property plant or equipment
  if the thing is a removable asset in a rental property
  and the thing meets the definition of plant and equipment under Division 43 of the Income Tax Act.

% A depreciating asset is CGT exempt asset if it is used soley for taxable income generating purposes.
an asset is a CGT exempt asset
  if the asset is a depreciating asset
  and the asset is used soley for taxable income generating purposes.

% A motor vehicle is a CGT exempt asset if it is used purely for personal use purposes.
an asset is a CGT exempt asset
  if the asset is a motor vehicle
  and the asset is used purely for personal use purposes.

% A motor cycle is a CGT exempt asset if it is used purely for personal use purposes.
an asset is a CGT exempt asset
  if the asset is a motor cycle
  and the asset is used purely for personal use purposes.

% An award is a CGT exempt asset if it was secured for valour or brave conduct.
% An award is not a CGT exempt asset if it was bought or if it was secured in exchange for another asset. 
an asset is a CGT exempt asset
  if the asset is an award
  and the asset was secured for valour or brave conduct
  and it is not the case that
      the asset was bought
      or the asset was secured in exchange for another asset. 

% Gambling winnings are a CGT exempt asset if they are secured through a game or a competition.
an asset is a CGT exempt asset
  if the asset is a gambling winning
  and the asset has been secured through a game or a competition. 

% Compensation or damages is a CGT exempt asset if received for an injury you or your relative suffered at work.
% Compensation or damages is a CGT exempt asset if received for an injury you or your relative suffered anywhere.
% Compensation or damages is a CGT exempt asset if received under an Unlawful Termination Assistance Scheme.
% Compensation or damages is a CGT exempt asset if received under an alternative Dispute Resolution Assistance Scheme.
% Compensation or damages is a CGT exempt asset if received under an M4/M5 Cashback Scheme.
% Compensation or damages is a CGT exempt asset if received under a scheme established under legislation by an Australian Government agency,  
% or a foreign government agency.
an asset is a CGT exempt asset
  if the asset corresponds to compensation or damages
  and the asset was received for an injury at a place of a taxpayer
      or the asset was received for an injury at place of the taxpayer s relative
      or the asset was received under an Unlawful Termination Assistance Scheme
      or the asset was received under an alternative Dispute Resolution Assistance Scheme
      or the asset was received under an M4/M5 Cashback Scheme
      or the asset was received under a scheme established under legislation by an Australian Government agency or a foreign government agency.

% Receipt of compensation or damages is a CGT exempt asset if received under a scheme established under legislation by a local government body.
% Receipt of compensation or damages is a CGT exempt asset if received under a scheme established under legislation by a foreign government agency.
an asset is a CGT exempt asset
  if the asset is a receipt for compensation or damages
  and the asset was received under a scheme established under legislation by a local government body
      or the asset was received under a scheme established under legislation by a foreign government agency. 

% An interest in one small super fund (a complying fund that has less than 5 members) is a CGT exempt asset if 
% the rights are transferred to another because of a relationship breakdown between spouses or former spouses.
an asset is a CGT exempt asset
  if the asset is an interest in a super fund
  and the super fund has a number of members
  and the number < 5
  and the asset is being transferred because of a relationship breakdown. 

% Rights are a CGT exempt asset if created or ended in a superannuation agreement.
an asset is a CGT exempt asset
  if the asset is a right
  and the asset was created in an agreement
      or the asset was ended in the agreement
  and the agreement is a superannuation agreement. 

% A payout under a general insurance policy is a CGT exempt asset if it arises from the maturity of the policy.
% A payout under a life insurance policy is a CGT exempt asset if it arises from the maturity of the policy.
an asset is a CGT exempt asset
  if the asset is a payout
  and the asset arose from the maturity of a policy
  and the policy is a general insurance policy for the asset
      or the policy is a life insurance policy for the asset.

% A payout under an annuity instrument is a CGT exempt asset if it arises from the maturity of the instrument.
an asset is a CGT exempt asset
  if the asset is a payout
  and the asset arose from the maturity of an instrument
  and the instrument is an annuity instrument for the asset. 

% A payment for surrender of an insurance policy is a CGT exempt asset where you are the original beneficial owner of the policy.
an asset of a taxpayer is a CGT exempt asset
  if the asset is a payment
  and the asset is for surrender of an insurance policy
  and the taxpayer is the original beneficial owner of the policy. 

% A share is a CGT exempt asset if held in a pooled development fund.
% A share is a CGT exempt asset if held in certain venture capital entity.
% A share is a CGT exempt asset if held in an early stage venture capital limited partnership.
an asset is a CGT exempt asset
  if the asset is a share 
  and the asset is held in a pooled development fund
      or the asset is held in certain venture capital entity
      or the asset is held in an early stage venture capital limited partnership. 

% A  gift is a CGT exempt asset if gifted through a will to a deductible gift recipient beneficiary.
an asset is a CGT exempt asset
  if the asset is a gift
  and asset was gifted through a will to a deductible gift recipient beneficiary.

% An asset is a CGT exempt asset if held by a person who was a resident of Norfolk Island before 24 October 2015
%   and the asset was acquired on Norfolk Island before 24 October 2015.
an asset is a CGT exempt asset
  if the asset belongs to a person
  and the person was a resident of Norfolk Island at a date
  and the date is before 2015-10-24.

% An asset is a CGT exempt asset if the asset was acquired on or before 19 September 1985.
an asset is a CGT exempt asset
  if the asset was acquired at a date
  and the date is before or equal to 1985-09-19. 
        
scenario Colin is:
    asset one belongs to Colin.
    Colin was a resident of Norfolk Island at 2000-01-01.
    asset two was acquired at 1985-09-19.  

scenario Crypto is: 
    Alice is a taxpayer.
    asset bitcoin is a cryptocurrency. 
    asset bitcoin belongs to Alice.
    asset bitcoin was acquired at 2020-01-01.
    asset bitcoin is used for the purchase of items for personal consumption.
    asset nft_rare belongs to Alice.
	asset nft_rare costed 15000 to acquire.
    asset nft_gift is a gift. 
	asset nft_gift belongs to Alice.
    asset nft_gift was gifted through a will to a deductible gift recipient beneficiary.

scenario Farmer is:
    Bob is a taxpayer.
    asset hobby_farm is a farm. 
    asset hobby_farm belongs to Bob.
    asset hobby_farm is used purely for personal use purposes.
    asset commercial_farm is a farm. 
    asset commercial_farm belongs to Bob.
    asset commercial_farm is used soley for taxable income generating purposes.
    asset tractor_business is a business equipment. 
    asset tractor_business belongs to Bob.
    asset tractor_business is a rental property plant or equipment.
    asset tractor_business meets the definition of plant and equipment under Division 43 of the Income Tax Act.
    asset tractor_vintage belongs to Bob.
    asset tractor_vintage costed 8000 to acquire. 

scenario Legal is:
    Carol is a taxpayer.
    asset land_right belongs to Carol.
    asset leasehold belongs to Carol.
    asset lawsuit_settlement belongs to Carol.
    asset lawsuit_settlement corresponds to compensation or damages.
    asset lawsuit_settlement was received for an injury at workplace of Carol.   

scenario Loan is:
    David is a taxpayer.
    asset iou_loan belongs to David.
    asset boat belongs to David.
    asset boat costed 9500 to acquire.
    asset boat is used purely for personal use purposes.
    asset stamps belongs to David.
    asset stamps was acquired at 1980-01-01.

scenario Insurance is: 
    Emma is a taxpayer.
    asset insurance_payout belongs to Emma.
    asset insurance_payout arose from the maturity of a policy.
    asset super_interest belongs to Emma.
    asset super_interest is an interest in a fund.
    %the fund has 4 members.
    asset super_interest is being transferred because of a relationship breakdown.
    asset medal belongs to Emma.
    asset medal was secured for valour or brave conduct.  

scenario Mixed is:
    Frank is a taxpayer.
    asset holiday_house belongs to Frank.
    asset granny_flat belongs to Frank.
    the arrangement for asset granny_flat is created.
    the arrangement is a granny flat arrangement.
    asset fridge belongs to Frank.
    asset fridge is a rental property plant or equipment.
    asset fridge meets the definition of plant and equipment under Division 43 of the Income Tax Act.

% Check if your assets are subject to CGT, exempt, or pre-date CGT.
query cgt asset is:
  which thing is a CGT asset.

query exempt asset is:
  which asset is a CGT exempt asset. 

query predate is:
  which asset is a pre-date CGT asset. 
"). 
% end of en('cgt_assets', ...

%7
en(citizenship, "This is the classical example about the British Nationality Act", "the target language is: prolog.

the templates are:
*a person* acquires British citizenship on *a date*.
*a person* is born in *a place* on *a date*,
*a date* is after commencement,
*a person* is the mother of *a person*,
*a person* is the father of *a person*,
*a person* is a British citizen on *a date*,
*a person* is settled in the UK on *a date*,
*a person* says that *a sentence*,
*a person* is qualified to determine fatherhood.

the knowledge base citizenship includes:
    
a person acquires British citizenship on a date
if the person is born in the UK on the date
and the date is after commencement
and an other person is the mother of the person
    or the other person is the father of the person
and the other person is a British citizen on the date
    or the other person is settled in the UK on the date.
    
a person is the father of an other person
    if a third person says 
    that the person is the father of the other person
    and the third person is qualified to determine fatherhood.
    
a person X is the father of a person Y
    if X says that X is the father of Y.  
    
scenario alice is:
    John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Alice is the mother of John.
	Alice is a British citizen on 2021-10-09.
    
scenario harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Harry is the father of John.
	Harry is settled in the UK on 2021-10-09.

scenario trust_harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Harry says that Harry is the father of John.
	Harry is settled in the UK on 2021-10-09.
    
scenario alice_harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Alice is the mother of John.
	Alice says that Harry is the father of John.
    Alice is qualified to determine fatherhood.
	Harry is settled in the UK on 2021-10-09.
      
query one is:
    which person acquires British citizenship on which date.

").
% end of en(citizen, ...

%8
en(gst, "This example covers Goods and Services Tax (GST) concepts", "the target language is: prolog. 
    
the templates are:
*an entity* does not operate for profit.
*an entity* must register for GST. 
*an entity* has *a turnover*. 
*an entity* projected turnover is *a turnover*. 
*an entity* has *a turnover*. 
the turnover of *an entity* for the current month is *an amount*.    
the turnover of *an entity* for the previous 11 months is *an amount*. 
the turnover of *an entity* for the next 11 months is *an amount*.
    
the ontology is:
    
sporting club is an entity.
charity is an entity.
community group is an entity.
professional association is an entity.    

an entity is a not-for-profit if the entity does not operate for profit. 
        
the knowledge base gst includes:

an entity must register for GST 
    if the entity is a not-for-profit
    and		the entity has a turnover 
    		and the turnover >= 150000
    	or	the entity projected turnover is a projection 
    		and the projection >= 150000. 

an entity must register for GST 
    if it is not the case that
    	the entity is a not-for-profit
    and 	the entity has a turnover 
   			and the turnover >= 75000 
    	or the entity projected turnover is a projection
    		and the projection >= 75000.

an entity has a current turnover
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the previous 11 months is a past amount
    and the current turnover = monthly amount + past amount. 

an entity projected turnover is a projection
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the next 11 months is a future amount
    and the projection = monthly amount + future amount. 
    
scenario A is:
Cherrio Charity is an entity. 
Cherrio Charity does not operate for profit. 
Cherrio Charity has 160000. 
    
query A is:
Cherrio Charity must register for GST. 
% yes
    
scenario B is: 
sporting club does not operate for profit.
sporting club projected turnover is 100000. 
        
query B is:
sporting club must register for GST. 
% no
    
scenario C is: 
Tom s Bakery is an entity.
Tom s Bakery has 80000.
    
query C is: 
Tom s Bakery must register for GST.
% yes
    
query not-for-profit is:
which entity is a not-for-profit. ").
% end of en(gst, ...

%9
en(gstturnover, "This example covers GST turnover and total business income concepts", "the target language is: prolog. 
    
the templates are:
*an entity* does not operate for profit.
*an entity* must register for GST. 
*an entity* has *a turnover*. 
*an entity* projected turnover is *a turnover*. 
*an entity* has *a turnover*. 
the turnover of *an entity* for the current month is *an amount*.    
the turnover of *an entity* for the previous 11 months is *an amount*. 
the turnover of *an entity* for the next 11 months is *an amount*.
*an amount* is the total business income. 
*an amount* is the GST included in sales to your customers.
*an amount* is sales to associates that are not for payment and are not taxable. 
*an amount* is sales not connected with an enterprise you run. 
*an amount* is input-taxed sales you make.
*an amount* is sales not connected with Australia (export sales). 
*an amount* is sales of goods or services within Australia that are sales to third parties.
*an amount* is the GST Turnover. 
    
the ontology is:
    
sporting club is an entity.
charity is an entity.
community group is an entity.
professional association is an entity.
Uber driver is a ride-sourcing service.    

an entity is a not-for-profit if the entity does not operate for profit. 
        
the knowledge base gst includes:

an entity must register for GST
    if the entity is of a service
    and the service is a ride-sourcing service. 

an entity must register for GST 
    if the entity is a not-for-profit
    and		the entity has a turnover 
    		and the turnover >= 150000
    	or	the entity projected turnover is a projection 
    		and the projection >= 150000.  

an entity must register for GST 
    if it is not the case that
    	the entity is a not-for-profit
    and 	the entity has a turnover 
   			and the turnover >= 75000 
    	or the entity projected turnover is a projection
    		and the projection >= 75000.

an entity has a current turnover
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the previous 11 months is a past amount
    and the current turnover = monthly amount + past amount. 

an entity projected turnover is a projection
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the next 11 months is a future amount
    and the projection = monthly amount + future amount. 
    
a TBI is the total business income
    if a GST is the GST included in sales to your customers
	and a Sales_Associates is sales to associates that are not for payment and are not taxable 
	and a Disconnected_sales is sales not connected with an enterprise you run 
	and a Input_taxed is input-taxed sales you make
	and an Exports is sales not connected with Australia (export sales) 
	and a Sales is sales of goods or services within Australia that are sales to third parties
    and TBI = Sales Associates + Disconnected_sales + Input_taxed + Exports + Sales. 

a GST_Turnover is the GST Turnover
    if a TBI is the total business income
	and a GST is the GST included in sales to your customers
	and a Sales_Associates is sales to associates that are not for payment and are not taxable 
	and a Disconnected_sales is sales not connected with an enterprise you run 
	and a Input_taxed is input-taxed sales you make
	and an Exports is sales not connected with Australia (export sales) 
	and a Sales is sales of goods or services within Australia that are sales to third parties
    and GST_Turnover = TBI - GST - Sales_Associates - Disconnected_sales - Input_taxed - Exports. 
    
    
scenario A is:
Cherrio Charity is an entity. 
Cherrio Charity does not operate for profit. 
Cherrio Charity has 160000. 
    
query A is:
Cherrio Charity must register for GST. 
% yes
    
scenario B is: 
sporting club does not operate for profit.
sporting club projected turnover is 100000. 
        
query B is:
sporting club must register for GST. 
% no
    
scenario C is: 
Tom s Bakery is an entity.
Tom s Bakery has 80000.
    
query C is: 
Tom s Bakery must register for GST.
% yes
    
query not-for-profit is:
which entity is a not-for-profit. 
     
scenario typical is:
	9090.91 is the GST included in sales to your customers.
	0 is sales to associates that are not for payment and are not taxable. 
	0 is sales not connected with an enterprise you run. 
	5000 is input-taxed sales you make.
	0 is sales not connected with Australia (export sales). 
	100000 is sales of goods or services within Australia that are sales to third parties.

scenario significant exports is:
	6818.18 is the GST included in sales to your customers.
	10000 is sales to associates that are not for payment and are not taxable. 
	2000 is sales not connected with an enterprise you run. 
	0 is input-taxed sales you make.
	50000 is sales not connected with Australia (export sales). 
	75000 is sales of goods or services within Australia that are sales to third parties.

scenario small business is:
	0 is the GST included in sales to your customers.
	5000 is sales to associates that are not for payment and are not taxable. 
	1000 is sales not connected with an enterprise you run. 
	0 is input-taxed sales you make.
	0 is sales not connected with Australia (export sales). 
	0 is sales of goods or services within Australia that are sales to third parties. 
    
query tbi is:
    which amount is the total business income. 
    
query gst_turnover is: 
    which amount is the GST Turnover.  

scenario uber is:
    mario is an Uber driver. 

query who is:
	which entity must register for GST.
").
% end of en(gstturnover, ...

%10
en('journal_balance', 'This example covers journal and trail balance concepts', "the target language is: prolog. 
the templates are:

*a journal entry* has as ID *an ID*.
*a journal entry* has as date *a date*.
*a journal entry* has as description *a description*.

*a journal head* is part of *a journal entry*.
*a journal head* is a debit of *an amount* to *an account*.
*a journal head* is a credit of *an amount* to *an account*.

*a journal head* has *an amount*.
*a journal entry* is valid.
*a journal entry* is balanced.
*a journal entry* has at least 2 heads.
    
the total debits for *an account* is *a number*. 
the total credits for *an account* is *a number*. 

a trial balance is defined. 
*an account* has a net balance of *an amount*. 

the ontology is:

cash at bank is an account.
sales revenue is an account.
gst payable is an account.

the knowledge base trial balance includes:

% A journal entry is valid if it is dated, has at least 2 heads, and is balanced.
a journal entry J is valid
    if the journal entry J has as date a date D
    and the journal entry J has at least 2 heads
    and the journal entry J is balanced.

% Rule to check for at least 2 heads (as per constraint)
an entry has at least 2 heads
    if a number N is the sum of each I such that
    	I = 1
        and an H is part of the entry
    and N >= 2.

% Rule to check if the journal entry sums to zero (as per constraint)
an entry is balanced
    if a number S is the sum of each signed amount SA such that
        a H is part of the entry
        and H has the signed amount SA
    and S = 0.

% Rule to get the signed value for a debit (positive)
a header has an amount S
    if the header is a debit of an amount A to an account Acc
    and S = A.

% Rule to get the signed value for a credit (negative)
a header has an amount S
    if the header is a credit of an amount A to an account Acc
    and S = 0-A.

% == Trial Balance Rules ==
% 1. Calculate the total debits for a specific account.
the total debits for an account A is a number N
    if A is an account
    and N is the sum of each amount such that
        a H is a journal head
        and an JE is a journal entry
        and H is part of JE
        and H is a debit of the amount to A.

% 2. Calculate the total credits for a specific account.
the total credits for an account A is a number N
    if A is an account
    and N is the sum of each amount such that
        a H is a journal head
        and an JE is a journal entry
        and H is part of JE
        and H is a credit of the amount to A.

% 3. Calculate the net balance for a specific account.
an account A has a net balance of an amount N
    if the total debits for A is a number D
    and the total credits for A is a number C
    and N = D - C.

% 4. A trial balance is defined if the sum of all account balances equals zero.
a trial balance is defined
    if for all cases in which
            an entry is a journal entry
        it is the case that
            the entry is valid  % the following seems redundant
    and a number S is the sum of each balance such that
        an Acc is an account % Sum over all defined accounts. 
        and Acc has a net balance of the balance
    and S = 0.

scenario user_example is:
    JE-201 is a journal entry.
    JE-201 has as ID JE-201.
    JE-201 has as date 2025-10-23.
    JE-201 has as description Journal entry from user example.
    % Dr Bank - $100
    head-1 is a journal head.
    head-1 is part of JE-201.
    head-1 is a debit of 100 to cash at bank.
    % Cr sales - 110
    head-2 is a journal head.
    head-2 is part of JE-201.
    head-2 is a credit of 110 to sales revenue.
    % Dr GST Payable - 10
    head-3 is a journal head.
    head-3 is part of JE-201.
    head-3 is a debit of 10 to gst payable.

scenario balanced_tb is:
    % --- JE-100: Sale on account for $100 ---
    JE-100 is a journal entry.
    JE-100 has as date 2025-11-01.
    JE-100 has as description Cash sale.    
    % Dr Cash $100
    head-100-1 is a journal head.
    head-100-1 is part of JE-100.
    head-100-1 is a debit of 100 to cash at bank.
    % Cr Sales Revenue $100
    head-100-2 is a journal head.
    head-100-2 is part of JE-100.
    head-100-2 is a credit of 100 to sales revenue.
    % --- JE-101: GST Payment of $10 ---
    JE-101 is a journal entry.
    JE-101 has as date 2025-11-05.
    JE-101 has as description GST payment.
    % Dr GST Payable $10 (to reduce the liability)
    head-101-1 is a journal head.
    head-101-1 is part of JE-101.
    head-101-1 is a debit of 10 to gst payable.
    % Cr Cash $10
    head-101-2 is a journal head.
    head-101-2 is part of JE-101.
    head-101-2 is a credit of 10 to cash at bank.

scenario unbalanced_tb is:
    % --- Includes all entries from balanced_tb (JE-102) ---
    % --- JE-102: Error - Single-entry Debit $50 ---
    JE-102 is a journal entry.
    JE-102 has as date 2025-11-10.
    JE-102 has as description Error Cash Deposit recorded Credit side forgotten.the target language is: prolog
    % Dr Cash $50 (but no corresponding credit was recorded)
    head-102-1 is a journal head.
    head-102-1 is part of JE-102.
    head-102-1 is a debit of 50 to cash at bank.
    % Note: JE-102 itself is *invalid* as it has only one head, but its *effect* 
    % on the TB is an uncorrected balance error.

scenario unbalanced_tb_2 is:
    % --- Includes all entries from balanced_tb (JE-103) ---
    % --- JE-102: Error - Single-entry Debit $50 ---
    JE-103 is a journal entry.
    JE-103 has as date 2025-11-10.
    JE-103 has as description Error Cash Deposit recorded incorrect Credit.  
    % Dr Cash $50 (but no corresponding credit was recorded)
    head-103-1 is a journal head.
    head-103-1 is part of JE-103.
    head-103-1 is a debit of 50 to cash at bank.
    % Cr Sales $10 (incorrect credit recorded)
    head-103-2 is a journal head.
    head-103-2 is part of JE-103.
    head-103-2 is a credit of 10 to gst payable.
    % Cr Sales $50 (incorrect credit recorded)
    head-103-3 is a journal head.
    head-103-3 is part of JE-103.
    head-103-3 is a credit of 50 to sales revenue.
    % the TB is an uncorrected balance error.

query trial_balance is:
    a trial balance is defined.

query check_user_example is:
    JE-201 is valid.
").
% end of en('journal_balance', ...

%11
en(journal, "This example covers journal concepts", "the target language is: prolog. 
the templates are:

*a journal entry* has as ID *an ID*.
*a journal entry* has as date *a date*.
*a journal entry* has as description *a description*.

*a journal head* is part of *a journal entry*.
*a journal head* is a debit of *an amount* to *an account*.
*a journal head* is a credit of *an amount* to *an account*.

*a journal head* has *an amount*.
*a journal entry* is valid.
*a journal entry* is balanced.
*a journal entry* has at least 2 heads.

the ontology is:

cash at bank is an account.
sales revenue is an account.
gst payable is an account.

the knowledge base journal includes:

% A journal entry is valid if it is dated, has at least 2 heads, and is balanced.
a journal entry J is valid
    if the journal entry J has as date a date D
    and the journal entry J has at least 2 heads
    and the journal entry J is balanced.

% Rule to check for at least 2 heads (as per constraint)
a J has at least 2 heads
    if a number N is the sum of each I such that
    	I = 1
        and an H is part of J
    and N >= 2.

% Rule to check if the journal entry sums to zero (as per constraint)
a J is balanced
    if a number S is the sum of each signed amount SA such that
        a H is part of J
        and H has the signed amount SA
    and S = 0.

% Rule to get the signed value for a debit (positive)
a H has an amount S
    if H is a debit of an amount A to an account Acc
    and S = A.

% Rule to get the signed value for a credit (negative)
a H has an amount S
    if H is a credit of an amount A to an account Acc
    and S = 0-A.

scenario user_example is:
    JE-201 is a journal entry.
    JE-201 has as ID JE-201.
    JE-201 has as date 2025-10-23.
    JE-201 has as description Journal entry from user example.
    % Dr Bank - $100
    head-1 is a journal head.
    head-1 is part of JE-201.
    head-1 is a debit of 100 to cash at bank.
    % Cr sales - 110
    head-2 is a journal head.
    head-2 is part of JE-201.
    head-2 is a credit of 110 to sales revenue.
    % Dr GST Payable - 10
    head-3 is a journal head.
    head-3 is part of JE-201.
    head-3 is a debit of 10 to gst payable.

query check_user_example is:
    JE-201 is valid.").
% end of en(journal, ...

%12
en(payg, "This example covers Pay As You Go (PAYG) withholding concepts", "the target language is: prolog. 
    
the templates are:
    the estimated tax for *an entity* for *a year* is *an amount*. 
    the estimated annual net tax payable for *an entity* for *a year* is *an amount*.
    the estimated taxable income for *an entity* for *a year* is *an amount*.
    the applicable tax rate for *an entity* on *a year* is *a percentage*.
    *an entity* is under the aggregated turnover threshold in *a year*. 
    *an entity* is a base rate entity. 
    the tax offsets for *an entity* for *a year* is *an amount*.
    the estimated tax credits for *an entity* for *a year* are *an amount*.
    the varied amount payable for *a quarter* for *a year* by *an entity* is *an amount*.
    the year-to-date fraction for *a quarter* is *a fraction*.
    the year-to-date instalment adjustment for *an entity* for *a year* is *an amount*.
    the new varied rate for *an entity* for *a year* is *an amount*.
    the estimated PAYG instalment income for *an entity* for *a year* is *an amount*.
    *an amount* with *an ID* is an estimated ordinary income for *an entity* for *a year*.
    *an amount* with *an ID* is an estimated type of business or investment income for *an entity* for *a year*.
    *an amount* with *an ID* is specially included as estimated instalment income for *an entity* for *a year*.
    *an amount* with *an ID* is a type of estimated excluded income for *an entity* for *a year*.
    *an amount* with *an ID* was withheld as tax because of no TFN or ABN for *an entity* for *a year*.
    *an amount* with *an ID* was withheld from *an entity* under the PAYG withholding system.
    *an amount* with *an ID* was reported as an instalment on *a quarter* of *a year*.
    *an amount* with *an ID* was reported as a variation on *a quarter* of *a year*.
    the estimated gross rent for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated dividends for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated royalties for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated foreign pensions for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated partnership income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated trust income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated foreign income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated interest for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated gross sales for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated gross fees for services for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated withdrawals from farm management deposits for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated fuel tax credits for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated JobKeeper payments for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated capital gain for *an entity* for *a year* is *an amount* with *an ID*.
    *an entity* is a super fund.
    the estimated GST for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated wine equalisation tax for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated luxury car tax for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated salary and wages for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated franking credit for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated deemed dividend for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated exempt income for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated payment under the National Rental Affordability Scheme for *an entity* for *a year* is *an amount* with *an ID*.
    the estimated grant under the energy grants credits scheme for *an entity* for *a year* is *an amount* with *an ID*.
    *an entity* is the taxpayer.
    *a year* is a year under consideration.
    *a quarter* is previous or equal to *a quarter*.
    *a quarter* is previous to *a quarter*.
    the current quarter is *a quarter*.

the ontology is:
    quarter 1 is a quarter.
    quarter 2 is a quarter.
    quarter 3 is a quarter.
    quarter 4 is a quarter.
    Q1 is a quarter 1.
    Q2 is a quarter 2.
    Q3 is a quarter 3.
    Q4 is a quarter 4.

    a quarter X is previous or equal to a quarter Y 
        if X is previous to Y 
        or X is equal to Y.

    quarter 1 is previous to quarter 2.
    quarter 1 is previous to quarter 3.
    quarter 1 is previous to quarter 4.
    quarter 2 is previous to quarter 3.
    quarter 2 is previous to quarter 4.
    quarter 3 is previous to quarter 4.
    Q1 is previous to Q2.
    Q1 is previous to Q3.
    Q1 is previous to Q4.
    Q2 is previous to Q3.
    Q2 is previous to Q4.
    Q3 is previous to Q4.

the knowledge base payg includes:

% Estimated tax for the year
the estimated tax for an entity for a year is an amount ET
    if the estimated annual net tax payable for the entity for the year is ET
        and ET >= 0 
    or ET = 0
    	and it is not the case that
       		the estimated annual net tax payable for the entity for the year is an X.

% Estimated annual net tax payable for the year
the estimated annual net tax payable for an entity for a year is an amount ENT
    if the estimated taxable income for the entity for the year is an amount ETI
    and the applicable tax rate for the entity on the year is a percentage ATR
    and the tax offsets for the entity for the year is an amount TO
    and TO >= 0
    and the estimated tax credits for the entity for the year are an amount TC
    and TC >= 0
    and ENT = ETI * ATR - TO - TC. 

% Applicable tax rate (simplified)
% the entity is under the aggregated turnover threshold in the year -> Given!
the applicable tax rate for an entity on a year is a number ATR
    if         the entity is under the aggregated turnover threshold in the year 
            or the entity is a base rate entity
        and ATR is 0.25
    or      ATR is 0.30
    	and it is not the case that
    		the entity is under the aggregated turnover threshold in the year
        and it is not the case that
        	the entity is a base rate entity.

% Varied amount payable for the quarter
the varied amount payable for a quarter Q for an income year by an entity E is an amount VA
    if the current quarter is Q
    and the estimated tax for E for the income year is an amount ET
    and the year-to-date fraction for Q is a number F
    and the year-to-date instalment adjustment for E for the income year is an amount Y
    and VA = ET * F - Y.

% Year-to-date fraction for the quarter
the year-to-date fraction for a quarter Q is a number F
    if Q is quarter 1 
        and F is 0.25
    or Q is quarter 2 
        and F is 0.5
    or Q is quarter 3 
        and F is 0.75
    or Q is quarter 4 
        and F is 1.0.

% Year-to-date instalment adjustment for an entity for an income year
the year-to-date instalment adjustment for an entity E for an income year is an amount V
    if E is the taxpayer
    and the current quarter is a quarter C
    and the income year is a year under consideration
    and a number IR is the sum of each amount such that
            the amount with an ID was reported as an instalment on a quarter X of the income year
            and X is previous to C
    and a number IVC is the sum of each number such that
            the number with an other ID was reported as a variation on a quarter Y of the income year
            and Y is previous to C
    and V = IR - IVC.

% New varied rate for the year
the new varied rate for an entity for a year is a percentage VR
    if the estimated tax for the entity for the year is an amount ET
    and the year-to-date instalment adjustment for the entity for the year is an amount P
    and the estimated PAYG instalment income for the entity for the year is an amount PAYG
    and         PAYG > 0
            and VR = ((ET-P)/PAYG)*100
        or      PAYG = 0
            and VR = 0.

% instalment income (adapted to estimate PAYG instalment income with yearly basis)
the estimated PAYG instalment income for an entity for a year is a total amount 
    if the entity is the taxpayer
    and the year is a year under consideration
    and the total amount is the sum of each partial such that
        partial with an ID is an estimated ordinary income for the entity for the year. 

% --- General Rule --- (as sketched by Gemini and Andrew)
% The primary rule states that business/investment income is included, unless it's specifically excluded.          
an amount with an ID is an estimated ordinary income for an entity for a year 
    if the amount with the ID is an estimated type of business or investment income for the entity for the year
    and     it is not the case that
                the amount with the ID is a type of estimated excluded income for the entity for the year
        or the amount with the ID is specially included as estimated instalment income for the entity for the year. 

% An exception for income where tax was withheld due to no TFN/ABN. This is always included.
an amount with an ID is specially included as estimated instalment income for an entity for a year
    if   the amount with the ID was withheld as tax because of no TFN or ABN for the entity for the year. 

an amount E with an ID is an estimated type of business or investment income for an entity for a year
    if the estimated gross rent for the entity for the year is E with ID
    or the estimated dividends for the entity for the year is E with ID
    or the estimated royalties for the entity for the year is E with ID
    or the estimated foreign pensions for the entity for the year is E with ID
    or the estimated partnership income for the entity for the year is E with ID
    or the estimated trust income for the entity for the year is E with ID
    or the estimated foreign income for the entity for the year is E with ID
    or the estimated interest for the entity for the year is E with ID
    or the estimated gross sales for the entity for the year is E with ID
    or the estimated gross fees for services for the entity for the year is E with ID
    or the estimated withdrawals from farm management deposits for the entity for the year is E with ID
    or the estimated fuel tax credits for the entity for the year is E with ID
    or the estimated JobKeeper payments for the entity for the year is E with ID.

 % Capital gains are a special case, only included for super funds.
an amount I with an ID is specially included as estimated instalment income for an entity for a year
    if the estimated capital gain for the entity for the year is I with ID
    and the entity is a super fund.

% % --- Specific Exclusions (Types of Excluded Income) ---
an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated GST for the entity for the year is the amount with ID
    or the estimated wine equalisation tax for the entity for the year is the amount with ID
    or the estimated luxury car tax for the entity for the year is the amount with ID.

an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated salary and wages for the entity for the year is the amount with ID
    and the amount with ID was withheld from the entity under the PAYG withholding system.

an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated franking credit for the entity for the year is the amount with ID
    or the estimated deemed dividend for the entity for the year is the amount with ID
    or the estimated exempt income for the entity for the year is the amount with ID
    or the estimated payment under the National Rental Affordability Scheme for the entity for the year is the amount with ID
    or the estimated grant under the energy grants credits scheme for the entity for the year is the amount with ID.

% General capital gains are excluded (the super fund rule above is the exception).
an amount with an ID is a type of estimated excluded income for an entity for a year
    if the estimated capital gain for the entity for the year is the amount with ID
    and it is not the case that
        the entity is a super fund.

scenario test_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO?
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 0. 
    the estimated tax credits for Australian entity for 2025 are 0.
    6250 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    the estimated dividends for Australian entity for 2025 is 40000 with iddAE2025.
    the estimated royalties for Australian entity for 2025 is 35000 with idrAE2025.
    the estimated capital gain for Australian entity for 2025 is 10000 with idcAE2025. % excluded not a superfund
    the estimated GST for Australian entity for 2025 is 20000 with idgAE2025. % just excluded

query test is:
    the varied amount payable for which quarter for which year by Australian entity is which amount.

scenario ato_1_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity. 
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.

scenario ato_2_quarter_3 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 3.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    1000 with idiAE202502 was reported as a variation on quarter 2 of 2025.

scenario ato_3_quarter_4 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 4.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity.
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    1000 with idiAE202502 was reported as a variation on quarter 3 of 2025.
    3000 with idiAE202503 was reported as an instalment on quarter 2 of 2025.

scenario ato_4_quarter_2 is:
    Australian entity is the taxpayer. 
    2025 is a year under consideration.
    the current quarter is quarter 2.
    the estimated taxable income for Australian entity for 2025 is 100000. % Given by ATO
    Australian entity is a base rate entity. 
    the tax offsets for Australian entity for 2025 is 5000. 
    the estimated tax credits for Australian entity for 2025 are 2000.
    3000 with idiAE202501 was reported as an instalment on quarter 1 of 2025.
    the estimated gross rent for Australian entity for 2025 is 100000 with idegAE2025.

query payable is:
    the varied amount payable for which quarter for which year by which entity is which amount.

% Varying by Rate
query new_rate is:
    the new varied rate for which entity for 2025 is which percentage.

query estimated_tax is:
    the estimated tax for which entity for which year is which amount.

query payg_income is:
    the estimated PAYG instalment income for which entity for which year is which amount.

").
% end of en(payg, ...

%13
en(sbpp_0, "This example covers Small Business participation percentage concepts", "the target language is: prolog. 
    
the templates are:
    the indirect small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    the indirect small business participation percentage 1 of *an entity* in *an other entity* is *a percentage*. 
    the indirect small business participation percentage 2 of *an entity* in *an other entity* is *a percentage*.  
    the direct small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    *an entity* holds the legal and equitable interests in shares in *a company*.
    the percentage of the voting power of *an entity* in *a company* is *a percentage*.
    *an entity* jointly holds the legal and equitable interests in the shares of *a company* with *an other entity*.
    the percentage of any dividend *a company* may pay is *a percentage*.
    the percentage of any distribution of capital *a company* may make is *a percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage* and *a third percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage*.
    *an entity*  has entitlements to all the income and capital of *a trust*.
    the percentage of any distribution of income *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    the percentage of any distribution of capital *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    *a trustee* makes distribution of income during the relevant year.
    *a trustee* makes distribution of capital during the relevant year.
    the percentage of *an entity* capital profits for *an other entity* is *a number*. 
    the percentage of *an entity* revenue profits for *an other entity* is *a number*. 

the knowledge base sbpp includes:
 
% http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.75.html 
%   
the indirect small business participation percentage 1 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in an intermediate entity is a number MyD
    and a number L1 is the sum of each Pi such that
    	the direct small business participation percentage of the intermediate entity in the other entity is Pi
    and a number L2 is the sum of each Po such that
    	the indirect small business participation percentage 1 of the intermediate entity in the other entity is Po  
    and I = MyD * (L1 + L2). 

the indirect small business participation percentage 2 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in the other entity is I
    or I = 0. 

the indirect small business participation percentage of an entity in an other entity is a percentage I 
    if the indirect small business participation percentage 1 of the entity in the other entity is a percentage I1
    and the indirect small business participation percentage 2 of the entity in the other entity is a percentage I2
    and I = I1 + I2. 

% 3
the direct small business participation percentage of an entity in a trust T is a percentage I
    if the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
    and T makes distribution of income during the relevant year
    and I = DI.      

the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits for the trustee is X.

                
scenario test is:
%the direct small business participation percentage of individual in private is 0.8.
the percentage of individual revenue profits for private is 0.8.
private makes distribution of income during the relevant year.
%the direct small business participation percentage of private in discretionary is 0.9.
the percentage of private revenue profits for discretionary is 0.9.
discretionary makes distribution of income during the relevant year.
%the direct small business participation percentage of discretionary in unit is 0.6.
the percentage of discretionary revenue profits for unit is 0.6.
unit makes distribution of income during the relevant year. 
%the direct small business participation percentage of individual in unit is 0.1.       

query indirect is:
	the indirect small business participation percentage of individual in unit is which number.
    
query direct is:
	the direct small business participation percentage of individual in unit is which number.    
").
% end of en(sbpp_0, ...

%14
en(sbppxml1, "This example covers Small Business participation percentage concepts in XML format", "the target language is: prolog. 
    
the templates are:
    the indirect small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    the indirect small business participation percentage 1 of *an entity* in *an other entity* is *a percentage*. 
    the indirect small business participation percentage 2 of *an entity* in *an other entity* is *a percentage*.  
    the direct small business participation percentage of *an entity* in *an other entity* is *a percentage*.
    *an entity* holds the legal and equitable interests in shares in *a company*.
    the percentage of *an entity* of the voting power in *a company* is *a percentage*.
    *an entity* jointly holds the legal and equitable interests in the shares of *a company* with *an other entity*.
    the percentage of any dividend *a company* may pay is *a percentage*.
    the percentage of any distribution of capital *a company* may make is *a percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage* and *a third percentage*.
    *a percentage* is the smallest among *a first percentage* and *a second percentage*.
    *an entity*  has entitlements to all the income and capital of *a trust*.
    the percentage of any distribution of income *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    the percentage of any distribution of capital *a trustee* may make to which *an entity* would be beneficially entitled is *a percentage*.
    *a trustee* makes distribution of income during the relevant year.
    *a trustee* makes distribution of capital during the relevant year.
    the percentage of *an entity* capital profits from *an other entity* is *a number*. 
    the percentage of *an entity* revenue profits from *an other entity* is *a number*. 

the knowledge base sbppxml1 includes:
 
% http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.75.html 
%   
the indirect small business participation percentage 1 of an entity in an other entity is a percentage I 
    if the direct small business participation percentage of the entity in an intermediate entity is a number MyD
    and a number L1 is the sum of each Pi such that
    	the direct small business participation percentage of the intermediate entity in the other entity is Pi
    and a number L2 is the sum of each Po such that
    	the indirect small business participation percentage 1 of the intermediate entity in the other entity is Po  
    and I = MyD * (L1 + L2). 

the indirect small business participation percentage of an entity in an other entity is a percentage I 
    if I is the sum of each Po such that
    	the indirect small business participation percentage 1 of the entity in the other entity is Po.

%  http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s152.70.html   


% 3
the direct small business participation percentage of an entity in a trust T is a percentage I
    if it is not the case that
    	the entity has entitlements to all the income and capital of T
    and the percentage of any distribution of income T may make to which the entity would be beneficially entitled is I
            and T makes distribution of income during the relevant year
            and it is not the case that
                T makes distribution of capital during the relevant year
        or  the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is I
            and T makes distribution of capital during the relevant year
            and it is not the case that
                T makes distribution of income during the relevant year
        or  the percentage of any distribution of income T may make to which the entity would be beneficially entitled is a DI
            and the percentage of any distribution of capital T may make to which the entity would be beneficially entitled is a DC
            and I is the smallest among DI and DC.      

a percentage S is the smallest among a percentage P1 and a percentage P2
     if P1 =< P2
         and S = P1
     or P2 < P1
         and S = P2.    

a percentage S is the smallest among a percentage P1 and a percentage P2 and a percentage P3
     if P1 =< P2
         and P1 =< P3
         and S = P1
     or P2 =< P1
         and P2 =< P3
         and S = P2
     or P1 >= P2
        and P2 >= P3
        and S = P3. 

the percentage of any distribution of income a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity revenue profits from the trustee is X. 

the percentage of any distribution of capital a trustee may make to which an entity would be beneficially entitled is a percentage X
    if the percentage of the entity capital profits from the trustee is X.
                
scenario one is: 
% Capital
the percentage of Edwards Family Trust capital profits from Tallow Unit Trust is 0.2.
the percentage of James Family Trust capital profits from Tallow Unit Trust is 0.4.
the percentage of Tom Family Trust capital profits from Tallow Unit Trust is 0.4.
% Income Revenue
the percentage of Edwards Family Trust revenue profits from Tallow Unit Trust is 0.5.
the percentage of James Family Trust revenue profits from Tallow Unit Trust is 0.25.
the percentage of Tom Family Trust revenue profits from Tallow Unit Trust is 0.25.
% Voting
the percentage of Edwards Family Trust of the voting power in Tallow Unit Trust is 0.5.
the percentage of James Family Trust of the voting power in Tallow Unit Trust is 0.25.
the percentage of Tom Family Trust of the voting power in Tallow Unit Trust is 0.25.
% distributions made
Tallow Unit Trust makes distribution of  capital  during the relevant year.
Tallow Unit Trust makes distribution of  income  during the relevant year.
% holds shares in
Edwards Family Trust holds the legal and equitable interests in shares in Tallow Unit Trust.
James Family Trust holds the legal and equitable interests in shares in Tallow Unit Trust.
Tom Family Trust holds the legal and equitable interests in shares in Tallow Unit Trust.
% Capital
the percentage of XYZ Pty Ltd capital profits from Edwards Family Trust is 0.5.
the percentage of BZJ Pty Ltd capital profits from Edwards Family Trust is 0.5.
% Income Revenue
the percentage of XYZ Pty Ltd revenue profits from Edwards Family Trust is 0.4.
the percentage of BZJ Pty Ltd revenue profits from Edwards Family Trust is 1.
% Voting
the percentage of XYZ Pty Ltd of the voting power in Edwards Family Trust is 0.
the percentage of BZJ Pty Ltd of the voting power in Edwards Family Trust is 0.
% distributions made
Edwards Family Trust makes distribution of  capital  during the relevant year.
Edwards Family Trust makes distribution of  income  during the relevant year.
% holds shares in
XYZ Pty Ltd holds the legal and equitable interests in shares in Edwards Family Trust.
BZJ Pty Ltd holds the legal and equitable interests in shares in Edwards Family Trust.
% Capital
the percentage of Tom Fry capital profits from XYZ Pty Ltd is 0.5.
the percentage of Sarah Fry capital profits from XYZ Pty Ltd is 0.5.
% Income Revenue
the percentage of Tom Fry revenue profits from XYZ Pty Ltd is 0.5.
the percentage of Sarah Fry revenue profits from XYZ Pty Ltd is 0.5.
% Voting
the percentage of Tom Fry of the voting power in XYZ Pty Ltd is 0.
the percentage of Sarah Fry of the voting power in XYZ Pty Ltd is 0.
% distributions made
XYZ Pty Ltd makes distribution of  capital  during the relevant year.
XYZ Pty Ltd makes distribution of  income  during the relevant year.
% holds shares in
Tom Fry holds the legal and equitable interests in shares in XYZ Pty Ltd.
Sarah Fry holds the legal and equitable interests in shares in XYZ Pty Ltd.

     
query indirect is:
	the indirect small business participation percentage of Tom Fry in Tallow Unit Trust is which number.
    
query direct is:
	the direct small business participation percentage of Tom Fry in Tallow Unit Trust is which number.    
").
% end of en(sbppxml1, ...

%15
en('small_business', "This example covers Small Business concepts", "the target language is: prolog. 
    
the templates are:

*an entity* is a small business entity for *an income year*. 
*an entity* carries on a business in *a year*. 
*an entity* carried on a business in *a year*.
*an entity* has an aggregated turnover of *an amount* for *a year*.
*an entity* has a probable aggregated turnover of *an amount* for *a year*.
*an entity* is entitled to a concession with *a threshold*.

the knowledge base small business includes:

% based on https://www.austlii.edu.au/cgi-bin/viewdoc/au/legis/cth/consol_act/itaa1997240/s328.110.html
% INCOME TAX ASSESSMENT ACT 1997 - SECT 328.110 Meaning of small business entity 

% You are a small business entity for an income year (the current year ) if:
%
%  (a)   you carry on a * business in the current year; and
%
%  (b)   one or both of the following applies:
%
%  (i)   you carried on a business in the income year (the previous year ) before the current year and your * aggregated turnover for the previous year was less than $10 million;
%
%  (ii)   your aggregated turnover for the current year is likely to be less than $10 million. 
%  
% Note 1:   The $10 million thresholds in this subsection and in subsections   (3) and (4) have been increased to $50 million for certain concessions (for example, see subsection   328 - 285(2)). 

an entity is a small business entity for a current year if
    the entity carries on a business in the current year
    and the entity is entitled to a concession with a threshold
    and     the entity carried on a business in a previous year
            and the entity has an aggregated turnover of an amount for the previous year
            and the amount is less than the threshold
        or  the entity has a probable aggregated turnover of an expected amount for the current year
            and the expected amount is less than the threshold.

scenario one is:
Cherrio Charity carried on a business in 2024.
Cherrio Charity carries on a business in 2025. 
Cherrio Charity is entitled to a concession with 10000000.
Cherrio Charity has a probable aggregated turnover of 9000000 for 2025. 
    
query one is:
Cherrio Charity is a small business entity for which year.  

scenario two is:
Tech Innovations Ltd carried on a business in 2024.
Tech Innovations Ltd carries on a business in 2025.
Tech Innovations Ltd is entitled to a concession with 10000000.
Tech Innovations Ltd has an aggregated turnover of 11000000 for 2024.
Tech Innovations Ltd has a probable aggregated turnover of 9000000 for 2025.

query two is:
Tech Innovations Ltd is a small business entity for which year.

scenario three is:
Bakehouse Bliss Partnership carries on a business in 2025.
Bakehouse Bliss Partnership carried on a business in 2024.
Bakehouse Bliss Partnership is entitled to a concession with 10000000.
Bakehouse Bliss Partnership has a probable aggregated turnover of 9500000 for 2025.

query three is:
Bakehouse Bliss Partnership is a small business entity for which year.

scenario four is:
River View Trust carried on a business in 2024.
River View Trust carries on a business in 2025.
River View Trust is entitled to a concession with 10000000.
River View Trust has an aggregated turnover of 8000000 for 2024.
River View Trust has a probable aggregated turnover of 11000000 for 2025.

query four is:
River View Trust is a small business entity for which year.

scenario five is:
Australian Trust carried on a business in 2024.
Australian Trust carries on a business in 2025.
Australian Trust is entitled to a concession with 10000000.
Australian Trust has an aggregated turnover of 8000000 for 2024.
Australian Trust has a probable aggregated turnover of 9000000 for 2025.

query five is:
Australian Trust is a small business entity for which year.").
% end of en('small_business', ...

%16
en('sum_onto', "This example covers summation over a list of numbers in an ontology", "the target language is: prolog.

the templates are:
*a number* is the result. 

the ontology is:
    3 is a factor.
    2 is a factor. 
    a is a b.
    b is a c.

the knowledge base sum includes:
    
a number S is the result if
    S is the sum of each F such that
         F is a factor. 
    
scenario one is:
    0 is a factor.    
      
query one is:
    which number is the result. 

query two is:
    which thing is of which other thing.").
% end of en('sum_onto', ...

%17
en('sum_simple', "This example covers simple summation over a list of numbers", "the target language is: prolog.

the templates are:
*a number* is the result. 
*a number* is a factor. 

the knowledge base sum includes:
    
a number S is the result if
    S is the sum of each F such that
         F is a factor. 
    
scenario one is:
    3 is a factor.
    2 is a factor.  
      
query one is:
    which number is the result. ").
% end of en('sum_simple', ...


%count
% en('<file_example>', '<description>', "<document_content>"). 
% end of en('<file_example>', ...


