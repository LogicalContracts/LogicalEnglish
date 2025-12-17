% A cured database of Document Examples for Logical English

:- module(le_en, [en/3]).

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


% en('<file_example>', '<description>', "<document_content>"). 



