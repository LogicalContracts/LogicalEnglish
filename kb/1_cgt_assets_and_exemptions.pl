:-module('1_cgt_assets_and_exemptions+https://www.ato.gov.au/Individuals/Capital-gains-tax/List-of-cgt-assets-and-exemptions/',[]).

en("the target language is: prolog. 
    
the templates are:

  % *a thing* is a *a type*. 
  % *a thing* is an *a type*.
  *an asset* is a CGT asset.
  *an asset* is a CGT exempt asset.
  *an asset* is a pre-date CGT asset.
  *a thing* is a property.
  *a thing* is a legal or equitable right that is not a property.
  *a thing* is a real state asset.
  *a thing* is a main residence.
  *a thing* is a granny flat.
  *a thing* is a vacant land.
  *a thing* is a business premise.
  *a thing* is a holiday house.
  *a thing* is a hobby farm.
  *a thing* is a farm. 
  *an arrangement* is a granny flat arrangement. 
  *an arrangement* for *an asset* is *an event*. 
  *a thing* is a share or a unit or similar investments.
  *a thing* is a real state asset.
  *a thing* is a personal use asset.
  *a thing* is a collectable asset.
  *a thing* is a intangible asset.
  *a thing* is a foreign currency asset.
  *a thing* is a depreciating asset.
  *a thing* is an award or payout asset. 
  *a thing* is a share.
  *a thing* is a unit.
  *a thing* is a convertible note.
  *a thing* is a right to acquire shares or units.
  *a thing* is an option to acquire shares or units.
  *a thing* is a stapled security.
  *a thing* is a cryptocurrency. 
  *a thing* is used for the purchase of items for personal consumption.
  *a thing* is a boat.
  *a thing* is a furniture.
  *a thing* is an electrical good.
  *a thing* is a household item.
  *a thing* is an option or right to acquire a personal use asset.
  *a thing* is an IOU as a loan to a family member.
  *a thing* is an IOU as a loan to a family friend.
  *a thing* is an IOU related to a personal use asset.
  *an asset* costed *an amount* to acquire.
  *a thing* is a lease.
  *a thing* is a goodwill.
  *a thing* is a license.
  *a thing* is a contractual right.
  *a thing* is a business equipment.
  *a thing* is rental property plant or equipment.
  *a thing* is a removable asset in a rental property.
  *a thing* meets the definition of plant and equipment under Division 43 of the Income Tax Act.
  *an asset* is used soley for taxable income generating purposes. 
  *an asset* is a motor vehicle. 
  *an asset* is a motor cycle.
  *an asset* is used purely for personal use purposes. 
  *an asset* is an award. 
  *an award* was secured for valour or brave conduct.
  *an asset* was bought.
  *an asset* was secured in exchange for *an other asset*.
  *an asset* is a gambling winning.
  *an asset* has been secured through a game or a competition. 
  *an asset* corresponds to compensation or damages.
  *an asset* was received for an injury at *a place* of *a taxpayer*. 
  *an asset* was received for an injury at *a place* of *a taxpayer*'s relative.
  *an asset* was received under an Unlawful Termination Assistance Scheme.
  *an asset* was received under an alternative Dispute Resolution Assistance Scheme.
  *an asset* was received under an M4/M5 Cashback Scheme. 
  *an asset* was received under a scheme established under legislation by an Australian Government agency or a foreign government agency.
  *an asset* is a receipt for compensation or damages. 
  *an asset* was received under a scheme established under legislation by a local government body.
  *an asset* was received under a scheme established under legislation by a foreign government agency. 
  *an asset* is an interest in *a fund*. 
  *a fund* has *a number* of members.
  *an asset* is being transferred because of a relationship breakdown. 
  *an asset* is a right.
  *an asset* was created in *an agreement*. 
  *an asset* was ended in *an agreement*. 
  *an asset* is a payout.
  *an asset* arose from the maturity of *a policy*. 
  *a policy* is a general insurance policy for *an asset*. 
  *a policy* is a life insurance policy for *an asset*.
  *an instrument* is an annuity instrument for *an asset*. 
  *an asset* of *a taxpayer* is a CGT exempt asset.
  *an asset* is a payment.
  *an asset* is for surrender of *a policy*. 
  *a taxpayer* is the original beneficial owner of *a policy*. 
  *an asset* is held in a pooled development fund. 
  *an asset* is held in certain venture capital entity.
  *an asset* is held in an early stage venture capital limited partnership. 
  *an asset* is a gift. 
  *an asset* was gifted through a will to a deductible gift recipient beneficiary.
  *an asset* belongs to *a person*. 
  *a person* was a resident of Norfolk Island at *a date*. 
  *an asset* was acquired at *a date*.
  *an agreement* is a superannuation agreement.
  *a thing* is an artwork. 
  *a thing* is jewellery. 
  *a thing* is an antique. 
  *a thing* is a rare coin. 
  *a thing* is a rare medallion. 
  *a thing* is a rare stamp. 
  *a thing* is a rare folio. 
  *a thing* is a manuscript. 
  *a thing* is a book. 

the knowledge base cgt_assets_and_exemptions includes:

% A CGT asset is:
% (a) any kind of property; or
% (b) a legal or equitable right that is not property.

a thing is a CGT asset
  if the thing is a property
  or the thing is a legal or equitable right that is not a property. 

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
  or the thing is a intangible asset
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
  or the thing is jewellery
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

% A foreign currency transaction is a CGT event if 'entity' makes a gain on disposal of that foreign currency due to an exchange rate fluctuation between time of acquisition and time of disposal.

% A foreign currency transaction is a CGT event if 'entity' makes a loss on disposal of that foreign currency due to an exchange rate fluctuation between time of acquisition and time of disposal.

% Depreciating assets list
% business equipment
% plant and equipment found in a rental property
a thing is a depreciating asset
  if the thing is a business equipment
  or the thing is rental property plant or equipment.

% Plant and equipment is rental property plant and equipment if it is a removable asset in a rental property
%   and it meets the definition of plant and equipment under Division 43 of the Income Tax Act.
a thing is is rental property plant or equipment
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
% Compensation or damages is a CGT exempt asset if received under a scheme established under legislation by an Australian Government agency,  or a foreign government agency.
an asset is a CGT exempt asset
  if the asset corresponds to compensation or damages
  and the asset was received for an injury at a place of a taxpayer
      or the asset was received for an injury at place of the taxpayer's relative
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
% not right, according to these:
% CGT does not apply to an asset if both the following are true:

% you were a resident of Norfolk Island before 24 October 2015
% you acquired the asset on Norfolk Island before 24 October 2015.
% only one enough?

% An asset is a CGT exempt asset if the asset was acquired on or before 19 September 1985.
an asset is a CGT exempt asset
  if the asset was acquired at a date
  and the date is before 1985-09-19
      or the date is equal to 1985-09-19. 

% We will have the following data in our database - 
% Entity type i.e. individual, company, superannuation fund or trust with respect to the entity buying or selling the asset. 
% We will have asset types i.e as per the list that I shared with Jacinto before.
% We will have the breakdown of the cost base of each asset. 1st to 5th element.

% Snapshot – Fundamental questions
% The application of the CGT regime requires consideration of each of the following fundamental
% elements.
% 1. Has a CGT event (¶3.6) happened to a CGT asset (¶3.2)?
% 2. Has a capital gain or capital loss been made – considering:
% •
% the cost base/reduced cost base (¶3.3) of the CGT asset, and any modifications to
% the cost base/reduced cost base (¶3.4)
% •
% the capital proceeds received from the CGT event, and any special rules and/or
% modifications that apply (¶3.5)
% •
% if the CGT discount applies for a CGT asset that has been held for more than 12
% months (¶3.7)?
% 3. Whether an exception or concession applies, such as capital gains or losses from certain pre-CGT
% assets that are disregarded (and note also the small business concessions in Module 4)?
% 4. Whether a roll-over (¶3.9) or similar relief relating to demerger (¶3.10) and/or death (¶3.11)
% applies?
% 5. What is the amount of net capital gain or net capital loss to be included in the taxpayer’s
% assessable income, noting
% • any current year or carried-forward capital losses to offset against capital gains, and
% • the method statements in s 102-5 ITAA97 (net capital gain) and s 102-10 ITAA97 (net
% capital loss) (see below).  

% Check if your assets are subject to CGT, exempt, or pre-date CGT.
query one is:
  which thing is a CGT asset.

query two is:
  which asset is a CGT exempt asset. 

query three is:
  which asset is a pre-date CGT asset. 

"). 

/** <examples>
 * 
?- answer('Colin', with('Colin'), le(R), An). 
?- show prolog.
?- answer(one, with('Colin')).
*/  