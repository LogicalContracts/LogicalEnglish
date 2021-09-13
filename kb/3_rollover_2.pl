:- module('two+https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover/',[]).

en("the target language is: prolog.
    
the templates are:
 *An entity* is an ultimate owner of *an asset* at *a time*,
 the small business restructure rollover applies to *an event*,
 *an event* occurs at *a time*,
 *an event* is a transfer of *an asset* by *a transferor* to *a transferee*,
 *an event* is part of genuine restructuring as informed by *a source*,
 *an event* meets the ultimate ownership test at *a time*,
 *an event* is undertaken as part of *a trust*,
 *a trust* relates to a family group,
 *an individual* is a member of *a group*,
 *a trust* is a family trust,
 *a thing* is of type *a type*,
 *a party* is a party of *an event*,
 *an amount* is the aggregated turnover of *a party*,
 *a party* is an eligible party,
 *an asset* is an eligible asset,
 *an asset* is ultimately owned by *a share* with *an owner* at *a time*,
 *an entity* has a share in *an asset* at *a time*,
 *an owner* owns *an asset* at *a time* as informed by *a source*,
 *a party* is a small business entity as informed by *a source*,
 *a party* has affiliated with *an affiliate* as informed by *a source*,
 *a party* is connected to *a taxpayer* as informed by *a source*,
 *a party* is a partner in partnership with *a partnership* as informed by *a source*,
 *an asset* is an active asset,
 *an asset* is used in business of *a taxpayer* as informed by *a source*,
 *an asset* is of asset type *a type* as informed by *a source*,
 *an asset* is of asset type *a type*,
 *a party* has an aggregated turnover of *an amount* as informed by *a source*,
 *a party* has an aggregated turnover of *an amount*,
 *a cost* is a rollover cost,
 *an asset* costs *an amount* at *a time*,

the knowledge base Rollover includes:

% these facts are not being used:
cgt asset is of type asset.
trading stock is of type asset.
revenue asset is of type asset.
depreciating asset is of type asset.
loan_to_shareholder is of type asset.

the small business restructure rollover applies to an event
     if the event occurs at a time
     and the time is after 2016-07-01
     and the event is a transfer of an asset by a transferor to a transferee
     and the asset is an eligible asset
     and the event is part of genuine restructuring as informed by a source
     and the event meets the ultimate ownership test at the time
     and for all cases in which
             a party is a party of the event
             and an amount is the aggregated turnover of the party
         it is the case that:
             the party is an eligible party
             and the amount < 10000000.

an event meets the ultimate ownership test at a time
     if the event is a transfer of an asset by a transferor to a transferee
     and a previous time is immediately before the time
     and for all cases in which
             an entity is an ultimate owner of the asset at the time
             and the entity has a share in the asset at the time
         it is the case that:
             the entity is an ultimate owner of the asset at the previous time
             and the entity has a share in the asset at the previous time.

an event meets the ultimate ownership test at a time
     if the event is a transfer of an asset by a transferor to a transferee
     and the event is undertaken as part of a trust
     and the trust is a family trust
     and the trust relates to a family group
     and a previous time is immediately before the time
     and for all cases in which
             an individual is an ultimate owner of the asset at the time
         it is the case that:
             the individual is a member of the family group
     and for all cases in which
             an individual is an ultimate owner of the asset at the previous time
         it is the case that:
             the individual is a member of the family group.

A tax payer is a party of an event
if  the event is a transfer of an asset by the tax payer to a transferee.

A tax payer is a party of an event
if  the event is a transfer of an asset by a transferor to the tax payer.

An asset is ultimately owned by 1 with an owner at a time
 if  the owner owns the asset at the time as informed by a source.

A party is an eligible party
 if the party is a small business entity as informed by a source.

A party is an eligible party
 if the party has affiliated with an affiliate as informed by a source
 and the affiliate is a small business entity as informed by a source.

A party is an eligible party
 if the party is connected to a taxpayer as informed by a source
 and the taxpayer is a small business entity as informed by a source.

A party is an eligible party
 if the party is a partner in partnership with a partnership as informed by a source
 and the partnership is a small business entity as informed by a source.

An asset is an eligible asset
 if  the asset is an active asset
 and the asset is of asset type a type
 and the type is in [cgt event,depreciating asset,trading stock,revenue asset].

An asset is an active asset
 if  the asset is used in business of a taxpayer as informed by a source.

An asset is of asset type a type
 if the asset is of asset type the type as informed by a source.

A party has an aggregated turnover of an amount
 if the party has an aggregated turnover of the amount as informed by a source.

A cost is a rollover cost
 if  the event occurs at a time
 and the event is a transfer of an asset by a transferor to a transferee
 and a previous time is immediately before the time
 and the asset costs the cost at the previous time.

% Company transfers its assets to partners in a partnership on 1 July 2020.
% Company has turnover of 5,300,000. Company is a Small Business Entity i.e.
% the transferor & the partners in a partnership i.e. transferee is also a small business entity.)
% Assets are -
% Goodwill
% Trading stock
% Plant & equipment
scenario Andrew email Feb 4 2021 is:
 transfer_20200701 occurs at 2020-07-01.
 2020-07-01 is after 2016-07-01.
 transfer_20200701 is a transfer of goodwill by company1 to andrew.
 transfer_20200701 is a transfer of trading stock by company1 to andrew.
 transfer_20200701 is a transfer of plant and equipment by company1 to andrew.
 goodwill is used in business of a taxpayer as informed by a source.
 plant and equipment is used in business of a taxpayer as informed by a source. 
 goodwill is of asset type revenue asset as informed by a source.
 plant and equipment is of asset type revenue asset as informed by a source.
 transfer_20200701 is part of genuine restructuring as informed by a source.
 2020-06-30 is immediately before 2020-07-01.
 5300000 is the aggregated turnover of company.
 company is a small business entity as informed by a source.
 company is an ultimate owner of goodwill at 2020-07-01.
 company has share_goodwill in goodwill at 2020-07-01.
 company is an ultimate owner of goodwill at 2020-06-30.
 company has share_goodwill in goodwill at 2020-06-30.

scenario testing one is:
 2016-07-01 is after 2016-06-30.
 2016-06-30 is immediately before 2016-07-01.

query one is:
for which event:
 the small business restructure rollover applies to the event.

query two is:
 which tax payer is a party of which event.

query three is:
 A first time is after a second time
 and the second time is immediately before the first time.
"). 

/** <examples>
?- answer("one with Andrew email Feb 4 2021"). 
*/