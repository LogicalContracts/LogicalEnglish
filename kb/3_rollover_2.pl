:- module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover/',[]).

en("the target language is: prolog.
    
the templates are:
 An entity is an ultimate owner of an asset at a time, 
 A trust 's election ocurred,
 A trust is the trust of a group,
 An owner / a share,
 the small business restructure rollover applies to an event,
 an asset is an eligible asset,
 an event occurs at a time,
 an event is a transfer of an asset by a transferor to a transferee,
 an event is part of genuine restructuring according to other legislation,
 an event meets the ultimate ownership test at a time,
 an event is undertaken as part of a trust,
 a trust relates to a family group,
 an individual is a member of a group,
 an individual is an ultimate owner of an asset at a time,
 a trust is a family trust,
 a thing is of type a type,
 a tax payer is in a transferee,
 a time is after a second time,
 a party is a party of an event,
 an amount is the aggregated turnover of a party,
 a party is an eligible party,
 a previous time is immediately before a time,
 an asset is ultimately owned by a share with an owner at a time,
 an entity has a share in an asset at a time,
 an owner owns an asset at a time according to other legislation,
 a party is a small business entity according to other legislation,
 a party has affiliated with an affiliate according to other legislation,
 a party is connected to a taxpayer according to other legislation,
 a party is a partner in partnership with a partnership according to other legislation,
 an asset is an active asset,
 an asset is used in business of a taxpayer according to other legislation,
 an asset is of asset type a type,
 an asset is of asset type a type according to other legislation,
 a party has an aggregated turnover of an amount,
 a party has an aggregated turnover of an amount according to other legislation,
 a cost is a rollover cost,
 an asset costs a cost at a time,
 an owner / a thing is in a set.

the knowledge base Rollover includes:

cgt asset is of type asset.
trading stock is of type asset.
revenue asset is of type asset.
depreciating asset is of type asset.
loan_to_shareholder is of type asset.

the small business restructure rollover applies to an event
     if the event occurs at a time
     and the time is after 20160701
     and the event is a transfer of an asset by a transferor to a transferee
     and the asset is an eligible asset
     and the event is part of genuine restructuring according to other legislation
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
             the entity is an ultimate owner of the asset at the time
             and the entity has a share in the asset at the time
         it is the case that:
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
 and the asset is of asset type a type
 and the type is in [cgt event,depreciating asset,trading stock,revenue asset].

An asset is an active asset
 if  the asset is used in business of a taxpayer according to other legislation.

An asset is of asset type a type
 if  the type is of type asset
 and the asset is of asset type the type according to other legislation.

A party has an aggregated turnover of an amount
 if the party has an aggregated turnover of the amount according to other legislation.

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
 transfer_20200701 occurs at 20200701.
 20200701 is after 20160701.
 transfer_20200701 is a transfer of goodwill by company1 to andrew.
 transfer_20200701 is a transfer of trading stock by company1 to andrew.
 transfer_20200701 is a transfer of plant and equipment by company1 to andrew.
 goodwill is used in business of a taxpayer according to other legislation.
 goodwill is of asset type revenue asset according to other legislation.
 transfer_20200701 is part of genuine restructuring according to other legislation.
 20200630 is immediately before 20200701.
 5300000 is the aggregated turnover of company.
 company is a small business entity according to other legislation.

scenario testing one is:
 20160701 is after 20160630.
 20160630 is immediately before 20160701.

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