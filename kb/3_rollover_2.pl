:- module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover',[]).

mainGoal(event_of_transfering_from_to_at_occurs(_ID,_Asset,_Transferor,_TransfereesList,_When), "Determine if a asset transfer event can be treated as a restructure rollover").

en("the templates are:
    A trust 's election ocurred,
    A trust is the trust of a group,
    An Event ID rollover applies,
    An owner / a share,
    An Event ID rollover of the transfer of an Asset from a Transferor to a Transferees List at a Time applies,
    event an event of transfering an asset from a payer to a recipient at a time occurs,
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

the knowledge base includes:
An Event ID rollover of the transfer of an Asset from a Transferor to a Transferees List at a Time applies
    if  this information transfer_event(the Event ID,the Asset,the Time,the Transferor,the Transferees List) has been recorded
    and the Event ID rollover applies.

cgt_asset is of type asset.
trading_stock is of type asset.
revenue_asset is of type asset.
depreciating_asset is of type asset.
loan_to_shareholder is of type asset.

A tax payer is a party of an event
   if  event the event of transfering an asset from the tax payer to a recipient at a time occurs.

A tax payer is a party of an event
    if  event the event of transfering an asset from a somebody to a transferee at a time occurs
    and the tax payer is in the transferee.

An event rollover applies
    if event the event of transfering an asset from a sender to a recipient at a time occurs
    and the time is after 20160701
    and for all cases in which
           a party is a party of the event
        it is the case that:
            the party has an aggregated turnover of a turnover
            and the turnover < 10000000
            and the party is an eligible party
    and the event is part of genuine restructuring according to other legislation
    and a previous time is immediately before the time
    and a set previous owners is a collection of an owner / a share
        where the asset is ultimately owned by the share with the owner at the previous time
    and a set new owners is a collection of the owner / the share
        where the asset is ultimately owned by the share with the owner at the time
    and the set new owners = the set previous owners
        or a family trust is the trust of a group
            and the family trust 's election ocurred
            and for all cases in which
                    the owner / le_some_thing is in the set previous owners
                it is the case that:
                    the owner is in the group
            and for all cases in which
                    the owner / le_some_thing is in the set new owners
                it is the case that:
                   the owner is in the group
    and the asset is an eligible asset.

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
    and the type is in [cgt_event,depreciating_asset,trading_stock,revenue_asset].

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

scenario testing one is:
    event 123 of transfering company1 goodwill from company1 to miguel at 20160701 occurs,
    20160701 is after 20160630,
    20160630 is immediately before 20160701,
    miguel has an aggregated turnover of 400000 according to other legislation,
	company1 has an aggregated turnover of 5300000 according to other legislation.

scenario testing two is:
    event 123 of transfering company1 goodwill from company1 to miguel at 20160701 occurs.    
    
scenario testing three is:
    20160701 is after 20160630,
    20160630 is immediately before 20160701.
    
query q one is:
  for which event:
      the event applies.
    
query q two is:
    which tax payer is a party of which event.
    
query q three is:
    A first time is after a second time
    and the second time is immediately before the first time. 
    
    
"). 

/** <examples>
?- answer("q one with testing one").
?- answer("q two with testing two", Goal, Result). 
*/