:- module('3_rollover_3',[]).

en("the target language is: prolog. 
    
    the templates are:
    *A trust* s election ocurred,
    *A trust* is the trust of a group,
    *An event* rollover applies,
    *An owner* / *a share*,
    *An Event ID* rollover of the transfer of *an Asset* from *a Transferor* to *a Transferees List* at *a time* applies,
    *an event* of transfering *an asset* from *a payer* to *a recipient* at *a time* occurs,
    the small business restructure rollover applies to *an event*,
    *an event* occurs at *a time*,
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

/** <examples>.
?- show prolog. 
?- answer("one with scenario Andrew email Feb 4 2021"). 
?- answer("one with scenario Andrew email Feb 4 2021 version 2").
?- answer("one with scenario Testing").
?- answer(one,with('Testing'), le(E), R).
*/
