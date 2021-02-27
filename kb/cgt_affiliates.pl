% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020,2021
% PRELIMINARY DRAFT

% Web page from which the present knowledge page was encoded
:-module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/',[]).


% indicate future API entry point: predicate pattern, description
mainGoal(affiliate(_Entity,_Affiliate),"Determine if a given entity is affiliate of another (also given)").

example(test,[scenario([
    acts_in_accordance_with_directions_from(company,andrew), 
    is_trust(_) if false, 
    is_partnership(_) if false, 
    is_superannuation_fund(_) if false, 
    is_individual_or_company(_)
    ],true)]).
% Assumptions: 
%   datetimes in iso_8601 format

% note: referred by cgt_concessions_basic_conditions_sb.pl:
affiliate(Entity,Affiliate) on Date if 
    Date not_before '20090101' % date (?) when affiliate definition changed
    and is_individual_or_company(Affiliate) 
	and not is_trust(Affiliate) and not is_partnership(Affiliate) and not is_superannuation_fund(Affiliate)
    and (acts_in_accordance_with_directions_from(Affiliate,Entity) or acts_in_concert_with(Affiliate,Entity)). % seems human-bound!
affiliate(Entity,Affiliate) on Date if
    Date before '20090101' % date when affiliate definition changed
    and must_be(nonvar,Entity) and must_be(nonvar,Affiliate)
    and affiliate_per_older_legislation(Affiliate,Entity).

question( affiliate_per_older_legislation(Affiliate,Entity), "Is '~w' an affiliate of '~w' as per the older legislation" - [Affiliate,Entity]).

% stub for an "external" Prolog call
is_individual_or_company(Affiliate) on Date because 'according to myDB_entities' :- 
    myDB_entities:is_individual_or_company_on(Affiliate,Date).

is_trust(Entity) if 
	is_trust(Entity) at entitiesDB.
is_partnership(Entity) if
	is_partnership(Entity) at entitiesDB.
is_superannuation_fund(Entity) if
	is_superannuation_fund(Entity) at entitiesDB.

/** <examples> 
?- query_with_facts(affiliate(andrew,company) on '20200101',test,Unknowns,Explanation,Result).
?- query_with_facts(affiliate(andrew,company) on '20200101',[acts_in_accordance_with_directions_from(company,andrew)],Unknowns,Explanation,Result).
*/