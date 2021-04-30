% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020,2021
% PRELIMINARY DRAFT

% Web page from which the present knowledge page was encoded
:-module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/',[]).


% indicate future API entry point: predicate pattern, description
mainGoal(has_affiliated_with(_Entity,_Affiliate),"Determine if a given entity is affiliate of another (also given)").

must_not_be_a_variable(Term) :-
    must_be(nonvar, Term).

is_before(T1, T2) :- T1 before T2. 

example(test,[scenario([
    acts_in_accordance_with_directions_from(company,andrew), 
    is_a_trust(_) if false, 
    is_a_partnership(_) if false, 
    is_a_superannuation_fund(_) if false, 
    is_an_individual_or_a_company(_)
    ],true)]).
% Assumptions: 
%   datetimes in iso_8601 format

% note: referred by cgt_concessions_basic_conditions_sb.pl:
has_affiliated_with(Entity,Affiliate) on Date if 
    Date is_not_before '20090101' % date (?) when affiliate definition changed
    and is_an_individual_or_a_company(Affiliate) 
	and not is_a_trust(Affiliate) and not is_a_partnership(Affiliate) and not is_a_superannuation_fund(Affiliate)
    and (acts_in_accordance_with_directions_from(Affiliate,Entity) or acts_in_concert_with(Affiliate,Entity)). % seems human-bound!
has_affiliated_with(Entity,Affiliate) on Date if
    is_before(Date, '20090101') % date when affiliate definition changed
    and must_not_be_a_variable(Entity) and must_not_be_a_variable(Affiliate)
    and are_affiliated_per_older_legislation(Affiliate,Entity).

question( are_affiliated_per_older_legislation(Affiliate,Entity), "Is '~w' an affiliate of '~w' as per the older legislation" - [Affiliate,Entity]).

% stub for an "external" Prolog call
is_an_individual_or_a_company(Affiliate) on Date because 'according to myDB_entities' :- 
    myDB_entities:is_individual_or_company_on(Affiliate,Date).

is_a_trust(Entity) if 
	is_a_trust(Entity) at entitiesDB.
is_a_partnership(Entity) if
	is_a_partnership(Entity) at entitiesDB.
is_a_superannuation_fund(Entity) if
	is_a_superannuation_fund(Entity) at entitiesDB.

/** <examples> 
?- query_with_facts(has_affiliated_with(andrew,company) on '20200101',test,Unknowns,Explanation,Result).
?- query_with_facts(has_affiliated_with(andrew,company) on '20200101',[acts_in_accordance_with_directions_from(company,andrew)],Unknowns,Explanation,Result).
?- le(LogicalEnglish).
*/