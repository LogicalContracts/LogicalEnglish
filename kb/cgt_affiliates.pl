% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/',[]).


% indicate future API entry point: predicate pattern, description
mainGoal(affiliate(_Entity,_Affiliate),"Determine if a given entity is affiliate of another (also given)").

% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- discontiguous if/2.

% note: referred by cgt_concessions_basic_conditions_sb.pl:
affiliate(Entity,Affiliate) on Date if 
    Date @>= '20090101' % date (?) when affiliate definition changed
    and is_individual_or_company(Affiliate) 
    and (acts_in_accordance_with_directions_from(Affiliate,Entity) or acts_in_concert_with(Affiliate,Entity)). % seems human-bound!
affiliate(Entity,Affiliate) on Date if
    Date @< '20090101' % date when affiliate definition changed
    and must_be(nonvar,Entity) and must_be(nonvar,Affiliate)
    and question( "Is ~w an affiliate of ~w as per the older legislation" - [Affiliate,Entity]).


is_individual_or_company(Affiliate) if 
    is_individual_or_company(Affiliate) at myDB_entities.