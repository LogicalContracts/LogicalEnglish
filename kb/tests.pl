:-module('http://tests.com',[]).

example("hypo tests",[
    scenario([xpto], testForall([0,1,2])),
    %scenario([], testForall([0,1,2])),
    %scenario([d(3) on time1], testForall([0,1,2,3])),
    %scenario([-d(3)], not testForall([0,1,2,3,4])),
    scenario([owns(_,_) at myDB1 if true], true)
]).

ultimate_owner(Asset,Owner,1) on _T if % full ownership, partial definition 
    owns(Owner,Asset)
        at "https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/".

failingIFTE if 
	if (d(13) or t(_)) then false else true.

a(X,Y) if 
    if c(X) then t(Y) else e(Y).

c(1).
c(2).
c(X) if d(X).

d(0).

t(a).

e(b).

foo if not bar.

testForall(L) if xpto and
    forall(member(X,L), c(X)).

testFailedAggregate(X,Total) if aggregate(sum(X),c(X),Total).

testFailedAggregateAll(X,Total) if aggregate_all(sum(X),c(X),Total).

testFindall(X,L) if findall(X,c(X),L).


/** <examples>
?- query_with_facts(testForall([0,1,2,3]) at 'http://tests.com',[d(3)],Unknowns,Explanation,Result).
*/