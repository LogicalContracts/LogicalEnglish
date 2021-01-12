:-module('https://tests.com',[]).

example("meta tests",[
    scenario([], testForall([0,1,2])),
    scenario([d(3)], testForall([0,1,2,3])),
    scenario([], not testForall([0,1,2,3,4]))
]).

a(X,Y) if 
    if c(X) then t(Y) else e(Y).

c(1).
c(2).
c(X) if d(X).

d(0).

t(a).

e(b).

testForall(L) :-
    forall(member(X,L),c(X)).