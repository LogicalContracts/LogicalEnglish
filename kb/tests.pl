:-module('https://tests.com',[]).



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