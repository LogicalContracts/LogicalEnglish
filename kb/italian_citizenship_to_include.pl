:- module('italian_citizenship_to_include',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
*a person* is an italian citizen,
*a person* is the father of *a person*,
*a person* is the mother of *a person*,
*a person* is born in italy,
*a person* is stateless,
*a person* follows the citizenship of *a person*.
    
the knowledge base italian_citizenship_to_include includes:

% Article 1.1(a)
a person A is an italian citizen
if a person B is the father of the person A
and the person B is an italian citizen.
    
a person A is an italian citizen
if a person B is the mother of the person A
and the person B is an italian citizen.

% Article 1.1(b)
a person A is an italian citizen
if the person A is born in italy
and unknown is the father of the person A
and unknown is the mother of the person A.
    
a person A is an italian citizen
if the person A is born in italy
and a person F is the father of the person A
and the person F is stateless
and the person M is the mother of the person A
and the person M is stateless.

a person A is an italian citizen
if the person A is born in italy
and a person F is the father of the person A
and a person M is the mother of the person A
and it is not the case that
	the person A follows the citizenship of the person F
    and the person A follows the citizenship of the person M.
    
% add as many as you need    
scenario giuseppe is:
felice is the father of giuseppe.
tatiana is the mother of giuseppe.
felice is an italian citizen.
tatiana is an italian citizen.
    
scenario luca is:
luca is born in italy.
unknown is the father of luca.
unknown is the mother of luca.

% add as many as you need    
query one is:
which person is an italian citizen.

").

/** <examples>
?- answer("query one with scenario giuseppe").
?- answer(one, with(giuseppe), le(E), R).
*/
