:- module('Euclid',[]).

en("the target language is: prolog. % other languages available soon
    
the templates are:
    *a number* is the gcd of *a number* and *a number*.
    
the knowledge base Euclid includes:
    
a number N is the gcd of  N and N.

a number D is the gcd of a number N and a number M
    if N > M
    and a number smallerN = N-M
    and D is the gcd of smallerN and M.
    
a number D is the gcd of a number N and a number M
    if M > N
    and a number smallerM = M-N
    and D is the gcd of N and smallerM.

query one is:
	2 is the gcd of 1946 and 2023.
    
query two is:
	7 is the gcd of 1946 and 2023.
    
query three is:
	which number is the gcd of 1946 and 2023.
    
query four is:
	which number is the gcd of 1946 and which other number.
    
query five is: 
    which number is the gcd of 15 and 18.
").


/** <examples>
?- answer one.
?- answer two.
?- answer three.
?- answer four.
?- answer five. 
?- show prolog.
?- answer(five, with(noscenario), le(Explanation), R). 
*/
