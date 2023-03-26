:- module('safe_gap',[]).

en("the target language is: prolog. % other languages available soon

the templates are:
    there is a safe gap in *a junction* at *a time*.
    the gap in *a junction* is of *a value* at *a time*.
    the minimal safe gap in *a junction* is of *a value* at *a time*.
    *a number* is greater than *an other number*.
    *a number* is the static safe gap at *a junction*.
    *a number* is the dynamic safe gap correction at *a time*.
    *a number* is the time of day index at *a time*. 
    *a number* is the weather index at *a time*. 
    *a number* is the visibility index at *a time*. 
    *a number* is the road index at *a time*.     

the knowledge base safe_gap includes:
there is a safe gap in a junction at a time if
    the gap in the junction is of a value at the time
    and the minimal safe gap in the junction is of a limit at the time
    and the value > the limit.

the minimal safe gap in a junction is of a value at a time if 
    a first number is the static safe gap at the junction
    and a second number is the dynamic safe gap correction at the time
    and the value is the first number - the second number. 

a number G is the dynamic safe gap correction at a time
    if a number T is the time of day index at the time
    and a number W is the weather index at the time
    and a number V is the visibility index at the time
    and a number R is the road index at the time
    and G is T + W + V + R.

% daylight +1 | warm and dry +1 | clear +1 | dry +1 | 4 |
scenario happyday is:
    the gap in the junction is of 4 at happyday. 
    5 is the static safe gap at the junction. 
    1 is the time of day index at happyday.
    1 is the weather index at happyday.
    1 is the visibility index at happyday. 
    1 is the road index at happyday. 

% night time 0 | cold and wet 0 | very poor 0 | icy 0 | 0 |
scenario rainynight is:
    the gap in the junction is of 4 at rainynight. 
    5 is the static safe gap at the junction. 
    0 is the time of day index at rainynight.
    0 is the weather index at rainynight.
    0 is the visibility index at rainynight. 
    0 is the road index at rainynight. 

% add as many as you need    
query one is:
    there is a safe gap in which junction at which time. 
    
query two is:
    the gap in the junction is of which value at which time. 

").

/** <examples>
?- answer("query one with scenario happyday").
?- answer("query one with scenario rainynight").
?- answer(one, with(rainynight), le(E), R).
?- answer(one, with(happyday), le(E), R).
*/
