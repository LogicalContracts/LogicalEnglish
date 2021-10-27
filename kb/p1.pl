% Logical English document

:- module('p1+http://tests.com',[]).

en("the target language is: prolog. % other languages available soon

the templates are:

this template has two *a variable one* and *a variable two*.
this template has *a variable one*,  

the knowledge base p1 includes:

this template has 1.
this template has two 1 and 2.  

").

/** <examples>
?- trace, (answer("this template has which value
                   and this template has two which value two and which value three")).
?- answer("this template has which value
                   and this template has two which value two and which value three").
?- answer("this template has which value").
*/
