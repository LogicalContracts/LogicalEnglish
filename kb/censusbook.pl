:- module(censusbook,[]).

en("the target language is: prolog. 
    
the templates are:
    *a subject* owns *a number* of the sheeps. 
    *a number* of sheeps do all my subjects together own.
    
the knowledge base censusbook includes:
    
a number N of sheeps do all my subjects together own 
    if  N is the sum of each other number such that
    	a subject owns the other number of the sheeps. 
    
scenario today is:
    John Smith owns 1 of the sheeps.
    Mary Jones owns 2 of the sheeps.
    Elon Musk  owns 5626 of the sheeps. 
    
scenario tomorrow is:
    John Smith owns 1 of the sheeps.
    Mary Jones owns 3 of the sheeps.
    Elon Musk  owns 5626 of the sheeps.

query total is:
	which number of sheeps do all my subjects together own.
    
query percapita is:
	which person owns which number of the sheeps.    

").

/** <examples>
?- answer("total with scenario today").
?- answer("total with scenario tomorrow").
?- answer("percapita with scenario today").
?- answer("percapita with scenario tomorrow").
?- show prolog.
*/