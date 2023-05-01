:- module('simple RPS+http://tests.com',[]).

en("the target language is: prolog. %

the templates are:
   	*a choice* beats *a choice*,
    *a person* inputs *a choice* and *an amount*,
    *a person* gets *an amount*,
	the game is a draw.
	    
the knowledge base simple RPS includes:
	scissors beats paper.
	paper beats rock.
	rock beats scissors.
    
a first player gets a prize
    if the first player inputs a first choice and an amount X
    and a second player inputs a second choice and an amount Y
    and the first player is different from the second player
    and the first choice beats the second choice
    and the prize is X+Y.
    
the game is a draw 
    if a first player inputs a first choice and an amount X
    and a second player inputs the first choice and an amount Y
    and the first player is different from the second player.
    
a player gets an amount
    if the game is a draw
    and the player inputs a choice and the amount.

scenario mbj is:
    miguel inputs paper and 100.
    bob inputs paper and 1000.
% 	jacinto inputs paper and 1000.
    
query gets is:
	which person gets which amount.
        
query draw is:
    the game is a draw.
    
query different is:
    jacinto is different from bob.
    
").


/** <examples>
?- answer gets with mbj.
?-  answer(gets, with(mbj), le(Explanations), R).
*/
