:- module('sousensemble+http://tests.com',[]).

fr("la langue cible est : prolog.
    
les modèles sont:
    *un ensemble* est un sous-ensemble d'*un ensemble*,
    *une chose* est un ensemble,
    *une chose* appartient à *un ensemble*.

la base de connaissances dont le nom est sousensemble comprend:

un ensemble A est un sous-ensemble d'un ensemble B
    si l'ensemble A est un ensemble
    et l'ensemble B est un ensemble
    et pour tous les cas où
        une chose appartient à l'ensemble A
    	c'est le cas que 
    	la chose appartient à l'ensemble B. 

le scénario un est:
    la famille un est un ensemble.
    la famille deux est un ensemble.
    Bob appartient à la famille un.
    Alice appartient à la famille un.
    
    Alice appartient à la famille deux.
      
la question un est:
    quelle premier famille est un sous-ensemble d' quelle seconde famille.
    
le scénario deux est:
    [Alice, Bob] est un ensemble.
    [Alice] est un ensemble.
    
    une chose appartient à un ensemble 
        si la chose is in l'ensemble.
    
la question deux est:
    quelle ensemble est un sous-ensemble d' quelle autre ensemble.
   
").

/** <examples>
?- show prolog.
?- répondre un avec un.
*/