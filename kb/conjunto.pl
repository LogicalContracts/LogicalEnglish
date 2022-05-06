:- module('conjunto',[]).

es("el lenguaje objetivo es: prolog.

los predicados son:
	*un conjunto* es un subconjunto de *un conjunto*,
	*una cosa* es un conjunto,
	*una cosa* pertenece a *un conjunto*.

la base de conocimiento conjunto incluye:

un conjunto A es un subconjunto de un conjunto B
    si el conjunto A es un conjunto
    y el conjunto B es un conjunto
    y en todos los casos en los que
    	una cosa pertenece a el conjunto A
        es el caso que
    	la cosa pertenece a el conjunto B. 

escenario uno es:
	familia uno es un conjunto.
	familia dos es un conjunto.
	Roberto pertenece a la familia uno.
	Alicia pertenece a la familia uno.
   
	Alicia pertenece a la familia dos.
      
la pregunta uno es:
 	cuál primera familia es un subconjunto de cuál segunda familia.
    
escenario dos es:
	[Alicia, Roberto] es un conjunto.
	[Alicia] es un conjunto.

	una cosa pertenece a un conjunto
    if la cosa is in el conjunto.
    
la pregunta dos es:
 	cuál conjunto es un subconjunto de cuál segundo conjunto.
 
").

/** <examples>
?- show prolog.
?- responde dos con dos.
*/