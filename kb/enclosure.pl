:- module(enclosure,[]).

en("the target language is: prolog. 
    
the templates are:
	*a group* is comfortable in *an enclosure*.
	high energy efficiency is achieved within *an enclosure*. 
	the radiation inside *an enclosure* is correct.
	the temperature in *an enclosure* is correct.
	humidity within *an enclosure* is correct. 
	the thermal load through *an enclosure* is minimal.
	the thermal load through *an enclosure* may be *a value*.
	*an amount* is less than *an other amount*. 
	the air conditioning is *a state*. 
	the wind blows.
	the windows of *an enclosure* are *a state*.  
	
the ontology is:
	low is less than medium.
	low is less than high. 
	medium is less than high.

the knowledge base enclosure includes:

	a group is comfortable in an enclosure if
		the radiation inside the enclosure is correct
		and the temperature in the enclosure is correct
		and humidity within the enclosure is correct. 
		
	high energy efficiency is achieved within an enclosure if
		the thermal load through the enclosure is minimal. 

	the thermal load through an enclosure is minimal if
		the thermal load through the enclosure may be a value V
		and it is not the case that
			the thermal load through the enclosure may be a value W
			and W is less than V. 
			
	the thermal load through an enclosure may be high if
		the air conditioning is on. 
		
	the thermal load through an enclosure may be medium if
		the wind blows
		and the windows of the enclosure are open.
		
	the thermal load through an enclosure may be low if
		the air conditioning is off
		and the wind blows
		and the windows of the enclosure are open. 
			
scenario test is:
    the wind blows.
    the windows of my house are open. 
    the air conditioning is off. 
    the radiation inside my house is correct.
    the temperature in my house is correct.
    humidity within my house is correct. 
    
query one is:
    my family is comfortable in which enclosure. 
    
query two is: 
    high energy efficiency is achieved within which enclosure. 
    
query three is:
    my family is comfortable in which enclosure
    and high energy efficiency is achieved within the enclosure.
").

/** <examples>
?- answer("query one with scenario test").
?- answer("query two with scenario test").
?- answer("query three with scenario test").
?- show prolog. 
?- answer(one, with(test), le(E), R).
?- answer(two, with(test), le(E), R). 
?- answer(three, with(test), le(E), R).
*/
