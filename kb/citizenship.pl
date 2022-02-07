:- module('citizenship_trust+https://www.legislation.gov.uk/ukpga/1981/61/section/1',[]).

en("the target language is: prolog.

the templates are:
*a person* acquires British citizenship on *a date*.
*a person* is born in *a place* on *a date*,
*a date* is after commencement,
*a person* is the mother of *a person*,
*a person* is the father of *a person*,
*a person* is a British citizen on *a date*,
*a person* is settled in the UK on *a date*,
*a person* says that *a sentence*,
*a person* is qualified to determine fatherhood.

the knowledge base citizenship includes:
    
a person acquires British citizenship on a date
if the person is born in the UK on the date
and the date is after commencement
and an other person is the mother of the person
    or the other person is the father of the person
and the other person is a British citizen on the date
    or the other person is settled in the UK on the date.
    
a person is the father of an other person
    if a third person says 
    that the person is the father of the other person
    and the third person is qualified to determine fatherhood.
    
a person X is the father of a person Y
    if X says that X is the father of Y.  
    
scenario alice is:
    John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Alice is the mother of John.
	Alice is a British citizen on 2021-10-09.
    
scenario harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Harry is the father of John.
	Harry is settled in the UK on 2021-10-09.

scenario trust_harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Harry says that Harry is the father of John.
	Harry is settled in the UK on 2021-10-09.
    
scenario alice_harry is:
	John is born in the UK on 2021-10-09.
	2021-10-09 is after commencement.
	Alice is the mother of John.
	Alice says that Harry is the father of John.
    Alice is qualified to determine fatherhood.
	Harry is settled in the UK on 2021-10-09.
      
query one is:
    which person acquires British citizenship on which date.
").

/** <examples>
?- answer one with trust_harry.
?- answer(one,with(trust_harry), le(Explanations), R).
?- answer one with alice.
?- answer(one,with(alice), le(Explanations), R).
*/
