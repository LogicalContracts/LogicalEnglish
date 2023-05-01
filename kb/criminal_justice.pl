:- module('criminal_justice+http://tests.com',[]).

en("the target language is: prolog.

the templates are:
*a person* has *a right* in *a proceeding*,
*a person* has *a right* in *a proceeding* under *an article*,
*a person* has *a right* with *a thing* in *a proceeding* under *an article* of *a law*,
*a person* is *a status* in *a proceeding*,
*a person* has been made aware that *a fact*,
*a proceeding* has *a status*,
*a proceeding* is ongoing,
*a proceeding* is in the language *a language*,
*a person* is involved in *a proceeding*,
*a person* speaks *a language*,
*a person* understands *a language*,
*a person* is represented by *a lawyer*,
*an exception* is an exception to *an article*,
*a person* knows *a language*,
*a person* is in danger of *a thing*,
exception to *an article* applies to *a person* under *an article* of *a law*.

the knowledge base criminal_justice includes:
a person has the right to interpretation with the trial in a proceeding under article 2(1) of Directive 2010(64)
if the person is criminal subject in the proceeding
and the proceeding is in the language a language
and it is not the case that
    the person knows the language.
    
a person has the right to interpretation with a lawyer in a proceeding under article 2(2) of Directive 2010(64)
if the person is criminal subject in the proceeding
and the person is represented by the lawyer
and the lawyer knows a language
and it is not the case that
    the person knows the language.
    
a person has the right to access materials with the trial in a proceeding under article 7(3) of Directive 2012(13)
if the person is criminal subject in the proceeding
and the person has document New Evidence
and it is not the case that
	exception to article 7(3) applies to the person under an other article of a law.
    
exception to article 7(3) applies to a person under article 7(4) of Directive 2012(13)
if the person is in danger of a thing
and the thing is in [life, fundamental rights, public interest].
    
a person has the right to interpretation with a lawyer in a proceeding under article 10 of Italian law
if the person is criminal subject in the proceeding
and the person is represented by the lawyer
and the lawyer knows a language
and it is not the case that
    the person knows the language.

a person is criminal subject in a proceeding
if the person is involved in the proceeding
and the proceeding is of type criminal
    or the proceeding is of type europeanArrestWarrant
and the person has been made aware 
    that the person is involved in the proceeding 
and the proceeding is ongoing.

a person is involved in a proceeding
if the person is accused in the proceeding
	or the person is suspect in the proceeding.

a proceeding is ongoing
if the proceeding has started
and it is not the case that
    the proceeding has concluded.
    
a person knows a language
if the person speaks the language
and the person understands the language.

scenario one is:
galileo is suspect in proc001.
galileo has been made aware that galileo is involved in proc001.
galileo understands german.
galileo is represented by francesco.
francesco speaks german.
francesco understands german.
proc001 is of type criminal.
proc001 has started.
proc001 is in german.
galileo has document New Evidence.
    
scenario two is:
galileo is in danger of life.
    
query two is:
exception to which article applies to which person under which other article of which law.

query one is:
which person has which right with which thing in which proceeding under which article of which law.

").