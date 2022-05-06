:- module('family_example',[]).

en("the target language is: prolog.

the templates are:
    *a person* is a parent of *an other person*,
    *a person* is a child of *an other person*,
    *a person* is male,
    *a person* is of opposite sex from *an other person*,
    *a person* is female,
    *a person* is a grandfather of *an other person*,
    *a person* is a father of *an other person*. 

the knowledge base family example includes:

a person Y is a parent of a person X
    if X is a child of Y.

a person Y is a father of a person X
    if X is a child of Y 
    and Y is male.

a person X is of opposite sex from a person Y
    if X is male 
    and Y is female.

a person Y is of opposite sex from a person X
    if X is male 
    and Y is female.

a person X is a grandfather of a person Z
    if X is a father of a person Y
    and Y is a parent of Z.

scenario one is:
john is a child of sue. 
john is a child of sam.
jane is a child of sue. 
jane is a child of sam.
sue is a child of george. 
sue is a child of gina.
john is male. 
jane is female. 
june is female.
sam is male. 
sue is female. 
george is male.

query one is:
    jane is a child of sue. 

query two is:
    gina is female.

query three is:
    george is a father of sue. 

query four is:
    jane is of opposite sex from george. 

query five is:
    george is a grandfather of john.

query six is:
    which person is a grandfather of john. 
").

/** <examples>
?- answer one with one.
?- answer two with one.
?- answer three with one.
?- answer four with one.
?- answer five with one.
?- answer six with one.
?- show prolog.
*/