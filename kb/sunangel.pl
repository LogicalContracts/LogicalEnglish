:- module('sunangel',[]).

en("the target language is: prolog.

the templates are:
    *a bird* has beautifully colorful feathers.
    *a bird* is probably a male.
    *a bird* is more attractive to the other sex. 
    *a bird* has an advantage for reproduction.
    *a bird* attracts predators.
    *a bird* puts in jeopardy its descendants.
    *a bird* is in charge of guarding the nest.
    *a strategy* contributes to the survival of *a species*. 
    there are *a first partner* and *a second partner* for the reproduction of *a species*. 

the knowledge base sunangel includes:

the hummingbird in the picture has beautifully colorful feathers.

a hummingbird is probably a male if
    the hummingbird has beautifully colorful feathers.

a bird is more attractive to the other sex if 
    the bird has beautifully colorful feathers.

a bird has an advantage for reproduction if 
    the bird is more attractive to the other sex.

%it is not true that
%    a bird is a hummingbird
%    and the bird is a male
%    and it is not the case that the bird has beautifully colorful feathers.

%it is not true that
%    a bird is a hummingbird
%    and the bird is a female
%    and the bird has beautifully colorful feathers.

a bird attracts predators if 
    the bird has beautifully colorful feathers.

a bird puts in jeopardy its descendants if
    the bird attracts predators
    and the bird is in charge of guarding the nest.

a bird is in charge of guarding the nest if
    the bird is a hummingbird
    and the bird is a female.

sexual dimorphism contributes to the survival of a species if
    there are a first partner and a second partner for the reproduction of the species
    and the first partner has an advantage for reproduction
    and it is not the case that
         the second partner puts in jeopardy its descendants.

scenario one is:
    the hummingbird in the picture is a bird.
    the hummingbird in the picture is a hummingbird.   
    the hummingbird in the picture is a male. 
    the hummingbird in the picture has beautifully colorful feathers.
    there are the hummingbird in the picture and some female for the reproduction of hummingbirds.    

query one is:
 	which bird is probably a male.

query two is:
 	which strategy contributes to the survival of which species.
     
").

/** <examples>
?- answer one with one.
?- show prolog.
?- answer(one, with(one), le(E), R). 
?- answer(two, with(one), le(E), R). 
*/