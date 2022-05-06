:- module('bigger',[]).

en("the target language is: prolog.

the templates are:
    *an animal* is bigger than *an other animal*,
    *an animal* is just bigger than *an other animal*,

the knowledge base bigger includes:

an animal X is bigger than an animal Y 
    if X is just bigger than Y.

an animal X is bigger than an animal Y 
    if X is just bigger than an animal Z
    and Z is bigger than Y. 
 
scenario one is:
	elephant is just bigger than horse.
    horse is just bigger than donkey.
      
query one is:
 	which animal X is bigger than which animal Y. 
   
").

/** <examples>
?- answer one with one.
?- show prolog.
*/