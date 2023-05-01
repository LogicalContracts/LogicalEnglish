:- module('right_translation_basic',[]).

en("the target language is: scasp. % other languages available soon
% Logical English document
% above example of a prolog module name. Please adjust as convenient.  

the templates are:
*a person* has *a right* according to *an article*,
*a document* needed by *a person* was recognised as an essential document,
*a person* understands *a language*,
*a document* needed by *a person* is of type *a type*,
the authority has decided that *a thing*,
the language of the proceedings of *a person* is *a language*,
*a person* understands *a language*.

the knowledge base right_translation_basic includes:

it is unknown whether a document needed by jacinto is of type charge.

a person has translation authorization according to Article 3_1
	if the language of the proceedings of the person is a language
    and a document needed by the person was recognised as an essential document
    and it is not the case that
    	the person understands the language.
    
a document needed by a person was recognised as an essential document
    if the document needed by the person is of type document depriving liberty.
    
a document needed by a person was recognised as an essential document
    if the document needed by the person is of type charge.
    
a document needed by a person was recognised as an essential document
    if the document needed by the person is of type indictment.
    
a document needed by a person was recognised as an essential document
    if the document needed by the person is of type judgement.
    
a document needed by a person was recognised as an essential document
    if the authority has decided that
    	the document needed by the person is of type essential document.
    
scenario one is:
it is unknown whether a document needed by mario is of type charge.
the language of the proceedings of mario is  dutch.
D1 needed by mario is of type charge.
the language of the proceedings of jacinto is spanish. 

query one is:
which person has which right according to which article.

").

/** <examples>
?- answer("query one with scenario one").
?- answer("query three with scenario one").
?- answer("query two with scenario one").
?- answer(one, with(one), le(E), R). 
*/
