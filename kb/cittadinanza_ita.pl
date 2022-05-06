:- module('cittadinanza_ita+http://tests.com',[]).

it("il linguaggio destinazione è: scasp.

i modelli sono:
*una persona* ha la cittadinanza italiana,
*una persona* è padre di *una persona*,
*una persona* è madre di *una persona*,
*una persona* è nato in italia,
*una persona* è apolide,
il padre di *una persona* è sconosciuto,
la madre di *una persona* è sconosciuta,
*una persona* segue la cittadinanza di *una persona*.

la base di conoscenza cittadinanza_ita include:
una persona A ha la cittadinanza italiana
se una persona B è padre di la persona A
e la persona B ha la cittadinanza italiana.

scenario giuseppe è:
felice è padre di giuseppe.
tatiana è madre di giuseppe.
felice ha la cittadinanza italiana.
tatiana ha la cittadinanza italiana.

domanda uno è:
quale persona ha la cittadinanza italiana.

").

/** <examples>
?- answer(uno, with(giuseppe), le(E), R).
?- risposta uno con giuseppe.
*/