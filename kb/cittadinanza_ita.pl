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

una persona A è genitore di una persona B
se A è madre di B
o A è padre di B.

una persona A ha la cittadinanza italiana
se una persona B è genitore di A
e B ha la cittadinanza italiana.

una persona A ha la cittadinanza italiana
se A è nato in italia
e il padre di A è sconosciuto
e la madre di A è sconosciuta.

una persona A ha la cittadinanza italiana
se A è nato in italia
e non risulta che 
una persona B è genitore di A.

una persona A ha la cittadinanza italiana
se A è nato in italia
e per ogni caso in cui
    una persona B è genitore di A
    è provato che
        B è apolide.

una persona A ha la cittadinanza italiana
se A è nato in italia
e per ogni caso in cui
    una persona B è genitore di A
    è provato che
        A non segue la cittadinanza di B.

scenario giuseppe è:
felice è padre di giuseppe.
tatiana è madre di giuseppe.
felice ha la cittadinanza italiana.
tatiana ha la cittadinanza italiana.

scenario filippo è:
filippo è nato in italia.

domanda uno è:
quale persona ha la cittadinanza italiana.

").

/** <examples>
?- answer(uno, with(giuseppe), le(E), R).
?- risposta uno con giuseppe.
*/