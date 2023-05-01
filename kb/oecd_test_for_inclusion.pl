:- module('oecd_test_for_inclusion',[]).

en("the target language is: prolog.

the templates are:
*an income* of *a person* is taxable in *a place* under *a rule*,
*a person* receives *a thing* for *a reason*,
*a thing* of *a person* is exercised in *a place*,
*a person* is resident in *a place*,
*a person* is resident in *a place* under *a rule*,
*a person* is an employee of *a person*,
*a person* is in *a state* for more than *a time* days,
exception to *a rule* applies to *a person* for *a thing*,
*a thing* operates in *a place*,
*a state* and *a state* are contracting states,
convention applies to *a state* and *a state*,
*a person* is liable to tax in *a state*,
*a person* is liable to tax in *a state* under *a law* of *a place*,
*a thing* is located in *a place*,
*a thing* is contained in *a place*,
*a thing* is of type *a thing* in *a state*,
*a thing* is of type *a thing*,
*a thing* is of type *a thing* under *a law* of *a state*,
*a person* has *a person* in *a thing*,
*a thing* is similar to *a thing*.
    
the knowledge base oecd_test_for_inclusion includes these files:
	italian_citizenship_to_include.

the knowledge base oecd_test_for_inclusion includes:
a thing is of type immovable property in a state
if the thing is located in the state
and the thing is of type immovable property under a law of the state.

a thing is of type immovable property in a state
if the thing is in [accessory, livestock, equipment]
and the thing is located in a property
and the property is of type immovable property in the state.

convention applies to a state A and a state B
if the state A and the state B are contracting states
    or the state B and the state A are contracting states
and the state A is different from the state B.
    
a property of a person is taxable in a state S under Article6_1
if the person receives income for the property
and the person is resident in a state R
and the property is of type immovable property in the state S
and convention applies to the state R and the state S.

% Art 7
a p of an enterprise is taxable in a state R under Article7_1
if the enterprise is resident in the state R
and the enterprise receives the p for a business
and the p is of type businesss profit
and convention applies to the state R and a state S
and it is not the case that
    exception to Article7_1 applies to the enterprise for the p.
    
% Art 15
a wage of an employee is taxable in a state R under Article15_1_1
if the wage is of type wage
and convention applies to the state R and a state S
and the employee receives the wage for an employment
and the employee is resident in the state R
and it is not the case that
    exception to Article15_1_1 applies to the employee for the wage.

a wage of an employee is taxable in a state S under Article15_1_2
if the wage is of type wage
and the employee receives the wage for an employment
and the employment of the employee is exercised in the state S
and the employee is resident in the state R
and convention applies to a state R and the state S
and it is not the case that
    exception to Article15_1_2 applies to the employee for the wage.

exception to Article15_1_1 applies to an employee for a wage
if the wage of the employee is taxable in a state under Article15_1_2
	or the wage of the employee is taxable in the state under Article15_2
	or the wage of the employee is taxable in the state under Article15_3.

exception to Article15_1_2 applies to an employee for a wage
if the wage of the employee is taxable in the state under Article15_2
or the wage of the employee is taxable in the state under Article15_3.

a wage of an employee is taxable in a state R under Article15_2
if convention applies to the state R and a state S
and the employee is resident in the state R
and the employee is an employee of an employer
and the employee receives the wage for an employment
and the employee is different from the employer
and the employment of the employee is exercised in an other state S
and the state R is different from the other state S
and it is not the case that
    the employee is in the other state S for more than 183 days
and it is not the case that
    the employer is resident in the other state
    or the employer is resident in the state R
    or permanent establishment of the employer is exercised in the other state S.

a wage of an employee is taxable in a state R under Article15_3
if convention applies to the state R and a state S
and the employee is resident in the state R
and the employee receives the wage for an employment
and the employment of the employee is exercised in a ship
and the ship operates in international traffic
and the ship operates in an other state S
and the ship operates in an other state C
and the other state S is different from the other state C.
    
a p A of a permanent establishment is taxable in a state S under Article7_2
if an enterprise A is resident in a state R
and the enterprise A has the permanent establishment in the state S
and the permanent establishment receives the p A for a business A
and the p A is of type business profit
and an enterprise B receives a p B for an business B
and the enterprise B is resident in the state S
and the p A is similar to the p B
and the business A is similar to the business B
and convention applies to the state R and the state S.
    
exception to Article7_1 applies to an enterprise for a p
if the enterprise has a permanent establishment in a state
and the p of the permanent establishment is taxable in the state under Article7_2.

scenario one is:
piera receives wage1 for employment1.
wage1 is of type wage.
employment1 of piera is exercised in italy.
piera is in portugal for more than 183 days.
italy and portugal are contracting states.

scenario two is:
piera receives wage1 for employment1.
wage1 is of type wage.
employment1 of piera is exercised in portugal.
piera is an employee of alessia.
italy and portugal are contracting states.
piera is resident in italy.
the castle is of type immovable property.
the castle is located in france.
piera receives income for the castle.
italy and france are contracting states.

scenario three is:
piera is resident in italy.
piera receives the wage for employment2.
employment2 of piera is exercised in ship1.
ship1 operates in international traffic.
ship1 operates in germany.
ship1 operates in france.
italy and france are contracting states.
    
scenario four is:
piera is resident in italy.
the castle is of type immovable property.
the castle is located in france.
piera receives income for the castle.
italy and france are contracting states.
    
scenario five is:
piera is resident in italy.
piera receives wage1 for employment1.
wage1 is of type wage.
employment1 of piera is exercised in portugal.
the castle is located in france.
piera receives income for livestock.
alessia receives income for the castle.
alessia is resident in germany.
germany and france are contracting states.
italy and portugal are contracting states.
italy and france are contracting states.
livestock is located in the castle.
    
scenario six is:
cirsfid is resident in italy.
cirsfid has a CPE in france.
CPE receives p2 for claudine.
p2 is of type business profit.
sorbona receives p3 for georgette.
sorbona is resident in france.
p2 is similar to p3.
claudine is similar to georgette.
italy and france are contracting states.
    
query two is:
which person is resident in which state.

query one is:
which thing of which person is taxable in which state under which article.
").

/** <examples>
?- answer one with one.
?- answer one with six.
?- aggregate_all(count, (answer one with six), Count).
?- answer(one, with(six), le(E), R).
*/