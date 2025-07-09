:- module('gstturnover',[]).

en("the target language is: prolog. 
    
the templates are:
*an entity* does not operate for profit.
*an entity* must register for GST. 
*an entity* has *a turnover*. 
*an entity* projected turnover is *a turnover*. 
*an entity* has *a turnover*. 
the turnover of *an entity* for the current month is *an amount*.    
the turnover of *an entity* for the previous 11 months is *an amount*. 
the turnover of *an entity* for the next 11 months is *an amount*.
*an amount* is the total business income. 
*an amount* is the GST included in sales to your customers.
*an amount* is sales to associates that are not for payment and are not taxable. 
*an amount* is sales not connected with an enterprise you run. 
*an amount* is input-taxed sales you make.
*an amount* is sales not connected with Australia (export sales). 
*an amount* is sales of goods or services within Australia that are sales to third parties.
*an amount* is the GST Turnover. 
    
the ontology is:
    
sporting club is an entity.
charity is an entity.
community group is an entity.
professional association is an entity.    

an entity is a not-for-profit if the entity does not operate for profit. 
        
the knowledge base gst includes:

an entity must register for GST 
    if the entity is a not-for-profit
    and		the entity has a turnover 
    		and the turnover >= 150000
    	or	the entity projected turnover is a projection 
    		and the projection >= 150000. 

an entity must register for GST 
    if it is not the case that
    	the entity is a not-for-profit
    and 	the entity has a turnover 
   			and the turnover >= 75000 
    	or the entity projected turnover is a projection
    		and the projection >= 75000.

an entity has a current turnover
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the previous 11 months is a past amount
    and the current turnover = monthly amount + past amount. 

an entity projected turnover is a projection
    if the turnover of the entity for the current month is a monthly amount
    and the turnover of the entity for the next 11 months is a future amount
    and the projection = monthly amount + future amount. 
    
a TBI is the total business income
    if a GST is the GST included in sales to your customers
	and a Sales_Associates is sales to associates that are not for payment and are not taxable 
	and a Disconnected_sales is sales not connected with an enterprise you run 
	and a Input_taxed is input-taxed sales you make
	and an Exports is sales not connected with Australia (export sales) 
	and a Sales is sales of goods or services within Australia that are sales to third parties
    and TBI = Sales Associates + Disconnected_sales + Input_taxed + Exports + Sales. 

a GST_Turnover is the GST Turnover
    if a TBI is the total business income
	and a GST is the GST included in sales to your customers
	and a Sales_Associates is sales to associates that are not for payment and are not taxable 
	and a Disconnected_sales is sales not connected with an enterprise you run 
	and a Input_taxed is input-taxed sales you make
	and an Exports is sales not connected with Australia (export sales) 
	and a Sales is sales of goods or services within Australia that are sales to third parties
    and GST_Turnover = TBI - GST - Sales_Associates - Disconnected_sales - Input_taxed - Exports. 
    
    
scenario A is:
Cherrio Charity is an entity. 
Cherrio Charity does not operate for profit. 
Cherrio Charity has 160000. 
    
query A is:
Cherrio Charity must register for GST. 
% yes
    
scenario B is: 
sporting club does not operate for profit.
sporting club projected turnover is 100000. 
        
query B is:
sporting club must register for GST. 
% no
    
scenario C is: 
Tom s Bakery is an entity.
Tom s Bakery has 80000.
    
query C is: 
Tom s Bakery must register for GST.
% yes
    
query not-for-profit is:
which entity is a not-for-profit. 
    
    
scenario typical is:
	9090.91 is the GST included in sales to your customers.
	0 is sales to associates that are not for payment and are not taxable. 
	0 is sales not connected with an enterprise you run. 
	5000 is input-taxed sales you make.
	0 is sales not connected with Australia (export sales). 
	100000 is sales of goods or services within Australia that are sales to third parties.

scenario significant exports is:
	6818.18 is the GST included in sales to your customers.
	10000 is sales to associates that are not for payment and are not taxable. 
	2000 is sales not connected with an enterprise you run. 
	0 is input-taxed sales you make.
	50000 is sales not connected with Australia (export sales). 
	75000 is sales of goods or services within Australia that are sales to third parties.

scenario small business is:
	0 is the GST included in sales to your customers.
	5000 is sales to associates that are not for payment and are not taxable. 
	1000 is sales not connected with an enterprise you run. 
	0 is input-taxed sales you make.
	0 is sales not connected with Australia (export sales). 
	0 is sales of goods or services within Australia that are sales to third parties. 
    
query tbi is:
    which amount is the total business income. 
    
query gst_turnover is: 
    which amount is the GST Turnover.  
    
").

/** <examples>
?- answer("query A with scenario A").
?- answer("query B with scenario B").
?- answer("query C with scenario C").
?- answer('C', with('C'), le(R), L). 
?- answer('B', with('B'), le(R), L). 
?- answer('A', with('A'), le(R), L). 
?- answer('not-for-profit', with('B'), le(R), L).
?- answer(tbi, with(typical), le(R), L).
?- answer(gst_turnover, with(typical), le(R), L).
?- answer(tbi, with('significant exports'), le(R), L).
?- answer(gst_turnover, with('significant exports'), le(R), L).
?- answer(tbi, with('small business'), le(R), L).
?- answer(gst_turnover, with('small business'), le(R), L).
*/
