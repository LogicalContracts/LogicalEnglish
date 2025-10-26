:- module(journal,[]).

en("the target language is: prolog. 
the templates are:

*a journal entry* has as ID *an ID*.
*a journal entry* has as date *a date*.
*a journal entry* has as description *a description*.

*a journal head* is part of *a journal entry*.
*a journal head* is a debit of *an amount* to *an account*.
*a journal head* is a credit of *an amount* to *an account*.

*a journal head* has *an amount*.
*a journal entry* is valid.
*a journal entry* is balanced.
*a journal entry* has at least 2 heads.

the ontology is:

cash_at_bank is an account.
sales_revenue is an account.
gst_payable is an account.

the knowledge base payg includes:

% A journal entry is valid if it is dated, has at least 2 heads, and is balanced.
a journal entry J is valid
    if the journal entry J has as date a date D
    and the journal entry J has at least 2 heads
    and the journal entry J is balanced.

% Rule to check for at least 2 heads (as per constraint)
a J has at least 2 heads
    if a number N is the sum of each I such that
    	I = 1
        and an H is part of J
    and N >= 2.

% Rule to check if the journal entry sums to zero (as per constraint)
a J is balanced
    if a number S is the sum of each signed amount SA such that
        a H is part of J
        and H has the signed amount SA
    and S = 0.

% Rule to get the signed value for a debit (positive)
a H has an amount S
    if H is a debit of an amount A to an account Acc
    and S = A.

% Rule to get the signed value for a credit (negative)
a H has an amount S
    if H is a credit of an amount A to an account Acc
    and S = 0-A.

scenario user_example is:
    JE-201 is a journal entry.
    JE-201 has as ID JE-201.
    JE-201 has as date 2025-10-23.
    JE-201 has as description Journal entry from user example.
    % Dr Bank - $100
    head-1 is a journal head.
    head-1 is part of JE-201.
    head-1 is a debit of 100 to cash_at_bank.
    % Cr sales - 110
    head-2 is a journal head.
    head-2 is part of JE-201.
    head-2 is a credit of 110 to sales_revenue.
    % Dr GST Payable - 10
    head-3 is a journal head.
    head-3 is part of JE-201.
    head-3 is a debit of 10 to gst_payable.

query check_user_example is:
    JE-201 is valid.

"). 

/** <examples> To test the rules, these are possible combinations of queries and scenarios
?- show prolog.  % check translation into Prolog`
?- answer(check_user_example, with(user_example), le(E), R).  
*/