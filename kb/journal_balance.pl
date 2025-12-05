:- module(journal_balance,[]).

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
    
the total debits for *an account* is *a number*. 
the total credits for *an account* is *a number*. 

a trial balance is defined. 
*an account* has a net balance of *an amount*. 

the ontology is:

cash at bank is an account.
sales revenue is an account.
gst payable is an account.

the knowledge base trial balance includes:

% A journal entry is valid if it is dated, has at least 2 heads, and is balanced.
a journal entry J is valid
    if the journal entry J has as date a date D
    and the journal entry J has at least 2 heads
    and the journal entry J is balanced.

% Rule to check for at least 2 heads (as per constraint)
an entry has at least 2 heads
    if a number N is the sum of each I such that
    	I = 1
        and an H is part of the entry
    and N >= 2.

% Rule to check if the journal entry sums to zero (as per constraint)
an entry is balanced
    if a number S is the sum of each signed amount SA such that
        a H is part of the entry
        and H has the signed amount SA
    and S = 0.

% Rule to get the signed value for a debit (positive)
a header has an amount S
    if the header is a debit of an amount A to an account Acc
    and S = A.

% Rule to get the signed value for a credit (negative)
a header has an amount S
    if the header is a credit of an amount A to an account Acc
    and S = 0-A.

% == Trial Balance Rules ==
% 1. Calculate the total debits for a specific account.
the total debits for an account A is a number N
    if A is an account
    and N is the sum of each amount such that
        a H is a journal head
        and an JE is a journal entry
        and H is part of JE
        and H is a debit of the amount to A.

% 2. Calculate the total credits for a specific account.
the total credits for an account A is a number N
    if A is an account
    and N is the sum of each amount such that
        a H is a journal head
        and an JE is a journal entry
        and H is part of JE
        and H is a credit of the amount to A.

% 3. Calculate the net balance for a specific account.
an account A has a net balance of an amount N
    if the total debits for A is a number D
    and the total credits for A is a number C
    and N = D - C.

% 4. A trial balance is defined if the sum of all account balances equals zero.
a trial balance is defined
    if for all cases in which
            an entry is a journal entry
        it is the case that
            the entry is valid  % the following seems redundant
    and a number S is the sum of each balance such that
        an Acc is an account % Sum over all defined accounts. 
        and Acc has a net balance of the balance
    and S = 0.

scenario user_example is:
    JE-201 is a journal entry.
    JE-201 has as ID JE-201.
    JE-201 has as date 2025-10-23.
    JE-201 has as description Journal entry from user example.
    % Dr Bank - $100
    head-1 is a journal head.
    head-1 is part of JE-201.
    head-1 is a debit of 100 to cash at bank.
    % Cr sales - 110
    head-2 is a journal head.
    head-2 is part of JE-201.
    head-2 is a credit of 110 to sales revenue.
    % Dr GST Payable - 10
    head-3 is a journal head.
    head-3 is part of JE-201.
    head-3 is a debit of 10 to gst payable.

scenario balanced_tb is:
    % --- JE-100: Sale on account for $100 ---
    JE-100 is a journal entry.
    JE-100 has as date 2025-11-01.
    JE-100 has as description Cash sale.    
    % Dr Cash $100
    head-100-1 is a journal head.
    head-100-1 is part of JE-100.
    head-100-1 is a debit of 100 to cash at bank.
    % Cr Sales Revenue $100
    head-100-2 is a journal head.
    head-100-2 is part of JE-100.
    head-100-2 is a credit of 100 to sales revenue.
    % --- JE-101: GST Payment of $10 ---
    JE-101 is a journal entry.
    JE-101 has as date 2025-11-05.
    JE-101 has as description GST payment.
    % Dr GST Payable $10 (to reduce the liability)
    head-101-1 is a journal head.
    head-101-1 is part of JE-101.
    head-101-1 is a debit of 10 to gst payable.
    % Cr Cash $10
    head-101-2 is a journal head.
    head-101-2 is part of JE-101.
    head-101-2 is a credit of 10 to cash at bank.

scenario unbalanced_tb is:
    % --- Includes all entries from balanced_tb (JE-102) ---
    % --- JE-102: Error - Single-entry Debit $50 ---
    JE-102 is a journal entry.
    JE-102 has as date 2025-11-10.
    JE-102 has as description Error Cash Deposit recorded Credit side forgotten.
    % Dr Cash $50 (but no corresponding credit was recorded)
    head-102-1 is a journal head.
    head-102-1 is part of JE-102.
    head-102-1 is a debit of 50 to cash at bank.
    % Note: JE-102 itself is *invalid* as it has only one head, but its *effect* 
    % on the TB is an uncorrected balance error.

scenario unbalanced_tb_2 is:
    % --- Includes all entries from balanced_tb (JE-103) ---
    % --- JE-102: Error - Single-entry Debit $50 ---
    JE-103 is a journal entry.
    JE-103 has as date 2025-11-10.
    JE-103 has as description Error Cash Deposit recorded incorrect Credit.  
    % Dr Cash $50 (but no corresponding credit was recorded)
    head-103-1 is a journal head.
    head-103-1 is part of JE-103.
    head-103-1 is a debit of 50 to cash at bank.
    % Cr Sales $10 (incorrect credit recorded)
    head-103-2 is a journal head.
    head-103-2 is part of JE-103.
    head-103-2 is a credit of 10 to gst payable.
    % Cr Sales $50 (incorrect credit recorded)
    head-103-3 is a journal head.
    head-103-3 is part of JE-103.
    head-103-3 is a credit of 50 to sales revenue.
    % the TB is an uncorrected balance error.

query trial_balance is:
    a trial balance is defined.

query check_user_example is:
    JE-201 is valid.

"). 

/** <examples> To test the rules, these are possible combinations of queries and scenarios
?- show prolog.  % check translation into Prolog`
?- answer(check_user_example, with(user_example), le(E), R).
?- answer(check_user_example, with(user_example)). 
?- answer(trial_balance, with(balanced_tb), le(E), R).  
?- answer(trial_balance, with(balanced_tb)). 
?- answer(trial_balance, with(unbalanced_tb), le(E), R). 
?- answer(trial_balance, with(unbalanced_tb)). 
?- answer(trial_balance, with(unbalanced_tb_2), le(E), R). . 
?- answer(trial_balance, with(unbalanced_tb_2)). 
*/