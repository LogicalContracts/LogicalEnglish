# The "Logical English" (LE) language
Logical English (from now on referred as "LE") is a small subset of English, and a syntactic sugar for pure PROLOG. A LE program represents a piece of regulatory text - legislation,  contract, legal opinion - so that it can be queried for logical consequences for particular data scenarios.
Its syntax is described in @le_syntax.md. Some syntax additions:
* do not use double quotes (") anywhere in the program unless escaped
* all templates and rules must have at least one variable
* arithmetic expressions are written Result = Expression, for example Diff = A-B
For each LE program, represented in a file with extension ".le", there should also exist a file with the same name and extension ".le.tests", containing:
* the expected answers for all queries and scenarios in the program, as PROLOG facts expected(QueryName,ScenarionName,Answers) ; Answers is a list of strings, each a sentence for a (bound) predicate
* least two expected facts for each top level predicate: one with no answers, the other with at least one answer
## LE Examples
Examples of LE programs and their expected answers can be found in @moreExamples/
# How to run an arbitrary query and scenario on a program
* Execute the LE query command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=query --query="<QUERY_SENTENCE>" --scenario="<SCENARIO_SENTENCES>" myProgram.le
# How to find the top level predicate templates
* Execute command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=top_predicates  myProgram.le
# How to test a LE program
* Execute the LE verify command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=verify <PROGRAM>
If the program is working as expected, this command will output "ALL GOOD :-)" at the end; otherwise there will be errors and/or warning messages, meaning that the program is not yet correct and needs further editing
# How to debug a LE program
React to the errors and warnings produced by the LE verify command. First edit the program as follows, then test it again:
## Missing template for 'sentence'
Generate a template for the sentence and add it to the program 
## Rule without variables
Rules should not refer concrete data, which should be in scenarios; predicates in rules are mostly to refer to variables.
## Missing rules
A LE program must have more than just facts, it needs rules.
## Predicate is not tested by any query
Ask the user to provide a query for the predicate, as well as expected answers for all scenarios
## Undefined predicate
The predicate must either have a rule defining it, or there must be a scenario with a fact sentence for the predicate. Try to obtain this from the given regulatory text, or perform a web search
## Missing expected result for query <Q> with scenario <S>
Ask the user
## Test failure in scenario <S> for query <Q>
This includes a second line with 
    expected ExpectedAnswers got ComputedAnswers
Be creative and edit the program to fix this.
## time_limit_exceeded
Look for uncontrolled recursions in the program rules and fix them
# How to convert regulatory text to a LE program
Perform 3 steps in sequence: 
* Analyze the given regulatory text
* Write the LE program
* Test and Debug it until correct.
## Analyze the given regulatory text
Focusing on the given text only:
* Analyse the text to understand the predicates (templates for true or false sentences) that it defines
* Extract the main types of arguments of those predicates, so that a small ontology of types can be built if needed.
* Extract rules in the text that define the truth of those predicates. 
* Extract examples from the text, if any present, that show how the regulatory text applies to concrete scenarios: data in the scenario, a query and the expected answers
* If the given text contains no examples, summarise the text in a short sentence S, search the web with "examples for S", and collect a few examples from the top page
Finally, you MUST summarise your findings in a new .txt file, prior to writing the LE program.
## Write the LE program
Before each LE element, put a PROLOG comment with its provenance within the given text, or web URL if the element originated in a web search.
* Write the templates based on the predicates found 
* Write the ontology based on the types
* Write the rules, defining and using the predicates
  * Make sure rules use variables, because concrete objects/entities should be provided via scenarios instead
  * Comparisons among numbers or dates need to be written with PROLOG operators, instead of comparative adjectives
* Write scenarios (sets of predicate fact sentences) and queries (useful questions), based on the examples
* Write the .le.tests file with expected answers for all queries 
  * for all queries, there should be at least one expected (non empty) answer in some scenario
  * expected answers are lists of strings, each a bound template sentence (result) for the query
## Test and Debug until correct
* Test and debug and edit it repeatedly as needed, until:
** all expected answers in the .le.tests file are obtained correctly 
** there are no warning messages
* DO NOT conclude "Test and Debug"" without all tests running as expected!
* ALL warnings and errors MUST be fixed. ALL tests MUST succeed.
* Double-check
  * again, the final LE program MUST have neither warnings nor errors, and its tests MUST all succeed
