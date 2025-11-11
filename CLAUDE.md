# The "Logical English" (LE) language
Logical English (from now on referred as "LE") is a small subset of English, and a syntactic sugar for pure PROLOG. A LE program represents a piece of regulatory text - legislation,  contract, legal opinion - so that it can be queried for logical consequences.
Its syntax is described in @le_syntax.md
For each LE program, represented in a file with extension ".le", there should also exist a file with the same name and extension ".le.tests", containing the expected answers for all queries and scenarios, as PROLOG facts expected(QueryName,ScenarionName,ScenarioSentences).
## Examples
Examples of LE programs and their expected answers can be fond in @moreExamples/

# How to build a LE program
* Analyse the given regulatory text to understand its purpose, the predicates (templates) it defines, and what rules define the truth of those predicates. Also extract main types of arguments for those predicates, so that a small ontology of types can be built if needed.
* Extract any examples from the text, if any, illustrating application of the regulatory text to concrete use cases; these should map to LE scenarios (sets of predicate fact sentences) plus queries for the main predicates (sentences asking an interesting  question to the regulatory text), and also the expected answers. 
* If the given text contains no examples, summarise the text in a short sentence S, search the web with "examples for S", and collect examples from the top page
* draft the LE program with the templates, ontology if any, rules, scenarios and queries; draft also the .le.tests file with expected answers for all queries and scenarios; for all queries, there should be at least one expected answer in some scenario
* Make sure rules use variables, because concrete objects/entities should be provided via scenarios instead; also, comparisons among numbers or dates need to be written with PROLOG operators, instead of comparative adjectives
* Test and debug it
# How to test a LE program
* Execute the LE verify command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=verify <PROGRAM>
If the program is working as expected, this command will output "ALL GOOD :-)" at the end; otherwise there will be errors and/or warning messages
# How to debug a LE program
React to the errors and warnings produced by the LE verify command. First edit the program as follows, then test it again:
## Missing template for 'sentence'
Generate a template for the sentence and add it to the program 
## Rule without variables
Rules should not refer concrete data, which should be in scenarios; predicates in rules are mostly to refer to variables.
## Predicate is not tested by any query
Ask the user to provide a query for the predicate, as well as expected answers for all scenarios
## Undefined predicate
Ask the user whether the undefined predicate should be obtained from the given regulatory text, or whether we should consult another web source
## Missing expected result for query <Q> with scenario <S>
Ask the user
## Test failure in scenario <S> for query <Q>
This includes a second line with 
    expected ExpectedAnswers got ComputedAnswers
Be creative and edit the program to fix this.
# How to run an arbitrary query and scenario on a program
* Execute the LE query command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=query --query="<QUERY_SENTENCE>" --scenario="<SCENARIO_SENTENCES>" myProgram.le